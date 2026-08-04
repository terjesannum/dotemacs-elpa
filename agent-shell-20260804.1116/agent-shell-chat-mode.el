;;; agent-shell-chat-mode.el --- Chat-style labels for agent-shell. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `agent-shell-chat-mode' relabels the shell so it reads like a chat:
;; each submitted user turn is boxed `Me' and each response is boxed with
;; the agent's name.  Labels are overlays, so the buffer text is untouched
;; (a `display' overlay replaces the visible comint prompt for `Me', and a
;; `before-string' overlay renders the agent label at the invisible
;; `<shell-maker-end-of-prompt>' marker).
;;
;; The live prompt awaiting input shows `Me' too, so you can type straight
;; into the shell.  When `agent-shell-prompt-bar-mode' is enabled, input
;; flows through that bar instead, so the live prompt is hidden.
;;
;; Toggle it with `M-x agent-shell-chat-mode'.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'seq)
(eval-when-compile (require 'subr-x))

(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")

(defvar agent-shell--state)
;; Soft reference: `agent-shell-prompt-bar-mode' may be unbound when the
;; prompt bar is not loaded.  Read it with `bound-and-true-p'.
(defvar agent-shell-prompt-bar-mode)

;; Forward-declared: used before the `define-minor-mode' at the end.
(defvar agent-shell-chat-mode)

;;; Constants

(defconst agent-shell-chat--prompt "❯ "
  "Prompt marker shown on the live shell prompt while it awaits input.
Cleared the instant the prompt is submitted, since the overlay then
renders the submitted turn instead.")

(defconst agent-shell-chat--body-indent "  "
  "Indent that lines the prompt input up with the response body.
Mirrors the two-column base `line-prefix' the response carries (see
`agent-shell-ui--indent-text'); hiding the comint prompt would otherwise
drop the input flush to column 0.")

;;; Faces

(defface agent-shell-chat-me-label
  '((t :inherit (bold font-lock-keyword-face) :inverse-video t :box t))
  "Face for the user (\"Me\") chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

(defface agent-shell-chat-agent-label
  '((t :inherit (bold font-lock-function-name-face) :inverse-video t :box t))
  "Face for the agent chat label.
`:inverse-video' fills the badge with the foreground color (text inverts
to the background); `:box' t adds a border in the foreground color."
  :group 'agent-shell)

;;; State

(defvar-local agent-shell-chat--labeled nil
  "Non-nil once chat labels have been applied to this shell buffer.")

(defvar-local agent-shell-chat--subscription nil
  "Event subscription token keeping chat labels in sync, or nil.")

(defvar-local agent-shell-chat--relabel-timer nil
  "Pending coalesced relabel timer for this buffer, or nil.")

;;; Labels

(defun agent-shell-chat--label (text face)
  "Return TEXT padded and propertized with FACE, as a chat label.

FACE carries the box (see `agent-shell-chat-me-label').

For example, (agent-shell-chat--label \"Me\" \\='agent-shell-chat-me-label)
returns \" Me \" in that face."
  (propertize (format " %s " text) 'face face))

(defun agent-shell-chat--agent-name ()
  "Return the attached agent's display name for the response label.

For example, with a mode-line name of \"Claude\" returns \"Claude\";
with none available, returns \"Agent\"."
  (or (map-nested-elt agent-shell--state '(:agent-config :mode-line-name))
      "Agent"))

(defun agent-shell-chat--prompt-face-p (value)
  "Return non-nil when a `font-lock-face' VALUE marks a shell prompt.
A live prompt carries `comint-highlight-prompt'; a restored or echoed
prompt carries `agent-shell-prompt' (which inherits it).  Either may be
repeated.

For example, \\='comint-highlight-prompt, \\='agent-shell-prompt, and
\\='(comint-highlight-prompt comint-highlight-prompt) all return non-nil,
while \\='default returns nil."
  (let ((faces (if (listp value) value (list value))))
    (or (memq 'comint-highlight-prompt faces)
        (memq 'agent-shell-prompt faces))))

(defun agent-shell-chat--overlay-in (beg end category)
  "Return an existing label overlay of CATEGORY between BEG and END, or nil."
  (seq-find (lambda (overlay) (eq (overlay-get overlay 'category) category))
            (overlays-in beg (max end (1+ beg)))))

(defun agent-shell-chat--upsert-overlay (category anchor-beg anchor-end beg end props)
  "Ensure a CATEGORY overlay spans BEG..END carrying PROPS.

PROPS is an alist of overlay property to value.  Reuses an existing
CATEGORY overlay overlapping ANCHOR-BEG..ANCHOR-END (moving it when the
span changed), otherwise creates one."
  (let ((overlay (or (agent-shell-chat--overlay-in anchor-beg anchor-end category)
                     (let ((created (make-overlay beg end)))
                       (overlay-put created 'category category)
                       (overlay-put created 'evaporate t)
                       created))))
    (unless (and (= (overlay-start overlay) beg) (= (overlay-end overlay) end))
      (move-overlay overlay beg end))
    (map-do (lambda (property value)
              (unless (equal (overlay-get overlay property) value)
                (overlay-put overlay property value)))
            props)))

(defun agent-shell-chat--label-prompts ()
  "Overlay each prompt run in the current buffer.

A prompt with non-empty input after it (a submitted turn) shows a `Me'
label.  The live prompt awaiting input has empty input: it also shows
`Me' so it can be typed into, unless `agent-shell-prompt-bar-mode' is on,
in which case input flows through the bar and the empty prompt is hidden.

Keys off the input, not the `<shell-maker-end-of-prompt>' marker (which
only appears once the response starts), so `Me' shows the instant a
prompt is submitted.

The overlay swallows the blank lines around the prompt (before it, and
the input's leading ones after it) so the label is padded by exactly one
blank line on each side, no matter how much padding the shell inserted.
Updates the overlay in place, so a prompt flips between hidden and `Me'
as soon as its input, or the bar, changes."
  (save-excursion
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((run-end (or (next-single-property-change pos 'font-lock-face) (point-max))))
          (when (agent-shell-chat--prompt-face-p
                 (get-text-property pos 'font-lock-face))
            (let* ((input-end (or (save-excursion
                                    (goto-char run-end)
                                    (when (re-search-forward
                                           "<shell-maker-end-of-prompt>" nil t)
                                      (match-beginning 0)))
                                  (point-max)))
                   (blank (string-blank-p (buffer-substring-no-properties run-end input-end)))
                   (start (save-excursion (goto-char pos)
                                          (skip-chars-backward " \t\n")
                                          (point)))
                   (end (if blank run-end
                          (save-excursion (goto-char run-end)
                                          (skip-chars-forward " \t\n")
                                          (point))))
                   (me-label (agent-shell-chat--label
                              "Me" 'agent-shell-chat-me-label))
                   (display (cond
                             ((and blank (bound-and-true-p agent-shell-prompt-bar-mode))
                              "")
                             ;; Live prompt awaiting input: show `Me' and the
                             ;; prompt marker, indented to meet the input.
                             (blank
                              (concat "\n\n" me-label "\n\n"
                                      agent-shell-chat--body-indent
                                      agent-shell-chat--prompt))
                             ;; Submitted turn: just the `Me' label.
                             (t (concat "\n\n" me-label "\n\n")))))
              (agent-shell-chat--upsert-overlay
               'agent-shell-chat-me pos run-end start end
               (list (cons 'display display)))
              ;; Indent the submitted input so it aligns with the response
              ;; body; the live (blank) prompt has no input to indent.
              (if blank
                  (when-let* ((stale (agent-shell-chat--overlay-in
                                      end (1+ end) 'agent-shell-chat-me-input)))
                    (delete-overlay stale))
                ;; End at the input's last real character, not `input-end':
                ;; the agent label's `before-string' renders in the trailing
                ;; newline before the marker, and would inherit this
                ;; `line-prefix' and sit indented.
                (let ((input-last (save-excursion (goto-char input-end)
                                                  (skip-chars-backward " \t\n")
                                                  (point))))
                  (agent-shell-chat--upsert-overlay
                   'agent-shell-chat-me-input end input-last end input-last
                   (list (cons 'line-prefix agent-shell-chat--body-indent)
                         (cons 'wrap-prefix agent-shell-chat--body-indent)))))))
          (setq pos run-end))))))

(defun agent-shell-chat--label-responses ()
  "Overlay the agent label before every response in the current buffer.

Anchored on the invisible `<shell-maker-end-of-prompt>' marker.  The
overlay swallows the blank lines around the marker (the input's trailing
newlines and the response's leading ones) and hides them with a `display'
of \"\", while a `before-string' renders the label padded by exactly one
blank line on each side.  Updates in place; idempotent."
  (save-excursion
    (goto-char (point-min))
    (let ((before (concat "\n\n" (agent-shell-chat--label
                                  (agent-shell-chat--agent-name)
                                  'agent-shell-chat-agent-label)
                          "\n\n")))
      (while (re-search-forward "<shell-maker-end-of-prompt>" nil t)
        (let ((mbeg (match-beginning 0))
              (mend (match-end 0)))
          (agent-shell-chat--upsert-overlay
           'agent-shell-chat-agent mbeg mend
           (save-excursion (goto-char mbeg) (skip-chars-backward " \t\n") (point))
           (save-excursion (goto-char mend) (skip-chars-forward " \t\n") (point))
           (list (cons 'before-string before) (cons 'display ""))))))))

(defun agent-shell-chat--relabel ()
  "Apply the `Me' and agent labels to the current buffer (idempotent).
Scans the whole buffer; cheap in practice since it walks property
changes and skips already-labeled runs, but could be scoped to the
active turn if it ever shows on very long conversations."
  (agent-shell-chat--label-prompts)
  (agent-shell-chat--label-responses))

(defun agent-shell-chat--relabel-all ()
  "Relabel every labeled shell buffer.
Used after `agent-shell-prompt-bar-mode' toggles, so the live prompt
flips between hidden and `Me' immediately across all shells."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'agent-shell-chat--labeled buffer)
      (with-current-buffer buffer
        (agent-shell-chat--relabel)))))

(defun agent-shell-chat--relabel-buffer (buffer)
  "Relabel BUFFER, clearing its pending relabel timer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-chat--relabel-timer nil)
      (agent-shell-chat--relabel))))

(defun agent-shell-chat--schedule-relabel (&rest _)
  "Schedule a coalesced, deferred relabel of the current buffer.

Deferred so the triggering change's own text properties (e.g. the prompt
face shell-maker applies after inserting) are in place; coalesced so a
burst yields a single relabel.  Runs from the event subscription (which
covers submissions, streaming, turn completion and `session-restored')
and from `shell-maker-finish-output-hook' (which covers `clear')."
  (when (and agent-shell-chat--labeled
             (not agent-shell-chat--relabel-timer))
    (setq agent-shell-chat--relabel-timer
          (run-at-time 0 nil #'agent-shell-chat--relabel-buffer (current-buffer)))))

(defun agent-shell-chat--enable-labels (buffer)
  "Turn on chat labels for shell BUFFER and keep them in sync.

Backfills existing turns, then subscribes to shell events so a coalesced
relabel tracks submissions, streaming responses, turn completion and
reloads (`session-restored').  `clear' is covered separately by
`shell-maker-finish-output-hook'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless agent-shell-chat--labeled
        (setq-local agent-shell-chat--labeled t)
        (agent-shell-chat--relabel)
        (setq-local agent-shell-chat--subscription
                    (agent-shell-subscribe-to
                     :shell-buffer buffer
                     :on-event #'agent-shell-chat--schedule-relabel))))))

(defun agent-shell-chat--on-shell-init ()
  "Enable chat labels for a newly initialized shell buffer.
Added to `agent-shell-mode-hook', so shells created while the mode is on
are labeled too."
  (agent-shell-chat--enable-labels (current-buffer)))

(defun agent-shell-chat--label-all-shells ()
  "Enable chat labels for every existing `agent-shell' buffer."
  (dolist (buffer (buffer-list))
    (when (provided-mode-derived-p
           (buffer-local-value 'major-mode buffer) 'agent-shell-mode)
      (agent-shell-chat--enable-labels buffer))))

(defun agent-shell-chat--unlabel-all ()
  "Remove chat labels, subscriptions and timers from every buffer."
  (dolist (buffer (buffer-list))
    (when (buffer-local-value 'agent-shell-chat--labeled buffer)
      (with-current-buffer buffer
        (when agent-shell-chat--subscription
          (agent-shell-unsubscribe :subscription agent-shell-chat--subscription))
        (when (timerp agent-shell-chat--relabel-timer)
          (cancel-timer agent-shell-chat--relabel-timer))
        (remove-overlays (point-min) (point-max)
                         'category 'agent-shell-chat-me)
        (remove-overlays (point-min) (point-max)
                         'category 'agent-shell-chat-me-input)
        (remove-overlays (point-min) (point-max)
                         'category 'agent-shell-chat-agent)
        (kill-local-variable 'agent-shell-chat--subscription)
        (kill-local-variable 'agent-shell-chat--relabel-timer)
        (kill-local-variable 'agent-shell-chat--labeled)))))

;;; Mode

;;;###autoload
(define-minor-mode agent-shell-chat-mode
  "Toggle chat-style `Me'/agent labels in every `agent-shell' buffer.

Each submitted turn is boxed `Me' and each response the agent's name.
The live prompt shows `Me' so you can type into the shell; when
`agent-shell-prompt-bar-mode' is on it is hidden, since input flows
through the bar instead."
  :global t
  :lighter nil
  :group 'agent-shell
  (if agent-shell-chat-mode
      (progn
        ;; `clear' is a shell-maker command with no agent-shell event, so
        ;; it rides its finish-output hook; everything else comes through
        ;; the per-buffer event subscription set up in `--enable-labels'.
        (add-hook 'shell-maker-finish-output-hook #'agent-shell-chat--schedule-relabel)
        (add-hook 'agent-shell-mode-hook #'agent-shell-chat--on-shell-init)
        (agent-shell-chat--label-all-shells))
    (remove-hook 'shell-maker-finish-output-hook #'agent-shell-chat--schedule-relabel)
    (remove-hook 'agent-shell-mode-hook #'agent-shell-chat--on-shell-init)
    (agent-shell-chat--unlabel-all)))

(provide 'agent-shell-chat-mode)

;;; agent-shell-chat-mode.el ends here
