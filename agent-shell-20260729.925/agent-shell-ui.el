;;; agent-shell-ui.el --- Interactive shell UI elements -*- lexical-binding: t; -*-

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
;; A library for creating interactive shell UI elements.
;;
;; Note: This package is in very early stages and likely has
;; rough edges.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'cl-lib)
(require 'agent-shell-work-buffer)
(require 'map)
(require 'cursor-sensor)
(require 'subr-x)
(require 'text-property-search)

(defvar agent-shell-ui-post-expand-fragment-at-point-hook nil
  "Hook run after expanding a fragment at point.
When run, the buffer is narrowed to the body region and
`inhibit-read-only' is in effect.")

(defvar agent-shell-ui-debug-enabled nil
  "When non-nil, surface internal UI debugging aids.
For example, fragment qualified-ids appear in the echo area on
hover.  These are implementation details of no use to users, so
they stay hidden by default.")

(defun agent-shell-ui--fragment-help-echo (qualified-id)
  "Return the `help-echo' value for a fragment tagged QUALIFIED-ID.
Returns QUALIFIED-ID only when `agent-shell-ui-debug-enabled' is set,
otherwise nil so the id stays hidden from users."
  (when agent-shell-ui-debug-enabled
    qualified-id))

(cl-defun agent-shell-ui-make-fragment-model (&key (namespace-id "global") (block-id "1") label-left label-right body group-id group-label (group-expanded t))
  "Create a fragment model alist.
NAMESPACE-ID, BLOCK-ID, LABEL-LEFT, LABEL-RIGHT, and BODY are the keys.

GROUP-ID nests this fragment under a collapsible group header (a sibling
fragment with `block-id' GROUP-ID in the same namespace).  When that
header does not yet exist, GROUP-LABEL materializes it (auto-create) with
GROUP-EXPANDED as its initial fold state.  GROUP-ID nil means a top-level
fragment."
  (list (cons :namespace-id namespace-id)
        (cons :block-id block-id)
        (cons :label-left (agent-shell-ui--string-or-nil label-left))
        (cons :label-right (agent-shell-ui--string-or-nil label-right))
        (cons :body (agent-shell-ui--string-or-nil body))
        (cons :group-id (agent-shell-ui--string-or-nil group-id))
        (cons :group-label (agent-shell-ui--string-or-nil group-label))
        (cons :group-expanded group-expanded)))

(cl-defun agent-shell-ui-make-group-model (&key (namespace-id "global") (block-id "1") label-left label-right (expanded t))
  "Create a group-header model alist.

A group header is a collapsible fragment with no body of its own; its
members are separate fragments referencing it by qualified-id via
`agent-shell-ui-make-fragment-model' GROUP-ID.  NAMESPACE-ID, BLOCK-ID,
LABEL-LEFT, and LABEL-RIGHT render the header line.  EXPANDED sets the
initial fold state.  v1 is two-level: a group may not itself be nested."
  (list (cons :namespace-id namespace-id)
        (cons :block-id block-id)
        (cons :kind 'group)
        (cons :label-left (agent-shell-ui--string-or-nil label-left))
        (cons :label-right (agent-shell-ui--string-or-nil label-right))
        (cons :expanded expanded)))

(defun agent-shell-ui--insert-read-only (text)
  "Insert TEXT as read-only output."
  (add-text-properties 0 (length text)
                       '(read-only t front-sticky (read-only))
                       text)
  (insert text))

(cl-defun agent-shell-ui-update-fragment (model &key append create-new on-post-process navigation expanded no-undo)
  "Update or add a fragment using MODEL.

When APPEND is non-nil, append to body instead of replacing.
When CREATE-NEW is non-nil, create new block.
When ON-POST-PROCESS is non-nil, call this function after updating.
When NAVIGATION is `never', block won't be TAB navigatable.
When NAVIGATION is `auto', block is navigatable if non-empty body.
When NAVIGATION is `always', block is always TAB navigatable.
When EXPANDED is non-nil, body will be expanded by default.
When NO-UNDO is non-nil, disable undo recording for this operation.

For existing blocks, the current expansion state is preserved unless overridden.

Updates to existing blocks are applied per section: a body append
inserts the new chunk at the end of the body region without disturbing
already-rendered content, so `agent-shell-markdown' frozen ranges
stay intact and streaming append is O(new-chunk) rather than
O(accumulated-body).  Label-only updates leave the body untouched."
  (let* ((window (get-buffer-window (current-buffer)))
         (saved-window-start (and window (window-start window))))
    (unwind-protect
        (save-mark-and-excursion
          (let* ((inhibit-read-only t)
                 (buffer-undo-list (if no-undo t buffer-undo-list))
                 (namespace-id (map-elt model :namespace-id))
                 (qualified-id (format "%s-%s" namespace-id (map-elt model :block-id)))
                 (new-label-left (map-elt model :label-left))
                 (new-label-right (map-elt model :label-right))
                 (new-body (map-elt model :body))
                 (group-member-id (map-elt model :group-id))
                 (effective-expanded (if (eq (map-elt model :kind) 'group)
                                         (map-elt model :expanded)
                                       expanded))
                 (block-start nil)
                 (padding-start nil)
                 (padding-end nil)
                 (group-header nil)
                 (match (save-mark-and-excursion
                          (goto-char (point-max))
                          (text-property-search-backward
                           'agent-shell-ui-state nil
                           (lambda (_ state)
                             (equal (map-elt state :qualified-id) qualified-id))
                           t))))
            ;; Resolve group membership.  A NEW member materializes its
            ;; header (auto-create) and routes into the group's region.  An
            ;; EXISTING member keeps whatever group it already belongs to;
            ;; an update must never create a header or re-route, otherwise a
            ;; caller whose group-id advanced (e.g. a message streamed between
            ;; a tool call and its completion) would spawn an empty group.
            ;; Either way the resolved parent qualified-id and indent are
            ;; recorded on the model so insertion and body regeneration nest.
            (cond
             ((and match (not create-new))
              (when-let* ((state (get-text-property (prop-match-beginning match)
                                                    'agent-shell-ui-state))
                          (existing-group (map-elt state :group-id)))
                (setq model (append model
                                    (list (cons :group-qualified-id existing-group)
                                          (cons :group-indent
                                                (or (map-elt state :group-indent) "  ")))))))
             (group-member-id
              (setq group-header (agent-shell-ui--insert-group-header
                                  :namespace-id namespace-id
                                  :group-id group-member-id
                                  :group-label (map-elt model :group-label)
                                  :expanded (map-elt model :group-expanded)
                                  :navigation navigation))
              (setq model (append model
                                  (list (cons :group-qualified-id (map-elt group-header :qualified-id))
                                        (cons :group-indent "  "))))))
            (when (or new-label-left new-label-right new-body)
              (cond
               ;; Existing block — apply edits per changed section.
               ((and match (not create-new))
                (let* ((state (get-text-property (prop-match-beginning match)
                                                 'agent-shell-ui-state))
                       (collapsed (map-elt state :collapsed)))
                  (setq block-start (prop-match-beginning match))
                  (save-excursion
                    (goto-char block-start)
                    (skip-chars-backward "\n")
                    (setq padding-start (point)))
                  (when new-label-left
                    (agent-shell-ui--replace-label
                     qualified-id 'label-left new-label-left))
                  (when new-label-right
                    (agent-shell-ui--replace-label
                     qualified-id 'label-right new-label-right))
                  (when new-body
                    ;; Re-derive the block extent and body range here,
                    ;; after the label replacements.  `agent-shell-ui--replace-label'
                    ;; can change a label's length, which shifts everything
                    ;; below it — a range captured before the replacements
                    ;; would point at the wrong chars (e.g. handing
                    ;; `replace-body' a stale range corrupts the body
                    ;; boundary and leaks its content past the collapse).
                    (let* ((current-block-end
                            (or (map-elt (agent-shell-ui--block-range :position block-start)
                                         :end)
                                (prop-match-end match)))
                           (existing-body-range
                            (agent-shell-ui--nearest-range-matching-property
                             :property 'agent-shell-ui-section :value 'body
                             :from block-start
                             :to current-block-end)))
                      (cond
                       ;; Append to existing body — preserves rendered content.
                       ((and append existing-body-range)
                        (agent-shell-ui--append-body
                         existing-body-range new-body qualified-id collapsed))
                       ;; Replace existing body in place.
                       (existing-body-range
                        (agent-shell-ui--replace-body
                         existing-body-range new-body qualified-id collapsed))
                       ;; Body arriving for the first time on a labels-only
                       ;; block — fall back to delete-and-regenerate so the
                       ;; indicator transitions from placeholder to triangle
                       ;; and the labels↔body separator is inserted.  Labels
                       ;; are recovered from the buffer (no cache).
                       (t
                        (let* ((existing-labels
                                (agent-shell-ui--read-fragment-labels
                                 block-start current-block-end))
                               (final-model
                                (list (cons :namespace-id namespace-id)
                                      (cons :block-id (map-elt model :block-id))
                                      (cons :label-left
                                            (or new-label-left
                                                (map-elt existing-labels :label-left)))
                                      (cons :label-right
                                            (or new-label-right
                                                (map-elt existing-labels :label-right)))
                                      (cons :body new-body)
                                      ;; Preserve group membership + indent so
                                      ;; the regenerated member stays nested.
                                      (cons :group-qualified-id
                                            (map-elt model :group-qualified-id))
                                      (cons :group-indent
                                            (map-elt model :group-indent)))))
                          (delete-region block-start current-block-end)
                          (goto-char block-start)
                          (agent-shell-ui--insert-fragment
                           final-model qualified-id (not collapsed) navigation))))))
                  (setq padding-end
                        (or (when-let* ((block-range
                                         (agent-shell-ui--block-range :position block-start)))
                              (map-elt block-range :end))
                            (point)))))
               ;; New group member, inserted into the group's region.  The
               ;; group's trailing separator (after the header) already sits
               ;; below, so no trailing newlines are added here.
               ((map-elt model :group-qualified-id)
                (goto-char (agent-shell-ui--group-insertion-point
                            :group-qualified-id (map-elt model :group-qualified-id)))
                (setq padding-start (point))
                (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
                (setq block-start (point))
                (agent-shell-ui--insert-fragment model qualified-id effective-expanded navigation)
                ;; The group's trailing separator (the header's `\n\n', inserted
                ;; once) now sits right after this last member; fold it into
                ;; this member's padding so it is not stranded outside every
                ;; block's range.
                (skip-chars-forward "\n")
                (setq padding-end (point)))
               ;; New block.
               (t
                (goto-char (point-max))
                (setq padding-start (point))
                (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
                (setq block-start (point))
                (agent-shell-ui--insert-fragment model qualified-id effective-expanded navigation)
                (agent-shell-ui--insert-read-only "\n\n")
                (setq padding-end (point)))))
            ;; A collapsed group's members must stay hidden across updates.
            ;; A member's own edit path (insert, or replace-label/body on an
            ;; update) restores visibility from the member's own state, which
            ;; would reveal it under a folded header; re-apply the group
            ;; collapse so updates don't leak members onto the header line.
            (when-let* ((group-qid (map-elt model :group-qualified-id))
                        (header (agent-shell-ui--group-header-range group-qid))
                        (header-state (get-text-property (map-elt header :start)
                                                         'agent-shell-ui-state))
                        ((map-elt header-state :collapsed)))
              (agent-shell-ui--set-group-collapsed group-qid t))
            (when on-post-process
              (funcall on-post-process))
            (when-let* ((block-range (agent-shell-ui--block-range :position block-start)))
              (list (cons :block block-range)
                    (cons :body (agent-shell-ui--nearest-range-matching-property
                                 :property 'agent-shell-ui-section :value 'body
                                 :from (map-elt block-range :start)
                                 :to (map-elt block-range :end)))
                    (cons :label-left (agent-shell-ui--nearest-range-matching-property
                                       :property 'agent-shell-ui-section :value 'label-left
                                       :from (map-elt block-range :start)
                                       :to (map-elt block-range :end)))
                    (cons :label-right (agent-shell-ui--nearest-range-matching-property
                                        :property 'agent-shell-ui-section :value 'label-right
                                        :from (map-elt block-range :start)
                                        :to (map-elt block-range :end)))
                    (cons :padding (when (and padding-start padding-end)
                                     (list (cons :start padding-start)
                                           (cons :end padding-end))))
                    (cons :group-header (map-elt group-header :range))))))
      (when window
        (set-window-start window saved-window-start t)))))

(defun agent-shell-ui--read-fragment-labels (block-start block-end)
  "Return alist with :label-left and :label-right strings (no properties).
Reads from the buffer between BLOCK-START and BLOCK-END.  Used only by
the body-arriving-on-labels-only fallback in `agent-shell-ui-update-fragment'.
Labels are short, prop-free strings — safe to round-trip through the
buffer."
  (let (fields)
    (when-let* ((range (agent-shell-ui--nearest-range-matching-property
                        :property 'agent-shell-ui-section :value 'label-right
                        :from block-start :to block-end)))
      (push (cons :label-right
                  (buffer-substring-no-properties (map-elt range :start)
                                                  (map-elt range :end)))
            fields))
    (when-let* ((range (agent-shell-ui--nearest-range-matching-property
                        :property 'agent-shell-ui-section :value 'label-left
                        :from block-start :to block-end)))
      (push (cons :label-left
                  (buffer-substring-no-properties (map-elt range :start)
                                                  (map-elt range :end)))
            fields))
    fields))

(defun agent-shell-ui--apply-body-section-properties (start end qualified-id state body-invisible)
  "Apply body-section text properties to chars in [START, END).
QUALIFIED-ID and STATE feed the help-echo and agent-shell-ui-state
properties.  BODY-INVISIBLE non-nil means the existing body region
is currently hidden (collapsed label-ful fragment); new chars must
match.  Explicit `invisible' assignment overrides any value the
new chars might have inherited via rear-stickiness from preceding
trailing-whitespace chars."
  (add-text-properties start end
                       `(agent-shell-ui-section body
                                                help-echo ,(agent-shell-ui--fragment-help-echo qualified-id)
                                                read-only t
                                                front-sticky (read-only)))
  (when state
    (put-text-property start end 'agent-shell-ui-state state))
  (put-text-property start end 'invisible (if body-invisible t nil)))

(defun agent-shell-ui--body-invisible-p (body-start body-end)
  "Return non-nil if the existing body region [BODY-START, BODY-END) is hidden.
Inspects the `invisible' property on the first body char.  The
trailing-whitespace handler only sets `invisible' on chars from
the last non-whitespace position onwards, never the first char,
so the first char's `invisible' tracks the body's true collapse
state — including whitespace-only bodies (e.g. a body left as
two newlines after the markdown renderer stripped an empty
fenced block)."
  (and (< body-start body-end)
       (eq (get-text-property body-start 'invisible) t)))

(defun agent-shell-ui--apply-trailing-whitespace-invisible (body-start body-end)
  "Hide trailing whitespace within [BODY-START, BODY-END) via invisible property.
Marks the hidden chars `rear-nonsticky' for `invisible' so chars later
inserted at BODY-END don't silently inherit `invisible t' from the
trailing-whitespace tail."
  (save-excursion
    (goto-char body-end)
    (when (re-search-backward "[^ \t\n]" body-start t)
      (forward-char 1)
      (when (< (point) body-end)
        (add-text-properties (point) body-end
                             '(invisible t rear-nonsticky (invisible)))))))

(defun agent-shell-ui--append-body (body-range chunk qualified-id _collapsed)
  "Append CHUNK to the body region described by BODY-RANGE.

BODY-RANGE is an alist with `:start' and `:end' marking the existing
body section.  Existing body chars stay in place — `agent-shell-markdown'
frozen tags and per-char faces survive across streaming chunks, no
re-rendering needed.  QUALIFIED-ID is the fragment identifier used to
tag the new chars so the body's section property and help-echo line up
with the rest of the block.

_COLLAPSED is intentionally unused: visibility for new chars is derived
from the current visibility of the existing body, not from caller-supplied
state, because label-less fragments don't follow `state :collapsed'
\(their bodies stay visible regardless of how `:collapsed' was stored)."
  (when (and (stringp chunk) (not (string-empty-p chunk)))
    (let* ((body-start (map-elt body-range :start))
           (body-end (map-elt body-range :end))
           (state (get-text-property (max body-start (1- body-end))
                                     'agent-shell-ui-state))
           (body-invisible (agent-shell-ui--body-invisible-p body-start body-end)))
      ;; Trailing-whitespace invisibility on the old tail may no longer
      ;; apply once the chunk lands — clear and re-derive.  Only when
      ;; the body is visible; for a hidden body the existing invisible
      ;; spans the whole body and must stay.
      (unless body-invisible
        (remove-text-properties body-start body-end '(invisible nil)))
      (goto-char body-end)
      (let ((insert-start (point)))
        (insert (agent-shell-ui--indent-text
                 chunk (concat (or (map-elt state :group-indent) "") "  ")))
        (let ((insert-end (point)))
          (agent-shell-ui--apply-body-section-properties
           insert-start insert-end qualified-id state body-invisible)
          (agent-shell-ui--apply-trailing-whitespace-invisible
           body-start insert-end))))))

(defun agent-shell-ui--replace-body (body-range new-body qualified-id _collapsed)
  "Replace the body region described by BODY-RANGE with NEW-BODY.

BODY-RANGE is an alist with `:start' and `:end'.  Only the body chars
are touched — the surrounding label, indicator, and padding stay put,
so block-id and section tagging on the rest of the block are preserved.
QUALIFIED-ID is the fragment identifier used to tag the inserted chars.

_COLLAPSED is intentionally unused: visibility on the inserted chars
matches the body's current visibility, not caller-supplied state."
  (let* ((body-start (map-elt body-range :start))
         (body-end (map-elt body-range :end))
         (state (get-text-property (max body-start (1- body-end))
                                   'agent-shell-ui-state))
         (body-invisible (agent-shell-ui--body-invisible-p body-start body-end)))
    (delete-region body-start body-end)
    (goto-char body-start)
    (when (and (stringp new-body) (not (string-empty-p new-body)))
      (let ((trimmed new-body))
        (when (string-prefix-p "\n" trimmed)
          (setq trimmed (string-trim-left trimmed "\n")))
        (when (string-suffix-p "\n\n" trimmed)
          (setq trimmed (concat (string-trim-right trimmed) "\n\n")))
        (let ((insert-start (point)))
          (insert (agent-shell-ui--indent-text
                   (string-remove-prefix "  " trimmed)
                   (concat (or (map-elt state :group-indent) "") "  ")))
          (let ((insert-end (point)))
            (agent-shell-ui--apply-body-section-properties
             insert-start insert-end qualified-id state body-invisible)
            (agent-shell-ui--apply-trailing-whitespace-invisible
             insert-start insert-end)))))))

(defun agent-shell-ui--replace-label (qualified-id section new-text)
  "Replace the SECTION region of fragment QUALIFIED-ID with NEW-TEXT.

SECTION is one of `label-left' or `label-right'.  Only the named label
region is rewritten — the other label, the indicator, and the body of
the same block stay untouched, so block tagging and fragment identity
are preserved across label updates."
  (when (stringp new-text)
    (when-let* ((block-match
                 (save-excursion
                   (goto-char (point-max))
                   (text-property-search-backward
                    'agent-shell-ui-state nil
                    (lambda (_ state)
                      (equal (map-elt state :qualified-id) qualified-id))
                    t)))
                (region
                 (save-excursion
                   (goto-char (prop-match-beginning block-match))
                   (when-let* ((m (text-property-search-forward
                                   'agent-shell-ui-section section t t)))
                     (when (<= (prop-match-end m) (prop-match-end block-match))
                       (cons (prop-match-beginning m)
                             (prop-match-end m)))))))
      (let* ((region-start (car region))
             (region-end (cdr region))
             (state (get-text-property region-start 'agent-shell-ui-state)))
        (delete-region region-start region-end)
        (goto-char region-start)
        (let ((insert-start (point)))
          (insert (agent-shell-ui-add-action-to-text
                   new-text
                   (lambda ()
                     (interactive)
                     (agent-shell-ui--toggle-fragment-at-point))
                   (lambda ()
                     (message "Press RET to toggle"))))
          (let ((insert-end (point)))
            (add-text-properties insert-start insert-end
                                 `(agent-shell-ui-section ,section
                                                          help-echo ,(agent-shell-ui--fragment-help-echo qualified-id)
                                                          read-only t
                                                          front-sticky (read-only)))
            (when state
              (put-text-property insert-start insert-end
                                 'agent-shell-ui-state state))))))))


(cl-defun agent-shell-ui-delete-fragment (&key namespace-id block-id no-undo)
  "Delete fragment with NAMESPACE-ID and BLOCK-ID.

When NO-UNDO is non-nil, disable undo recording for this operation."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list (if no-undo t buffer-undo-list))
           (qualified-id (format "%s-%s" namespace-id block-id))
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when match
        (let ((block-start (prop-match-beginning match))
              (block-end (prop-match-end match)))
          ;; Remove trailing vertical space that's part of the block, but
          ;; stop at the next fragment's content.  The next fragment's
          ;; leading indicator (e.g. the "  " collapse placeholder) is
          ;; whitespace too, so a plain `skip-chars-forward' would swallow
          ;; it and misalign that fragment.  Its chars carry an
          ;; `agent-shell-ui-state', which the inter-block separators do not.
          (goto-char block-end)
          (while (and (not (eobp))
                      (memq (char-after) '(?\s ?\t ?\n))
                      (not (get-text-property (point) 'agent-shell-ui-state)))
            (forward-char 1))
          (setq block-end (point))
          (delete-region block-start block-end))))))

(cl-defun agent-shell-ui--block-range (&key position)
  "Get block range at POSITION if found.  Nil otherwise.

In the form:

  ((start . 1)
   (end . 3))."
  (when-let* ((qualified-id (map-elt (get-text-property (or position (point)) 'agent-shell-ui-state) :qualified-id)))
    (agent-shell-ui--nearest-range-matching-property
     :property 'agent-shell-ui-state
     :value qualified-id
     :predicate (lambda (qualified-id property)
                  (equal (map-elt property :qualified-id) qualified-id)))))

(cl-defun agent-shell-ui--nearest-range-matching-property (&key property value (predicate t) from to)
  "Return nearest range where PREDICATE is non-nil for PROPERTY and VALUE."
  (save-mark-and-excursion
    (save-restriction
      (when (and from to)
        (narrow-to-region from to))
      (let ((backward-match (or (text-property-search-backward property value predicate)
                                (progn
                                  (unless (eobp)
                                    (forward-char 1))
                                  (text-property-search-backward property value predicate))))
            (forward-match (text-property-search-forward property value predicate)))
        (when (or backward-match forward-match)
          `((:start . ,(if backward-match
                           (prop-match-beginning backward-match)
                         (prop-match-beginning forward-match)))
            (:end . ,(if forward-match
                         (prop-match-end forward-match)
                       (prop-match-end backward-match)))))))))

(defun agent-shell-ui--group-header-range (group-qualified-id)
  "Return (:start :end) of the group header GROUP-QUALIFIED-ID, or nil."
  (save-mark-and-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward
                        'agent-shell-ui-state nil
                        (lambda (_ state)
                          (and (equal (map-elt state :qualified-id) group-qualified-id)
                               (eq (map-elt state :kind) 'group)))
                        t)))
      (agent-shell-ui--block-range :position (prop-match-beginning match)))))

(cl-defun agent-shell-ui--group-children (&key group-qualified-id)
  "Return ordered member block ranges of group GROUP-QUALIFIED-ID.
Each element is (:qualified-id ID :start S :end E).  Members are the
fragments that follow the header contiguously and carry `:group-id'
equal to GROUP-QUALIFIED-ID; the run stops at the first non-member."
  (when-let* ((header (agent-shell-ui--group-header-range group-qualified-id)))
    (save-mark-and-excursion
      (let ((children '())
            (pos (map-elt header :end)))
        (catch 'done
          (while t
            (goto-char pos)
            (skip-chars-forward " \t\n")
            (when (eobp) (throw 'done nil))
            (let ((state (get-text-property (point) 'agent-shell-ui-state)))
              (unless (and state (equal (map-elt state :group-id) group-qualified-id))
                (throw 'done nil))
              (let ((block (agent-shell-ui--block-range :position (point))))
                (push (list (cons :qualified-id (map-elt state :qualified-id))
                            (cons :start (map-elt block :start))
                            (cons :end (map-elt block :end)))
                      children)
                (setq pos (map-elt block :end))))))
        (nreverse children)))))

(cl-defun agent-shell-ui--group-child-region (&key group-qualified-id)
  "Return (:start :end) spanning group GROUP-QUALIFIED-ID's members, or nil.
Spans from just after the header to the end of the last member."
  (when-let* ((header (agent-shell-ui--group-header-range group-qualified-id))
              (children (agent-shell-ui--group-children :group-qualified-id group-qualified-id)))
    (list (cons :start (map-elt header :end))
          (cons :end (map-elt (car (last children)) :end)))))

(cl-defun agent-shell-ui--group-insertion-point (&key group-qualified-id)
  "Return the buffer position for a new member of group GROUP-QUALIFIED-ID.
After the current last member, or just after the header when empty."
  (when-let* ((header (agent-shell-ui--group-header-range group-qualified-id)))
    (if-let* ((children (agent-shell-ui--group-children :group-qualified-id group-qualified-id)))
        (map-elt (car (last children)) :end)
      (map-elt header :end))))

(cl-defun agent-shell-ui--insert-group-header (&key namespace-id group-id group-label (expanded t) navigation)
  "Insert a header for NAMESPACE-ID/GROUP-ID unless one already exists.
When created, it lands at `point-max' with GROUP-LABEL as its label,
EXPANDED as its initial fold state, and NAVIGATION for navigability.
Return an alist with `:qualified-id' and, only when this call created
the header, `:range' as (:start . :end) spanning the inserted header
plus its surrounding padding."
  (let ((group-qualified-id (format "%s-%s" namespace-id group-id))
        (range nil))
    (unless (agent-shell-ui--group-header-range group-qualified-id)
      (goto-char (point-max))
      (let ((start (point)))
        (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
        (agent-shell-ui--insert-fragment
         (agent-shell-ui-make-group-model
          :namespace-id namespace-id :block-id group-id
          :label-left group-label :expanded expanded)
         group-qualified-id expanded navigation)
        (agent-shell-ui--insert-read-only "\n\n")
        (setq range (list (cons :start start)
                          (cons :end (point))))))
    (list (cons :qualified-id group-qualified-id)
          (cons :range range))))

(defun agent-shell-ui--labels-end (block)
  "Return the end of BLOCK's label-right, else label-left, else nil."
  (or (map-elt (agent-shell-ui--nearest-range-matching-property
                :property 'agent-shell-ui-section :value 'label-right
                :from (map-elt block :start) :to (map-elt block :end))
               :end)
      (map-elt (agent-shell-ui--nearest-range-matching-property
                :property 'agent-shell-ui-section :value 'label-left
                :from (map-elt block :start) :to (map-elt block :end))
               :end)))

(defun agent-shell-ui--set-indicator-collapsed (block collapsed)
  "Set BLOCK's fold indicator glyph to `▶'/`▼' to match COLLAPSED.
Both glyphs are two columns wide, so surrounding positions do not shift."
  (when-let* ((indicator (agent-shell-ui--nearest-range-matching-property
                          :property 'agent-shell-ui-section :value 'indicator
                          :from (map-elt block :start) :to (map-elt block :end)))
              (props (text-properties-at (map-elt indicator :start))))
    (delete-region (map-elt indicator :start) (map-elt indicator :end))
    (goto-char (map-elt indicator :start))
    (insert (if collapsed "▶ " "▼ "))
    (add-text-properties (map-elt indicator :start) (point) props)))

(defun agent-shell-ui--apply-own-collapsed (block-start)
  "Re-apply the fragment at BLOCK-START's own fold state to its content.
Leaf: hide/show its body per `:collapsed'.  Group: recurse into members."
  (when-let* ((state (get-text-property block-start 'agent-shell-ui-state))
              (block (agent-shell-ui--block-range :position block-start)))
    (if (eq (map-elt state :kind) 'group)
        (agent-shell-ui--set-group-collapsed (map-elt state :qualified-id)
                                             (and (map-elt state :collapsed) t))
      (when-let* ((body (agent-shell-ui--nearest-range-matching-property
                         :property 'agent-shell-ui-section :value 'body
                         :from (map-elt block :start) :to (map-elt block :end)))
                  (invisible-start (agent-shell-ui--labels-end block)))
        (put-text-property invisible-start (map-elt body :end)
                           'invisible (and (map-elt state :collapsed) t))))))

(defun agent-shell-ui--set-group-collapsed (group-qualified-id collapsed)
  "Fold or unfold group GROUP-QUALIFIED-ID (recompute-on-toggle).
COLLAPSED hides the whole member region regardless of member states;
expanding reveals it and restores each member's own fold state."
  (when-let* ((header (agent-shell-ui--group-header-range group-qualified-id))
              (region (agent-shell-ui--group-child-region
                       :group-qualified-id group-qualified-id))
              (state (get-text-property (map-elt header :start) 'agent-shell-ui-state)))
    (if collapsed
        (put-text-property (map-elt region :start) (map-elt region :end)
                           'invisible t)
      (put-text-property (map-elt region :start) (map-elt region :end)
                         'invisible nil)
      (dolist (child (agent-shell-ui--group-children
                      :group-qualified-id group-qualified-id))
        (agent-shell-ui--apply-own-collapsed (map-elt child :start))))
    (agent-shell-ui--set-indicator-collapsed header collapsed)
    (map-put! state :collapsed collapsed)
    (put-text-property (map-elt header :start) (map-elt header :end)
                       'agent-shell-ui-state state)))

(defun agent-shell-ui--insert-fragment (model qualified-id &optional expanded navigation)
  "Insert fragment from MODEL with QUALIFIED-ID text properties.
EXPANDED determines initial state (default nil for collapsed).
NAVIGATION controls navigability:

 `never' (not navigatable)
 `auto' (navigatable if body and indicator present)
 `always' (always navigatable).

A group header (MODEL `:kind' `group') gets a fold triangle and no body of
its own; its members render below it as separate fragments tagged with its
qualified-id via `:group-qualified-id'.  MODEL `:group-indent' visually
indents a member's header line under its group header."
  (let* ((block-start (point))
         (kind (map-elt model :kind))
         (group (eq kind 'group))
         (group-indent (or (map-elt model :group-indent) ""))
         (group-qualified-id (map-elt model :group-qualified-id))
         (body-indent (concat group-indent "  "))
         (label-left (map-elt model :label-left))
         (label-right (map-elt model :label-right))
         (body (unless group (map-elt model :body)))
         (need-space nil)
         (indicator-start)
         (indicator-end)
         (label-left-start)
         (label-left-end)
         (label-right-start)
         (label-right-end)
         (body-start)
         (body-end)
         (collapsable))

    ;; Insert collapse indicator.  A body (or a group header, whose members
    ;; are its collapsible content) gets a fold triangle; a plain labels-only
    ;; fragment reserves two columns so it aligns and doesn't jump when a
    ;; body arrives later.
    (when-let* ((has-labels (or label-left label-right)))
      (if (or body group)
          (progn
            (setq collapsable (and body has-labels))
            (setq indicator-start (point))
            (insert (agent-shell-ui-add-action-to-text
                     (if expanded "▼ " "▶ ")
                     (lambda ()
                       (interactive)
                       (agent-shell-ui--toggle-fragment-at-point))
                     (lambda ()
                       (message "Press RET to toggle"))))
            (setq indicator-end (point))
            (add-text-properties indicator-start indicator-end
                                 `(agent-shell-ui-section indicator
                                                          keymap ,(agent-shell-ui-make-action-keymap
                                                                   (lambda ()
                                                                     (interactive)
                                                                     (agent-shell-ui--toggle-fragment-at-point)))
                                                          read-only t
                                                          front-sticky (read-only))))
        (setq collapsable nil)
        (setq indicator-start (point))
        ;; Reserving the space for expand indicators enables
        ;; aligning columns but also avoids text jumping when
        ;; body arrives later on.
        ;;
        ;; For example:
        ;;
        ;; "   [ completed ] [ read ] Read agent-shell/README.org"
        ;;
        ;; vs
        ;;
        ;; "▼  [ completed ] [ read ] Read agent-shell/README.org"
        (insert "  ") ;; "▶ "
        (setq indicator-end (point))))

    (when label-left
      (setq label-left-start (point))
      (insert (agent-shell-ui-add-action-to-text
               label-left
               (lambda ()
                 (interactive)
                 (agent-shell-ui--toggle-fragment-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-left-end (point))
      (add-text-properties label-left-start label-left-end
                           `(agent-shell-ui-section label-left
                                                    help-echo ,(agent-shell-ui--fragment-help-echo qualified-id)
                                                    read-only t
                                                    front-sticky (read-only)))
      (setq need-space t))

    (when label-right
      (when need-space
        (insert " "))
      (setq label-right-start (point))
      (insert (agent-shell-ui-add-action-to-text
               label-right
               (lambda ()
                 (interactive)
                 (agent-shell-ui--toggle-fragment-at-point))
               (lambda ()
                 (message "Press RET to toggle"))))
      (setq label-right-end (point))
      (add-text-properties label-right-start label-right-end
                           `(agent-shell-ui-section label-right
                                                    help-echo ,(agent-shell-ui--fragment-help-echo qualified-id)
                                                    read-only t
                                                    front-sticky (read-only))))

    (when body
      (when (or label-left label-right)
        (insert "\n\n"))
      ;; Drop any leading body newlines as newlines are
      ;; already inserted between labels and body.
      (when (string-prefix-p "\n" body)
        (setq body (string-trim-left body "\n")))
      ;; Never leave more than two trailing newlines.
      (when (string-suffix-p "\n\n" body)
        (setq body (concat (string-trim-right body) "\n\n")))
      (setq body-start (point))
      (let ((clean-body (string-remove-prefix "  " body)))
        (insert (agent-shell-ui--indent-text clean-body body-indent)))
      (setq body-end (point))
      (add-text-properties body-start body-end
                           `(agent-shell-ui-section body
                                                    help-echo ,(agent-shell-ui--fragment-help-echo qualified-id)
                                                    read-only t
                                                    front-sticky (read-only))))
    ;; Indent a group member's header line under its group header.  The
    ;; body already carries its own (deeper) `line-prefix' from above.
    (unless (string-empty-p group-indent)
      (add-text-properties block-start (or label-right-end label-left-end indicator-end)
                           `(line-prefix ,group-indent wrap-prefix ,group-indent)))
    ;; Include the newlines before the body in the invisible region
    (when collapsable
      (add-text-properties (or label-right-end label-left-end)
                           body-end
                           `(invisible ,(if expanded nil t))))
    ;; Hide trailing whitespace (don't delete) in body using text properties.
    (when body
      (save-mark-and-excursion
        (goto-char body-end)
        (when (re-search-backward "[^ \t\n]" body-start t)
          (forward-char 1)
          (when (< (point) body-end)
            (add-text-properties (point) body-end
                                 '(invisible t))))))
    (put-text-property
     block-start (or body-end label-right-end label-left-end)
     'agent-shell-ui-state (list
                            (cons :qualified-id qualified-id)
                            (cons :kind kind)
                            (cons :group-id group-qualified-id)
                            (cons :group-indent group-indent)
                            (cons :collapsed (not expanded))
                            (cons :navigatable (cond
                                                ((eq navigation 'never) nil)
                                                ((eq navigation 'always) t)
                                                (group t)
                                                ((eq navigation 'auto)
                                                 (and body indicator-start))
                                                (t
                                                 ;; Default to auto
                                                 (and body indicator-start))))))
    (put-text-property block-start (or body-end label-right-end label-left-end) 'read-only t)
    (put-text-property block-start (or body-end label-right-end label-left-end) 'front-sticky '(read-only))))

(cl-defun agent-shell-ui-update-text (&key namespace-id block-id text append create-new no-undo)
  "Update or insert a plain text entry identified by NAMESPACE-ID and BLOCK-ID.

TEXT is the string to insert or append.
When APPEND is non-nil, append TEXT to existing entry.
When CREATE-NEW is non-nil, always create a new entry.
When NO-UNDO is non-nil, disable undo recording."
  (save-mark-and-excursion
    (let* ((inhibit-read-only t)
           (buffer-undo-list (if no-undo t buffer-undo-list))
           (qualified-id (format "%s-%s" namespace-id block-id))
           (props `(agent-shell-ui-state ((:qualified-id . ,qualified-id))
                                         read-only t
                                         front-sticky (read-only)))
           (match (save-mark-and-excursion
                    (goto-char (point-max))
                    (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_ state)
                       (equal (map-elt state :qualified-id) qualified-id))
                     t))))
      (when text
        (cond
         ;; Append to existing entry.
         ((and match (not create-new) append)
          (goto-char (prop-match-end match))
          (insert (apply #'propertize text props))
          (list (cons :block (list (cons :start (prop-match-beginning match))
                                   (cons :end (point))))
                (cons :padding (list (cons :start (prop-match-beginning match))
                                     (cons :end (point))))))
         ;; Replace existing entry.
         ((and match (not create-new))
          (let ((padding-start (save-excursion
                                 (goto-char (prop-match-beginning match))
                                 (skip-chars-backward "\n")
                                 (point))))
            (delete-region (prop-match-beginning match) (prop-match-end match))
            (goto-char (prop-match-beginning match))
            (insert (apply #'propertize text props))
            (list (cons :block (list (cons :start (prop-match-beginning match))
                                     (cons :end (point))))
                  (cons :padding (list (cons :start padding-start)
                                       (cons :end (point)))))))
         ;; New entry.
         (t
          (goto-char (point-max))
          (let ((padding-start (point)))
            (agent-shell-ui--insert-read-only (agent-shell-ui--required-newlines 2))
            (let ((block-start (point)))
              (insert (apply #'propertize text props))
              (list (cons :block (list (cons :start block-start)
                                       (cons :end (point))))
                    (cons :padding (list (cons :start padding-start)
                                         (cons :end (point)))))))))))))

(defun agent-shell-ui--required-newlines (desired)
  "Return string of newlines needed to reach DESIRED before POSITION."
  (let ((context (save-mark-and-excursion
                   (let ((end (point)))
                     (forward-line (- (+ 1 desired)))
                     (buffer-substring (point) end)))))
    (agent-shell-with-work-buffer
      (insert context)
      ;; When counting visible newlines before point,
      ;; we may encounter invisible text, which may
      ;; look like newlines but gives false negatives.
      ;; In those cases, delete any 'invisible text
      ;; and try counting.
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (while (not (bobp))
          (let* ((end (point))
                 (start (previous-single-property-change end 'invisible nil (point-min))))
            (if (get-text-property (1- end) 'invisible)
                (delete-region start end))
            (goto-char start))))
      (goto-char (point-max))
      (let ((pos (point)))
        (skip-chars-backward "\n")
        (make-string (max 0 (- desired (- pos (point)))) ?\n)))))

(defun agent-shell-ui--toggle-fragment-at-point ()
  "Toggle visibility of fragment body at point.
Internal primitive; callers must position point on the fragment's
state-property range first.  User-facing toggling goes through
`agent-shell-ui-toggle-fragment'."
  (save-mark-and-excursion
    (if-let* ((state (get-text-property (point) 'agent-shell-ui-state))
              ((eq (map-elt state :kind) 'group))
              (inhibit-read-only t)
              (buffer-undo-list t))
        (agent-shell-ui--set-group-collapsed
         (map-elt state :qualified-id)
         (not (map-elt state :collapsed)))
      (agent-shell-ui--toggle-leaf-fragment-at-point))))

(defun agent-shell-ui--toggle-leaf-fragment-at-point ()
  "Toggle visibility of a non-group fragment's body at point."
  (save-mark-and-excursion
    (when-let* ((inhibit-read-only t)
                (buffer-undo-list t)
                (state (get-text-property (point) 'agent-shell-ui-state))
                (block (agent-shell-ui--block-range :position (point)))
                (body (agent-shell-ui--nearest-range-matching-property
                       :property 'agent-shell-ui-section :value 'body
                       :from (map-elt block :start)
                       :to (map-elt block :end)))
                (indicator (agent-shell-ui--nearest-range-matching-property
                            :property 'agent-shell-ui-section :value 'indicator
                            :from (map-elt block :start)
                            :to (map-elt block :end)))
                ;; Find where labels end (either label-right or label-left)
                (invisible-start (or (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-right
                                               :from (map-elt block :start)
                                               :to (map-elt block :end))
                                              :end)
                                     (map-elt (agent-shell-ui--nearest-range-matching-property
                                               :property 'agent-shell-ui-section :value 'label-left
                                               :from (map-elt block :start)
                                               :to (map-elt block :end))
                                              :end)))
                ;; Must be saved before deleting region.
                (indicator-properties (text-properties-at (map-elt indicator :start))))
      (let ((new-collapsed-state (not (map-elt state :collapsed))))
        ;; Toggle invisible text property including newlines before body
        (put-text-property invisible-start
                           (map-elt body :end)
                           'invisible new-collapsed-state)
        ;; Update indicator
        (delete-region (map-elt indicator :start)
                       (map-elt indicator :end))
        (goto-char (map-elt indicator :start))
        (insert (if new-collapsed-state "▶ " "▼ "))
        ;; Update state
        (add-text-properties (map-elt indicator :start)
                             (point) indicator-properties)
        (map-put! state :collapsed new-collapsed-state)
        (put-text-property (map-elt block :start)
                           (map-elt block :end) 'agent-shell-ui-state state)
        (unless new-collapsed-state
          (save-restriction
            (narrow-to-region (map-elt body :start) (map-elt body :end))
            (run-hooks 'agent-shell-ui-post-expand-fragment-at-point-hook)))))))

(defun agent-shell-ui-collapse-fragment-by-id (namespace-id block-id)
  "Collapse fragment with NAMESPACE-ID and BLOCK-ID."
  (save-mark-and-excursion
    (let ((qualified-id (format "%s-%s" namespace-id block-id)))
      (goto-char (point-max))
      (when (text-property-search-backward
             'agent-shell-ui-state qualified-id
             (lambda (_ state)
               (equal (map-elt state :qualified-id) qualified-id))
             t)
        (agent-shell-ui--toggle-fragment-at-point)))))

(defvar-local agent-shell-ui--fold-toggle-state nil
  "Current global fold state for the buffer.
One of `expanded', `collapsed', or nil (first call — derive from buffer).
Used by `agent-shell-ui-toggle-all-fragments' to alternate.")

(defun agent-shell-ui--enclosing-fragment-position ()
  "Return position of the nearest enclosing fragment, or nil.

If point is already on a fragment (has `agent-shell-ui-state' property),
return point.  Otherwise scan backward for the nearest fragment whose
block range contains point, then forward as a fallback."
  (if (get-text-property (point) 'agent-shell-ui-state)
      (point)
    (let ((pos (point)))
      (or (save-mark-and-excursion
            (when-let* ((match (text-property-search-backward
                                'agent-shell-ui-state nil
                                (lambda (_ state) (and state t))
                                t))
                        (start (prop-match-beginning match))
                        (block (agent-shell-ui--block-range :position start))
                        ((>= pos (map-elt block :start)))
                        ((<= pos (map-elt block :end))))
              start))
          (save-mark-and-excursion
            (when-let* ((match (text-property-search-forward
                                'agent-shell-ui-state nil
                                (lambda (_ state) (and state t))
                                t)))
              (prop-match-beginning match)))))))

(defun agent-shell-ui-toggle-fragment ()
  "Toggle fragment fold at or near point.

If point is on a fragment, toggle that fragment.  If point is inside a
fragment's block range, toggle the enclosing fragment.  Otherwise jump
to the next fragment forward and toggle it.

After toggling, point returns to its starting position, except when the
action collapsed the fragment from inside the body — that position is
now invisible, so point moves to the start of the title line instead.
Silent no-op when no fragment exists at or after point."
  (interactive)
  (when-let* ((origin (point))
              (target (agent-shell-ui--enclosing-fragment-position)))
    (let* ((target-id (map-elt (get-text-property target 'agent-shell-ui-state)
                               :qualified-id))
           (origin-id (map-elt (get-text-property origin 'agent-shell-ui-state)
                               :qualified-id))
           (origin-was-inside (and origin-id (equal origin-id target-id))))
      (goto-char target)
      (agent-shell-ui--toggle-fragment-at-point)
      (when origin-was-inside
        (when-let* ((block (agent-shell-ui--block-range :position target)))
          (if (get-text-property origin 'invisible)
              ;; The prior position is now invisible — land at the start
              ;; of the title line instead.
              (goto-char (map-elt block :start))
            (goto-char (min origin (map-elt block :end)))))))))

(defun agent-shell-ui--majority-collapsed-p ()
  "Return non-nil when most navigatable fragments in the buffer are collapsed.
Used by `agent-shell-ui-toggle-all-fragments' on its first invocation
when the toggle state hasn't been established yet."
  (save-mark-and-excursion
    (goto-char (point-min))
    ;; Dedup: text-property-search may visit the same qualified-id
    ;; multiple times within a single block's propertied region.
    (let ((collapsed 0)
          (expanded 0)
          (seen-ids nil)
          next)
      (while (setq next (text-property-search-forward
                         'agent-shell-ui-state nil
                         (lambda (_ state)
                           (and state (map-elt state :navigatable)))
                         t))
        (when-let* ((start (prop-match-beginning next))
                    (state (get-text-property start 'agent-shell-ui-state))
                    (qid (map-elt state :qualified-id))
                    ((not (member qid seen-ids))))
          (push qid seen-ids)
          (if (map-elt state :collapsed)
              (setq collapsed (1+ collapsed))
            (setq expanded (1+ expanded))))
        (goto-char (prop-match-end next)))
      (> collapsed expanded))))

(defun agent-shell-ui-toggle-all-fragments ()
  "Toggle global fold state: all-expanded ↔ all-collapsed.

Iterates over every navigatable fragment in the buffer.  When the
current global state is `expanded', all fragments are collapsed and
state flips to `collapsed'.  Next invocation expands them all again.

On first invocation (state nil), examines the buffer to derive the
current majority state, then flips it — so the command \"does what you
see,\" regardless of any manual folds in place.

Fragments whose `:navigatable' flag is nil (e.g. inline message chunks)
are skipped — they have no fold indicator to act on."
  (interactive)
  (let* ((target-collapsed
          (pcase agent-shell-ui--fold-toggle-state
            ('expanded t)
            ('collapsed nil)
            (_ (not (agent-shell-ui--majority-collapsed-p)))))
         (origin (point))
         ;; Dedup: text-property-search may visit the same qualified-id
         ;; multiple times within a single block's propertied region.
         (seen-ids nil))
    (save-mark-and-excursion
      (goto-char (point-min))
      (let (next)
        (while (setq next (text-property-search-forward
                           'agent-shell-ui-state nil
                           (lambda (_ state)
                             (and state (map-elt state :navigatable)))
                           t))
          (when-let* ((start (prop-match-beginning next))
                      (state (get-text-property start 'agent-shell-ui-state))
                      (qid (map-elt state :qualified-id))
                      ((not (member qid seen-ids))))
            (push qid seen-ids)
            (when (eq (map-elt state :collapsed) (not target-collapsed))
              (goto-char start)
              (agent-shell-ui--toggle-fragment-at-point)))
          (goto-char (prop-match-end next)))))
    (setq agent-shell-ui--fold-toggle-state
          (if target-collapsed 'collapsed 'expanded))
    (goto-char origin)))

(defun agent-shell-ui--string-or-nil (str)
  "Return STR if it is not nil and not empty, otherwise nil."
  (and str (not (string-empty-p str)) str))

(defun agent-shell-ui--indent-text (text &optional indent-string)
  "Indent TEXT visually without affecting copied text.
INDENT-STRING defaults to two spaces.
Uses `line-prefix' display property so indentation is visual only.

TEXT's caller-set text properties (for example `agent-shell-markdown-frozen'
on a pre-rendered diff) are preserved on every char — the previous
split-and-rejoin reconstructed the inter-line `\\n's as bare strings,
which broke contiguous property ranges and made the markdown
renderer's avoid-range checks miss header / blockquote matches
that span a line break."
  (when text
    (let ((indent (or indent-string "  "))
          (copy (copy-sequence text)))
      (add-text-properties 0 (length copy)
                           `(line-prefix ,indent wrap-prefix ,indent)
                           copy)
      copy)))

(defun agent-shell-ui--next-visible-navigatable ()
  "From point, return the start of the next visible navigatable block, or nil.
Blocks hidden inside a collapsed group (their start is `invisible') are
skipped; a collapsed leaf fragment, whose header stays visible, is not."
  (let (result)
    (catch 'done
      (while t
        (let ((next (text-property-search-forward
                     'agent-shell-ui-state nil
                     (lambda (_old-val new-val)
                       (and new-val (map-elt new-val :navigatable)))
                     t)))
          (unless next (throw 'done nil))
          (let ((beg (prop-match-beginning next)))
            (unless (invisible-p beg)
              (setq result beg)
              (throw 'done nil))))))
    result))

(defun agent-shell-ui--previous-visible-navigatable ()
  "From point, return the start of the previous visible navigatable block, or nil.
Skips blocks hidden inside a collapsed group (see
`agent-shell-ui--next-visible-navigatable')."
  (let (result)
    (catch 'done
      (while t
        (let ((prev (text-property-search-backward
                     'agent-shell-ui-state nil
                     (lambda (_old-val new-val)
                       (and new-val (map-elt new-val :navigatable)))
                     t)))
          (unless prev (throw 'done nil))
          (let ((beg (prop-match-beginning prev)))
            (unless (invisible-p beg)
              (setq result beg)
              (throw 'done nil))))))
    result))

(defun agent-shell-ui-forward-block ()
  "Jump to the next block."
  (interactive)
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       ;; In navigatable block already
                       ;; move past it.
                       (when-let* ((state (get-text-property (point) 'agent-shell-ui-state))
                                   (block (agent-shell-ui--block-range :position (point))))
                         (goto-char (map-elt block :end)))
                       (agent-shell-ui--next-visible-navigatable))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

(defun agent-shell-ui-backward-block ()
  "Jump to the previous block.

When point is strictly inside a navigatable block, jump to that
block's beginning instead of the previous block."
  (interactive)
  (when-let* ((start-point (point))
              (found (save-mark-and-excursion
                       (let* ((state (get-text-property (point) 'agent-shell-ui-state))
                              (block (and state (agent-shell-ui--block-range :position (point))))
                              (block-start (and block (map-elt block :start))))
                         (if (and block-start
                                  (map-elt state :navigatable)
                                  (< block-start start-point))
                             block-start
                           (agent-shell-ui--previous-visible-navigatable))))))
    (when found
      (deactivate-mark)
      (goto-char found)
      found)))

(defun agent-shell-ui-make-action-keymap (action)
  "Create keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun agent-shell-ui-add-action-to-text (text action &optional on-entered face)
  "Add ACTION lambda to propertized TEXT and return modified text.
ON-ENTERED is a function to call when the cursor enters the text.
FACE when non-nil applies the specified face to the text."
  (add-text-properties 0 (length text)
                       `(keymap ,(agent-shell-ui-make-action-keymap action))
                       text)
  (when on-entered
    (add-text-properties 0 (length text)
                         (list 'cursor-sensor-functions
                               (list (lambda (_window _old-pos sensor-action)
                                       (when (eq sensor-action 'entered)
                                         (funcall on-entered)))))
                         text))
  (when face
    (add-text-properties 0 (length text)
                         `(font-lock-face ,face
                           face ,face)
                         text))
  (add-text-properties 0 (length text)
                       '(pointer hand
                         rear-nonsticky t)
                       text)
  text)

(defvar-local agent-shell-ui--isearch-opened-fragments nil
  "List of fragment qualified-ids that were opened during isearch.")

(defun agent-shell-ui--isearch-filter-predicate (beg end)
  "Custom isearch filter that expands collapsed fragments when matches are found.
BEG and END define the match region."
  ;; Check if the match contains invisible text
  (let ((pos beg)
        (found-invisible nil))
    (while (and (< pos end) (not found-invisible))
      (when (get-text-property pos 'invisible)
        (setq found-invisible t))
      (setq pos (1+ pos)))

    ;; If we found invisible text, expand the fragment
    (when found-invisible
      (save-excursion
        (goto-char beg)
        (when-let* ((state (get-text-property (point) 'agent-shell-ui-state))
                    (qualified-id (map-elt state :qualified-id))
                    ((map-elt state :collapsed)))
          ;; Track which fragments we've opened
          (unless (member qualified-id agent-shell-ui--isearch-opened-fragments)
            (push qualified-id agent-shell-ui--isearch-opened-fragments))
          ;; Expand the fragment
          (agent-shell-ui--toggle-fragment-at-point))))

    ;; Always return t to include the match
    t))

(defun agent-shell-ui--isearch-cleanup ()
  "Clean up isearch state when search ends."
  (setq agent-shell-ui--isearch-opened-fragments nil))

(defvar agent-shell-ui-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `agent-shell-ui-mode'.")

;;;###autoload
(define-minor-mode agent-shell-ui-mode
  "Minor mode for SUI block navigation."
  :lighter " SUI"
  :keymap agent-shell-ui-mode-map
  (if agent-shell-ui-mode
      (progn
        (cursor-sensor-mode 1)
        ;; Enable searching in invisible text and auto-expansion
        (setq-local search-invisible 'open-all)
        ;; Use custom filter predicate to expand fragments during search
        (setq-local isearch-filter-predicate #'agent-shell-ui--isearch-filter-predicate)
        ;; Clean up when search ends
        (add-hook 'isearch-mode-end-hook #'agent-shell-ui--isearch-cleanup nil 'local))
    (cursor-sensor-mode -1)
    (kill-local-variable 'search-invisible)
    (kill-local-variable 'isearch-filter-predicate)
    (remove-hook 'isearch-mode-end-hook #'agent-shell-ui--isearch-cleanup 'local)))

(provide 'agent-shell-ui)

;;; agent-shell-ui.el ends here
