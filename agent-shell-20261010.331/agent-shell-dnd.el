;;; agent-shell-dnd.el --- Drag and drop support for agent-shell. -*- lexical-binding: t; -*-

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
;; Attaches files dropped on shell and viewport buffers to the prompt,
;; and images yanked into them with `yank-media'.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'dnd)
(require 'subr-x)
(require 'agent-shell-project)
(require 'agent-shell-prompt-queue)

(declare-function agent-shell--dot-subdir "agent-shell")
(declare-function agent-shell--get-files-context "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell-insert "agent-shell")

(defun agent-shell--dnd-handle-file-url (urls _action)
  "Attach the local files behind dropped URLS as file context.

A `dnd-protocol-alist' handler: files dropped on the buffer attach to
the prompt the way `agent-shell-yank-dwim' attaches a clipboard image,
instead of opening in the window.  URLS is the list of file: URLs in
the drop, or a single URL on Emacs 29, which calls handlers once per
file.  ACTION is ignored.  Returns `private', the action Emacs expects
a handler to report.

Every file is checked before any is copied, so a drop that includes an
unreadable file or a directory attaches nothing rather than part of the
selection.  A drop made mid-turn attaches to the live prompt when
`agent-shell-persistent-prompt-enabled' keeps one there.  Without one, it
is queued the way `agent-shell-send-region' queues its region, whether it
lands on the shell or on a viewport buffer.

For example, dropping \"file:///tmp/diagram.png\" and
\"file:///tmp/notes.txt\" together inserts \"@/tmp/diagram.png\" with
an image preview and \"@/tmp/notes.txt\"."
  (let* ((files (mapcar #'agent-shell--dnd-local-file (ensure-list urls)))
         (shell-buffer (agent-shell--shell-buffer))
         (agent-cwd (with-current-buffer shell-buffer
                      (agent-shell-cwd)))
         (text (agent-shell--get-files-context
                ;; Copy in the shell's project, the one agent-cwd came from,
                ;; not the project of whichever buffer took the drop.
                :files (with-current-buffer shell-buffer
                         (mapcar (lambda (file)
                                   (agent-shell--dnd-keep-file file agent-cwd))
                                 files))
                :agent-cwd agent-cwd)))
    (if (agent-shell--can-insert-into-prompt-p :shell-buffer shell-buffer)
        (agent-shell-insert :text text :shell-buffer shell-buffer)
      (with-current-buffer shell-buffer
        (agent-shell-prompt-queue
         (agent-shell--prompt-queue-read :initial (concat text "\n\n")))))
    'private))

;; Emacs 30+ reads the dnd-multiple-handler property off the handler's
;; symbol before each drop.  When it is non-nil,
;; `dnd-handle-multiple-urls' calls the handler just once, passing every
;; URL in the drop together, so several files become one insertion and
;; one busy check.  Emacs 29 never reads this property, so setting it
;; has no effect there: the handler is called per URL with a single
;; string, which the ensure-list above absorbs.
(put 'agent-shell--dnd-handle-file-url 'dnd-multiple-handler t)

(defun agent-shell--dnd-local-file (url)
  "Return the readable regular file behind file URL, or signal a `user-error'.

For example, \"file:///tmp/my%20notes.txt\" => \"/tmp/my notes.txt\"."
  (let ((file (dnd-get-local-file-name url t)))
    (unless file
      (user-error "Cannot read %s" url))
    (unless (file-regular-p file)
      (user-error "Cannot attach %s: not a regular file" file))
    file))

(defun agent-shell--dnd-keep-file (file &optional project-dir)
  "Return FILE, or a copy of it that outlives the drop when FILE is transient.

A file promised by the drag source rather than dragged from disk (a
screenshot thumbnail, an image dragged out of a browser) is written
under the variable `temporary-file-directory' for the duration of the
drag and deleted once it ends, so it is copied into the screenshots
directory first.  Any other file is returned as is, and so is a file
under PROJECT-DIR: a project that itself lives in the temporary
directory (see `agent-shell-new-temp-shell') drags its own files,
which are not going anywhere.

For example:

  \"/var/folders/.../T/TemporaryItems/NSIRD_screencaptureui_x/Shot.png\"
  => \"<project>/.agent-shell/screenshots/dropped-20260913-081512-Ab3xK9.png\"

  \"/home/user/design.png\"
  => \"/home/user/design.png\""
  (if (and (file-in-directory-p file temporary-file-directory)
           (not (and project-dir (file-in-directory-p file project-dir))))
      (let ((copy (make-temp-file
                   (expand-file-name (format-time-string "dropped-%Y%m%d-%H%M%S-")
                                     (agent-shell--dot-subdir "screenshots"))
                   nil
                   (file-name-extension file t))))
        (copy-file file copy t)
        copy)
    file))

(defun agent-shell--enable-dnd ()
  "Route files dropped on the current buffer to `agent-shell--dnd-handle-file-url'.

Buffer-local, so drops on any other buffer keep Emacs's default handling.
Only the local file: forms Emacs itself routes to `dnd-open-local-file'
are taken; a \"file://HOST/...\" URL still reaches `dnd-open-file'.
Does nothing on a buffer already routed, so re-entering a mode does not
add a second set of entries."
  (unless (rassq #'agent-shell--dnd-handle-file-url dnd-protocol-alist)
    (setq-local dnd-protocol-alist
                (append (mapcar (lambda (pattern)
                                  (cons pattern #'agent-shell--dnd-handle-file-url))
                                '("^file:///" "^file:/[^/]" "^file:[^/]"))
                        dnd-protocol-alist))))

(defun agent-shell--yank-media-image (type data)
  "Attach clipboard image DATA of MIME TYPE to the prompt.

A `yank-media' handler: Emacs reads the image off the clipboard itself,
so no external utility from `agent-shell-clipboard-image-handlers' is
needed.  DATA is saved to the screenshots directory and inserted as file
context, or queued when the shell is busy with no live prompt, the way
`agent-shell--dnd-handle-file-url' attaches a dropped file.

For example, yanking an image/png inserts
\"@.agent-shell/screenshots/clipboard-20260928-101512-Ab3xK9.png\" with
an image preview."
  (let* ((shell-buffer (agent-shell--shell-buffer))
         (text (agent-shell--get-files-context
                :files (with-current-buffer shell-buffer
                         (let ((coding-system-for-write 'no-conversion))
                           (list (make-temp-file
                                  (expand-file-name (format-time-string "clipboard-%Y%m%d-%H%M%S-")
                                                    (agent-shell--dot-subdir "screenshots"))
                                  nil
                                  (concat "." (agent-shell--yank-media-image-extension type))
                                  data))))
                :agent-cwd (with-current-buffer shell-buffer
                             (agent-shell-cwd)))))
    (if (agent-shell--can-insert-into-prompt-p :shell-buffer shell-buffer)
        (agent-shell-insert :text text :shell-buffer shell-buffer)
      (with-current-buffer shell-buffer
        (agent-shell-prompt-queue
         (agent-shell--prompt-queue-read :initial (concat text "\n\n")))))))

(defun agent-shell--yank-media-image-extension (type)
  "Return the file extension for image MIME TYPE, without the dot.

For example:

  image/png     => \"png\"
  image/svg+xml => \"svg\""
  (let ((subtype (cadr (split-string (symbol-name type) "/"))))
    (if (equal subtype "svg+xml")
        "svg"
      subtype)))

(provide 'agent-shell-dnd)

;;; agent-shell-dnd.el ends here
