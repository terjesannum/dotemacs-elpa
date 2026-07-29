;;; agent-shell-work-buffer.el --- Reusable work buffer helper -*- lexical-binding: t; -*-

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
;; Provides `agent-shell-with-work-buffer', a thin wrapper that uses
;; `with-work-buffer' (Emacs 31, which reuses a pooled temporary buffer)
;; when available and otherwise falls back to `with-temp-buffer'.
;;
;; The choice is made at macro-expansion (byte-compile) time.  This
;; matters because `with-work-buffer' is a macro: if it is merely absent
;; when a caller is byte-compiled, a direct use would compile to an
;; ordinary function call and fail at runtime with
;; "invalid-function with-work-buffer".  Going through this wrapper keeps
;; older Emacsen working via `with-temp-buffer', with no dependency on
;; `compat' to backport the macro.

;;; Code:

(defmacro agent-shell-with-work-buffer (&rest body)
  "Evaluate BODY in a temporary buffer, reusing one when possible.
Expands to `with-work-buffer' when that macro is available at expansion
time (Emacs 31 and later), otherwise to `with-temp-buffer'."
  (declare (indent 0) (debug t))
  (if (fboundp 'with-work-buffer)
      `(with-work-buffer ,@body)
    `(with-temp-buffer ,@body)))

(provide 'agent-shell-work-buffer)
;;; agent-shell-work-buffer.el ends here
