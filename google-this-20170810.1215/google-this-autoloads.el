;;; google-this-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from google-this.el

(autoload 'google-this-search "google-this" "\
Write and do a google search.
Interactively PREFIX determines quoting.
Non-interactively SEARCH-STRING is the string to search.

(fn PREFIX &optional SEARCH-STRING)" t)
(autoload 'google-this-lucky-and-insert-url "google-this" "\
Fetch the url that would be visited by `google-this-lucky'.

If you just want to do an \"I'm feeling lucky search\", use
`google-this-lucky-search' instead.

Interactively:
* Insert the URL at point,
* Kill the searched term, removing it from the buffer (it is killed, not
  deleted, so it can be easily yanked back if desired).
* Search term defaults to region or line, and always queries for
  confirmation.

Non-Interactively:
* Runs synchronously,
* Search TERM is an argument without confirmation,
* Only insert if INSERT is non-nil, otherwise return.

(fn TERM &optional INSERT)" t)
(autoload 'google-this-lucky-search "google-this" "\
Exactly like `google-this-search', but use the \"I'm feeling lucky\" option.
PREFIX determines quoting.

(fn PREFIX)" t)
(autoload 'google-this-string "google-this" "\
Google given TEXT, but ask the user first if NOCONFIRM is nil.
PREFIX determines quoting.

(fn PREFIX &optional TEXT NOCONFIRM)")
(autoload 'google-this-line "google-this" "\
Google the current line.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t)
(autoload 'google-this-ray "google-this" "\
Google text between the point and end of the line.
If there is a selected region, googles the region.
PREFIX determines quoting. Negative arguments invert the line segment.
NOCONFIRM goes without asking for confirmation.
NOREGION ignores the region.

(fn PREFIX &optional NOCONFIRM NOREGION)" t)
(autoload 'google-this-word "google-this" "\
Google the current word.
PREFIX determines quoting.

(fn PREFIX)" t)
(autoload 'google-this-symbol "google-this" "\
Google the current symbol.
PREFIX determines quoting.

(fn PREFIX)" t)
(autoload 'google-this-region "google-this" "\
Google the current region.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t)
(autoload 'google-this "google-this" "\
Decide what the user wants to google (always something under point).
Unlike `google-this-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited.
PREFIX argument determines quoting.
NOCONFIRM goes without asking for confirmation.

(fn PREFIX &optional NOCONFIRM)" t)
(autoload 'google-this-noconfirm "google-this" "\
Decide what the user wants to google and go without confirmation.
Exactly like `google-this' or `google-this-search', but don't ask
for confirmation.
PREFIX determines quoting.

(fn PREFIX)" t)
(autoload 'google-this-error "google-this" "\
Google the current error in the compilation buffer.
PREFIX determines quoting.

(fn PREFIX)" t)
(autoload 'google-this-clean-error-string "google-this" "\
Parse error string S and turn it into googleable strings.

Removes unhelpful details like file names and line numbers from
simple error strings (such as c-like erros).

Uses replacements in `google-this-error-regexp' and stops at the first match.

(fn S)" t)
(autoload 'google-this-cpp-reference "google-this" "\
Visit the most probable cppreference.com page for this word." t)
(autoload 'google-this-forecast "google-this" "\
Search google for \"weather\".
With PREFIX, ask for location.

(fn PREFIX)" t)
(defvar google-this-mode nil "\
Non-nil if Google-This mode is enabled.
See the `google-this-mode' command
for a description of this minor mode.")
(custom-autoload 'google-this-mode "google-this" nil)
(autoload 'google-this-mode "google-this" "\
Toggle Google-This mode on or off.

This is a global minor mode.  If called interactively, toggle the
`Google-This mode' mode.  If the prefix argument is positive, enable the
mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='google-this-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "google-this" '("google-this-"))

;;; End of scraped data

(provide 'google-this-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; google-this-autoloads.el ends here
