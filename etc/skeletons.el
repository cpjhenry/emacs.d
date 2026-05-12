;;; skeletons.el --- My skeletons -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: skeleton

;;; Commentary:

;;; Code:
(define-skeleton cpj/elisp-header
  "Insert a standard Emacs Lisp file header."
  "Short description: "
  ";;; "
  (file-name-nondirectory (or buffer-file-name (buffer-name)))
  " --- "
  str
  " -*- lexical-binding: t; -*-\n\n"
  ";; Author: "
  user-full-name
  (if (and user-mail-address
           (not (string= user-mail-address "")))
      (concat " <" user-mail-address ">")
    "")
  "\n"
  ";; Package-Requires: ((emacs \"29.1\"))\n"
  ";; Keywords: "
  _
  "\n\n"
  ";;; Commentary:\n\n"
  ";;; Code:")

(define-skeleton cpj/elisp-footer
  "Insert a standard Emacs Lisp footer."
  nil
  "\n(provide '"
  (file-name-base (buffer-file-name))
  ")\n\n"
  ";;; "
  (file-name-nondirectory (buffer-file-name))
  " ends here\n")

;;; skeletons.el ends here
