;;; org-plain-latex-preview.el --- Plain-text Org export with LaTeX-aware cleanup -*- lexical-binding: t; -*-

;;; Commentary:

;; This package defines a small Org export back-end derived from the ASCII
;; back-end.  It is intended for previewing Org documents that are primarily
;; aimed at LaTeX export, while rendering selected LaTeX export directives and
;; house macros in a more readable plain-text form.
;;
;; It does not attempt to run LaTeX or fully interpret TeX.  Instead, it
;; translates common structural directives such as \smallskip and samepage
;; markers into plain-text equivalents, while dropping LaTeX header material.

;; To enable:
;; (require 'org-plain-latex-preview)

;; -OR-

;; (use-package org-plain-latex-preview
;;   :ensure nil)

;;; Code:

(require 'ox)
(require 'ox-ascii)
(require 'subr-x)

;; NOTE: This overlaps conceptually with `org-macro-displays', but serves a
;; different purpose: export output rather than buffer display.
(defvar org-plain-latex-preview-macro-alist
  '(("\\first{}"  . "1st")
    ("\\second{}" . "2nd")
    ("\\third{}"  . "3rd")
    ("\\td{}"     . "...")
    ("\\fd{}"     . ".....")
    ("\\asterism\\b" . "* * *")) ; `\b' is word boundary
  "Alist mapping LaTeX macros to plain-text replacements.")

(defun org-plain-latex-preview--replace-macros (string)
  "Replace known LaTeX macros in STRING."
  (dolist (pair org-plain-latex-preview-macro-alist string)
    (setq string
          (replace-regexp-in-string
           (regexp-quote (car pair))
           (cdr pair)
           string t t))))

(defun org-plain-latex-preview--latex-directive (value)
  "Return a plain-text rendering of LaTeX directive VALUE."
  (pcase (string-trim value)
    ("\\smallskip" "\n")
    ("\\medskip" "\n\n")
    ("\\bigskip" "\n\n")
    ("\\begin{samepage}" "")
    ("\\end{samepage}" "")
    ((or "\\clearpage" "\\newpage" "\\pagebreak") "\n\n\n\n")
    (_ "")))

(defun org-plain-latex-preview-keyword (keyword _contents _info)
  "Render Org KEYWORD for plain LaTeX-aware preview export."
  (let ((key (org-element-property :key keyword))
        (value (or (org-element-property :value keyword) "")))
    (pcase key
      ("LATEX"
       (org-plain-latex-preview--latex-directive
        (org-plain-latex-preview--replace-macros value)))
      ("LATEX_HEADER" "")
      ("LATEX_HEADER_EXTRA" "")
      ("OPTIONS" "")
      ("BIND" "")
      (_ nil))))

(defun org-plain-latex-preview-latex-fragment (fragment _contents _info)
  "Render LaTeX FRAGMENT as readable plain text."
  (org-plain-latex-preview--replace-macros
   (string-trim
    (org-element-property :value fragment))))

(defun org-plain-latex-preview-latex-environment (environment _contents _info)
  "Render LaTeX ENVIRONMENT for plain-text preview."
  (let ((value (org-element-property :value environment)))
    (cond
     ((string-match-p "\\\\begin{samepage}" value) "")
     (t ""))))

(org-export-define-derived-backend 'plain-latex-preview 'ascii
  :menu-entry
  '(?p "Plain LaTeX-aware preview"
       ((?p "To temporary buffer"
            (lambda (a s v b)
              (org-export-to-buffer 'plain-latex-preview
                  "*Org Plain LaTeX Preview*"
                a s v b nil
                (lambda () (text-mode)))))
        (?P "To file"
            (lambda (a s v b)
              (org-export-to-file 'plain-latex-preview
                  (org-export-output-file-name ".txt" s)
                a s v b)))))
  :translate-alist
  '((keyword . org-plain-latex-preview-keyword)
    (latex-fragment . org-plain-latex-preview-latex-fragment)
    (latex-environment . org-plain-latex-preview-latex-environment)))

(provide 'org-plain-latex-preview)
;;; org-plain-latex-preview.el ends here

; LocalWords:  asterism samepage clearpage newpage pagebreak
