;;; org-paragraph-preview.el --- Preview Org paragraphs as excerpts -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, latex

;;; Commentary:
;; This package creates a temporary Org buffer in which prose
;; paragraphs are reduced to short single-line previews.
;;
;; The intent is to provide a compact structural overview suitable for
;; export, review, summaries, speaking notes, archival digests, or
;; high-level editorial inspection.
;;
;; Headings and Org structure are preserved, while ordinary prose
;; paragraphs are collapsed into abbreviated excerpts ending with
;; ellipses when truncated.
;;
;; The preview buffer may also inject export-specific Org keywords,
;; such as LaTeX header includes or document directives, so that the
;; resulting buffer can be exported immediately without modifying the
;; original source document.
;;
;; The original buffer is never modified.

;;; Code:
(require 'org)

(defcustom org-paragraph-preview-latex-header nil
  "LaTeX header file included in preview export buffers."
  :type '(choice (const :tag "None" nil) file)
  :group 'org-paragraph-preview)

(defcustom org-paragraph-preview-latex-directives nil
  "List of LaTeX directives inserted into preview export buffers."
  :type '(repeat string)
  :group 'org-paragraph-preview)

(defcustom org-paragraph-preview-limit 40
  "Default maximum paragraph preview length."
  :type 'integer
  :group 'org-paragraph-preview)

(defun org-paragraph-preview--shorten (text limit)
  "Shorten TEXT to LIMIT characters, preserving the final word.
When TEXT is longer than LIMIT, keep the beginning of TEXT, insert
an ellipsis, and preserve the final word including punctuation."
  (if (<= (length text) limit)
      text
    (let* ((ellipsis " … ")
           (last-word
            (if (string-match "\\([^[:space:]]+\\)[[:space:]]*$" text)
                (match-string 1 text)
              ""))
           (available (- limit
                         (length ellipsis)
                         (length last-word)))
           (prefix
            (if (> available 0)
                (substring text 0 (min available (length text)))
              "")))
      (concat
       (string-trim-right prefix)
       ellipsis
       last-word))))

(defun org-paragraph-preview (&optional limit)
  "Create an Org export buffer with paragraphs shortened to LIMIT characters.
Headings are preserved.  Paragraphs are each reduced to one line ending
with an ellipsis when truncated."
  (interactive "P")
  (let* ((limit (or (and limit
			 (prefix-numeric-value limit))
                    org-paragraph-preview-limit))
         (text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (save-restriction
                   (org-narrow-to-subtree)
                   (buffer-substring-no-properties (point-min) (point-max))))))
    (with-current-buffer (get-buffer-create "*Org Paragraph Preview*")
      (erase-buffer)

      ;; Insert and clean source material.
      (insert (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))
      (goto-char (point-min))
      (flush-lines "^#\\+\\(LATEX\\|LATEX_HEADER\\|ATTR_\\|BEGIN_EXPORT\\|END_EXPORT\\)")

      ;; Add generated export scaffold.
      (goto-char (point-min))
      (when org-paragraph-preview-latex-header
	(insert (format "#+INCLUDE: %S\n"
			org-paragraph-preview-latex-header)))

      (when org-paragraph-preview-latex-directives
	(dolist (directive org-paragraph-preview-latex-directives)
	  (insert (format "#+LATEX: %s\n" directive))))

      (when (or org-paragraph-preview-latex-header
		org-paragraph-preview-latex-directives)
	(insert "\n"))

      ;; Now treat it as Org.
      (org-mode)
      (while (re-search-forward "^\\([^*\n#].+\\(?:\n[^*\n#].+\\)*\\)" nil t)
        (let* ((beg (match-beginning 1))
               (end (match-end 1))
               (para (replace-regexp-in-string
                      "[ \t\n]+" " "
                      (buffer-substring-no-properties beg end))))
          (delete-region beg end)
          (goto-char beg)
          ;; (insert
           ;; (if (> (length para) limit)
           ;;     (concat (substring para 0 limit) "...")
           ;;   para))
	  (insert
	   (org-paragraph-preview--shorten para limit))))

      (let ((workbuf (current-buffer))
	    (file (make-temp-file "org-paragraph-preview-" nil ".org")))
	(write-region (point-min) (point-max) file nil 'silent)
	(find-file file)
	(when (bound-and-true-p jinx-mode)
	  (jinx-mode -1))
	(kill-buffer workbuf)))))

(provide 'org-paragraph-preview)
;;; org-paragraph-preview.el ends here
