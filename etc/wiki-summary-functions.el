;;; wiki-summary-functions.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun cpj/wiki-summary-clean-term (term)
  "Clean TERM for use as a Wikipedia search phrase."
  (when term
    (setq term
          (replace-regexp-in-string
           "[ \t]*(.*?)[ \t]*" " " term))
    (setq term
          (replace-regexp-in-string
           "[ \t]*\"[^\"]*\"[ \t]*" " " term))
    (setq term (string-trim term))
    (setq term
          (replace-regexp-in-string
           "[ \t]+Day\\'" "" term))
    (string-trim term)))

(defun cpj/wiki-summary (&optional prompt)
  "Look up the region, agenda item, or word at point in Wikipedia.

With prefix argument PROMPT, confirm or edit the search term first."
  (interactive "P")
  (let* ((term
          (cond
           ((use-region-p)
            (buffer-substring-no-properties
             (region-beginning) (region-end)))
           ((derived-mode-p 'org-agenda-mode)
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position)))
           (t
            (thing-at-point 'word t))))
         (term (cpj/wiki-summary-clean-term term)))
    (wiki-summary
     (if prompt
         (read-string
          (concat "Wikipedia Article"
                  (if term (format " (%s)" term) "")
                  ": ")
          nil nil term)
       term))))

(defun cpj/wiki-summary-format-summary-in-buffer (summary)
  "Display SUMMARY as clean, filled plaintext."
  (let ((buf (generate-new-buffer "*wiki-summary*")))
    (with-current-buffer buf
      (text-mode)
      (insert summary)

      ;; Treat standalone colon-ended lines as paragraph headings.
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+?:\\)$" nil t)
        (let ((beg (line-beginning-position))
	      (end (line-end-position)))
          (unless (or (= beg (point-min))
		      (save-excursion
                        (goto-char beg)
                        (forward-line -1)
                        (looking-at-p "^[[:space:]]*$")))
            (goto-char beg)
            (insert "\n")
            (setq end (1+ end)))
          (goto-char end)
          (forward-line 1)
          (unless (or (eobp)
		      (looking-at-p "^[[:space:]]*$"))
            (insert "\n"))))

      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (view-mode))
    (pop-to-buffer buf)))

(provide 'wiki-summary-functions)

;;; wiki-summary-functions.el ends here
