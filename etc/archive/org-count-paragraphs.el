(defun org-count-paragraphs-in-region (beg end)
  "Count Org \='paragraph' elements between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ast (org-element-parse-buffer)))
        (length (org-element-map ast 'paragraph #'identity))))))

(defun org-count-paragraphs ()
  "Report number of Org paragraphs in the current top-level heading.

If point is not in a heading, count in the whole buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (save-excursion
    (save-restriction
      (widen)
      (let (beg end scope-label)
        (if (org-before-first-heading-p)
            (setq beg (point-min)
                  end (point-max)
                  scope-label "Buffer")
          ;; Find current top-level (level 1) ancestor, then narrow to its subtree.
          (org-back-to-heading t)
          (while (> (org-current-level) 1)
            (org-up-heading-safe))
          (setq scope-label (format "%s" (org-get-heading t t t t)))
          (setq beg (point))
          (org-end-of-subtree t t)
          (setq end (point)))
        (let ((n (org-count-paragraphs-in-region beg end)))
          (message "%s has %d paragraph%s" scope-label n (if (= n 1) "" "s")))))))
