;; https://emacs.stackexchange.com/questions/45584/count-the-number-of-paragraphs
(defun count-paragraphs (start end)
  "Return number of paragraphs between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (- (buffer-size) (forward-paragraph (buffer-size))))))

(defun count-paragraphs-region-or-buffer ()
  "Report number of paragraphs in the region (if it's active) or the entire buffer."
  (declare (interactive-only count-paragraphs))
  (interactive)
  (let ((paragraphs (if (use-region-p)
                        (count-paragraphs (region-beginning) (region-end))
                        (count-paragraphs (point-min) (point-max)))))
    (message "%s has %d paragraph%s"
             (if (use-region-p) "Region" "Buffer")
             paragraphs
             (if (> paragraphs 1) "s" ""))))
