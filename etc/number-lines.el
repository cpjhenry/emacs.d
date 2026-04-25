;;; number-lines.el --- Number unindented logical lines
;;; commentary:

;;; code:
(defun org-previous-numbered-list-number ()
  "Return the nearest previous Org numbered-list number in the same block, or nil.

Stops searching if a blank line is encountered."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (forward-line -1)              ;; move up one line
                  (not (bobp))
                  (not (looking-at-p "^[[:space:]]*$")))  ;; stop at blank line
        (when (looking-at
               "^[[:space:]]*\\([0-9]+\\)\\.\\(?:[[:space:]]\\|$\\)")
          (setq found (string-to-number (match-string 1)))))
      found)))

(defun number-lines-dwim (beg end)
  "Number unindented logical lines in region, or from point to next blank line.

If a region is active, number each unindented nonblank line in the region.
Otherwise, number from point until the next blank line.

Indented lines are skipped and do not consume a number.

In Org buffers, continue from the previous numbered list item when one
exists; otherwise start at 1."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let ((beg (line-beginning-position))
             end)
         (while (and (not (eobp))
                     (not (looking-at-p "^[[:space:]]*$")))
           (forward-line 1))
         (setq end (point))
         (list beg end)))))

  (save-excursion
    (let* ((line-count
            (save-excursion
              (goto-char beg)
              (let ((count 0))
                (while (< (point) end)
                  (unless (or (looking-at-p "^[[:space:]]*$")
                              (looking-at-p "^[[:space:]]"))
                    (setq count (1+ count)))
                  (forward-line 1))
                count)))
           (start-number
            (if (derived-mode-p 'org-mode)
                (1+ (or (save-excursion
                          (goto-char beg)
                          (org-previous-numbered-list-number))
                        0))
              1))
           (last-number (+ start-number line-count -1))
           (width (length (number-to-string last-number)))
           (fmt (format "%%%dd. " width))
           (n start-number)
           (end-marker (copy-marker end t)))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-marker)
        (unless (or (looking-at-p "^[[:space:]]*$")
                    (looking-at-p "^[[:space:]]"))
          (insert (format fmt n))
          (setq n (1+ n)))
        (forward-line 1))
      (set-marker end-marker nil))))

;;; number-lines.el ends here
