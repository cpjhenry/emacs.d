(defun delete-duplicate-blank-lines (&optional quiet)
  "Remove duplicate blank lines in region or buffer.

When called interactively:
- no prefix → report number removed
- \\[universal-argument] → suppress message

When called from Lisp:
- QUIET non-nil suppresses message.

Returns the number of duplicate blank lines removed."
  (interactive "P")
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end)       (point-max)))
         (inhibit-read-only t)
         (before (count-lines beg end)))
    (delete-duplicate-lines beg end nil t)
    ;; Buffer may have shrunk; clamp END again before recounting.
    (let* ((end (max (point-min) (min end (point-max))))
           (after (count-lines beg end))
           (removed (- before after)))
      (unless quiet
        (message "Removed %d duplicate blank line%s."
                 removed
                 (if (= removed 1) "" "s")))
      removed)))

(defun collapse-multiple-spaces-in-region (beg end)
  "Collapse runs of 2+ literal spaces to one space in the active region.
Tabs untouched. Skips Org tables/src/fixed-width blocks.
Requires Transient Mark Mode with a visibly active region."
  (interactive "r")
  (unless (and transient-mark-mode (use-region-p))
    (user-error "No active region (Transient Mark Mode required)"))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t)
        (unless (or (and (derived-mode-p 'org-mode)
                         (or (org-at-table-p)
                             (org-in-src-block-p)
                             (org-in-fixed-width-region-p))))
          (replace-match " " t t))))))

(defun normalize-text-dwim (beg end)
  "Normalize text in region, or whole buffer if no region.

Removes leading whitespace, trailing whitespace, and collapses multiple
blank lines to a single blank line. Internal spacing within lines is left
alone."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      ;; Remove leading and trailing whitespace line by line.
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (delete-horizontal-space)
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1))

      ;; Collapse 3+ newlines to 2 newlines.
      ;; This preserves paragraph breaks, but removes blank-line bloat.
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n")))))
