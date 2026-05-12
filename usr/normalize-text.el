;;; normalize-text.el --- DWIM text normalization helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides routines for normalizing pasted or imported
;; text while attempting to preserve intentional structure.
;;
;; The primary entry point is `normalize-text-dwim', which operates on
;; the active region if one exists, otherwise on the entire buffer.
;;
;; The normalization process is intentionally conservative:
;;
;; - Removes leading whitespace at the beginning of lines.
;; - Removes trailing whitespace at the ends of lines.
;; - Collapses runs of multiple literal spaces into a single space.
;; - Preserves tabs.
;; - Collapses excessive blank lines while preserving paragraph breaks.
;;
;; In Org buffers, additional safeguards prevent destructive cleanup
;; inside structures where spacing is semantically meaningful:
;;
;; - Org tables
;; - Source blocks
;; - Fixed-width regions
;;
;; The package is intended primarily for cleaning text pasted from web
;; browsers, PDFs, email, generated content, or other external sources
;; before restructuring or editing.
;;
;; Philosophy:
;;
;; The goal is not typographic perfection, but practical normalization:
;; reduce noise, preserve meaning, and avoid damaging intentional
;; formatting.
;;
;; This package reflects a "do what I mean" editing style, favouring
;; safe cleanup operations over aggressive reformatting.

;;; Code:

(defun normalize-text-dwim (beg end)
  "Normalize text in region, or whole buffer if no region.

Removes leading whitespace, trailing whitespace, collapses runs of
2+ literal spaces to one space, and collapses multiple blank lines
to a single blank line.

Tabs are left untouched.  In Org buffers, internal space-collapsing
is skipped in tables, src blocks, and fixed-width regions."
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

      ;; Collapse runs of 2+ literal spaces to one space.
      ;; Tabs are untouched.  Org tables/src/fixed-width regions are skipped.
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t)
        (unless (and (derived-mode-p 'org-mode)
                     (or (org-at-table-p)
                         (org-in-src-block-p)
                         (org-in-fixed-width-region-p)))
          (replace-match " " t t)))

      ;; Collapse 3+ newlines to 2 newlines.
      ;; This preserves paragraph breaks, but removes blank-line bloat.
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n")))))

(provide 'normalize-text)
;;; normalize-text.el ends here
