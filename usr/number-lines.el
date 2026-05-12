;;; number-lines.el --- Simple DWIM line numbering helpers -*- lexical-binding: t; -*-

;; Author: cpj
;; Keywords: editing, text, convenience
;;
;; This file provides lightweight utilities for adding and removing
;; ordered list markers from text regions or entire buffers.
;;
;; The routines follow a DWIM ("Do What I Mean") workflow:
;;
;;   • If a region is active, operate only on the region.
;;   • Otherwise, operate on the entire buffer.
;;
;; These commands are intended primarily for restructuring pasted
;; material, preparing outlines, cleaning copied web text, and
;; rapidly converting between plain text and ordered lists.
;;
;; The philosophy is intentionally Unix-like:
;;
;;   • perform one transformation cleanly
;;   • preserve surrounding structure where possible
;;   • compose naturally with other editing commands
;;
;; Current commands:
;;
;;   `number-lines-dwim'
;;      Add sequential numbering to lines.
;;
;;   `unnumber-lines-dwim'
;;      Remove common ordered-list markers while preserving indentation.
;;
;; Supported removable markers include:
;;
;;   1.  2.  3.
;;   1)  2)  3)
;;   a.  b.  c.
;;   A)  B)  C)
;;   iv. IV) etc.
;;
;; Designed for plain text, Org, Markdown, notes, drafts, and
;; general editing workflows.

;;; Commentary:

;; These helpers emerged from a recurring workflow of flattening and
;; restructuring copied material from browsers, PDFs, email, and
;; generated text.  Rather than relying on heavyweight outline or
;; list-management packages, these commands provide fast structural
;; transformations with minimal assumptions.

;;; Code:
(defun org-previous-numbered-list-number ()
  "Return the nearest previous Org numbered-list number in the same block, or nil.

Stops searching if a blank line is encountered."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (forward-line -1)
                  (not (bobp))
                  (not (looking-at-p "^[[:space:]]*$")))
        (when (looking-at
               "^[[:space:]]*\\([0-9]+\\)\\.\\(?:[[:space:]]\\|$\\)")
          (setq found (string-to-number (match-string 1)))))
      found)))

(defconst ordered-list-marker-regexp
  "^[ \t]*\\([0-9]+\\)[.)][ \t]+"
  "Regexp matching a leading numeric ordered-list marker.")

(defun numbered-line-p ()
  "Return non-nil if current line already has a numeric list marker."
  (looking-at ordered-list-marker-regexp))

(defun numberable-line-p ()
  "Return non-nil if current line should be numbered."
  (not (or (looking-at-p "^[ \t]*$")
           (looking-at-p "^[ \t]")
           (looking-at-p "^[ \t]*[*+-][ \t]")
           (numbered-line-p))))

(defun number-lines-dwim (beg end &optional renumber)
  "Number unindented logical lines in region, or from point to next blank line.

If region is active, number each eligible line in the region.
Otherwise, number from point until the next blank line.

Indented lines and Org unordered list items are skipped.

Already-numbered lines are ignored by default.  With prefix argument
RENUMBER, existing numeric list markers are replaced and renumbered.

In Org buffers, continue from the previous numbered list item when one
exists; otherwise start at 1."
  (interactive
   (let ((renumber current-prefix-arg))
     (if (use-region-p)
         (list (region-beginning) (region-end) renumber)
       (save-excursion
         (let ((beg (line-beginning-position))
               end)
           (while (and (not (eobp))
                       (not (looking-at-p "^[[:space:]]*$")))
             (forward-line 1))
           (setq end (point))
           (list beg end renumber))))))

  (save-excursion
    (let* ((line-count
            (save-excursion
              (goto-char beg)
              (let ((count 0))
                (while (< (point) end)
                  (when (or (numberable-line-p)
                            (and renumber
                                 (numbered-line-p)))
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
        (cond
         ((numberable-line-p)
          (insert (format fmt n))
          (setq n (1+ n)))

         ((and renumber
               (numbered-line-p))
          (replace-match (format fmt n))
          (setq n (1+ n))))
        (forward-line 1))

      (set-marker end-marker nil))))

(defun unnumber-lines-dwim ()
  "Remove leading ordered-list markers.

If region is active, operate on the region.
Otherwise, operate on the entire buffer.

Handles decimal, alphabetic, and Roman numeral markers, using
either a period or closing parenthesis:

  1. item
  1) item
  a. item
  A) item
  iv. item
  IV) item

Indentation is preserved."
  (interactive)
  (save-excursion
    (save-restriction
      (if (use-region-p)
          (narrow-to-region (region-beginning) (region-end))
        (widen))
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([[:space:]]*\\)\\(?:[0-9]+\\|[A-Za-z]\\|[ivxlcdmIVXLCDM]+\\)[.)][[:space:]]+"
              nil t)
        (replace-match "\\1")))))

(provide 'number-lines)
;;; number-lines.el ends here
