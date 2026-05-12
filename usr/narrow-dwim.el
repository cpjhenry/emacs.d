;;; narrow-dwim.el --- Intelligent narrowing and widening commands -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides a single DWIM ("Do What I Mean") command for
;; narrowing and widening buffers.
;;
;; Rather than requiring separate commands for regions, Org subtrees,
;; source blocks, pages, or defuns, `narrow-or-widen-dwim' attempts to
;; determine the most appropriate scope automatically.
;;
;; Behaviour is context-sensitive:
;;
;; - If the buffer is narrowed, widen it.
;; - If a region is active, narrow to the region.
;; - In Org buffers:
;;   - edit source blocks with `org-edit-src-code'
;;   - narrow to blocks when appropriate
;;   - otherwise narrow to the current subtree
;; - With a negative prefix argument, narrow to page.
;; - Otherwise narrow to the current defun.
;;
;; The goal is to make narrowing a natural part of everyday editing,
;; reducing the need to remember multiple specialized narrowing
;; commands.
;;
;; Typical usage:
;;
;;   (keymap-set global-map "C-c x n" #'narrow-dwim)
;;
;; Inspired by the well-known narrowing helper popularized by
;; Endless Parentheses and adapted through community refinements.

;;; Code:
(require 'org)

(defun narrow-dwim (arg)
  "Widen, or narrow intelligently.

Without prefix ARG, widen if the buffer is narrowed.  Otherwise
narrow to the active region, Org source block, Org block, Org
subtree, page, or defun, as appropriate.

With prefix ARG, narrow even if the buffer is already narrowed.
With a negative prefix ARG, narrow to page."
  (interactive "P")
  (cond
   ((and (buffer-narrowed-p) (not arg))
    (widen))

   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end)))

   ((derived-mode-p 'org-mode)
    (cond
     ((ignore-errors (org-edit-src-code) t))
     ((ignore-errors (org-narrow-to-block) t))
     (t (org-narrow-to-subtree))))

   ((eq arg '-)
    (narrow-to-page))

   (t
    (narrow-to-defun))))

(provide 'narrow-dwim)
;;; narrow-dwim.el ends here
