;;; section-nav-mode.el --- Navigate and narrow delimiter-based sections -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode for working with text sections delimited by the
;; group separator character (^], ASCII 29, hex \x1D).
;;
;; Provides commands to:
;; - move to the next section
;; - move to the previous section
;; - narrow to the current section
;;
;; A "section" is the text between two delimiters, or between
;; `point-min' / `point-max' and the nearest delimiter.

;;; Recommended keybindings, if you prefer to bind them globally instead:
;;
;; (define-key global-map (kbd "M-s ]") #'section-nav-next-section)
;; (define-key global-map (kbd "M-s [") #'section-nav-previous-section)
;; (define-key global-map (kbd "M-s n") #'section-nav-narrow-to-current-section)

;;; Code:

(defgroup section-nav nil
  "Navigation and narrowing for delimiter-based text sections."
  :group 'convenience)

(defcustom section-nav-separator "\x1D"
  "Separator string used to delimit sections."
  :type 'string
  :group 'section-nav)

(defun section-nav--section-start (&optional pos)
  "Return the start position of the section containing POS.
If there is no previous separator, return `point-min'."
  (save-excursion
    (when pos
      (goto-char pos))
    (or (when (search-backward section-nav-separator nil t)
          (1+ (point)))
        (point-min))))

(defun section-nav--section-end (&optional pos)
  "Return the end position of the section containing POS.
If there is no next separator, return `point-max'."
  (save-excursion
    (when pos
      (goto-char pos))
    (or (when (search-forward section-nav-separator nil t)
          (1- (point)))
        (point-max))))

(defun section-nav-narrow-to-current-section ()
  "Narrow buffer to the current section.

The current section is the text between the previous separator and
the next separator. If there is no previous separator, use
`point-min'. If there is no next separator, use `point-max'."
  (interactive)
  (let ((pos (point))
        start
        end)
    (save-excursion
      (setq start (section-nav--section-start pos))
      (setq end   (section-nav--section-end pos)))
    (narrow-to-region start end)))

(defun section-nav-next-section ()
  "Move point to the beginning of the next section.

If no next separator is found, leave point unchanged and report it."
  (interactive)
  (let ((origin (point)))
    (if (search-forward section-nav-separator nil t)
        (goto-char (point))
      (goto-char origin)
      (user-error "No next section"))))

(defun section-nav-previous-section ()
  "Move point to the beginning of the previous section.

If point is inside a section, move to the beginning of the previous
section, not merely to the current section's start. If no previous
section exists, leave point unchanged and report it."
  (interactive)
  (let* ((pos (point))
         (current-start (section-nav--section-start pos))
         (target
          (save-excursion
            (goto-char current-start)
            (when (> current-start (point-min))
              ;; Step back onto the preceding separator, then find the
              ;; start of the section before that.
              (goto-char (1- current-start))
              (section-nav--section-start)))))
    (if target
        (goto-char target)
      (user-error "No previous section"))))

;;;###autoload
(define-minor-mode section-nav-mode
  "Minor mode for navigating and narrowing delimiter-based sections.

Sections are delimited by `section-nav-separator', which defaults
to the group separator character (^])."
  :lighter " SecNav"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-s ]") #'section-nav-next-section)
    (define-key map (kbd "M-s [") #'section-nav-previous-section)
    (define-key map (kbd "M-s n") #'section-nav-narrow-to-current-section)
    map))

(provide 'section-nav-mode)
;;; section-nav-mode.el ends here
