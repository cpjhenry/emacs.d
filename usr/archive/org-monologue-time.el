;;; org-monologue-time.el --- Paragraph timing for Org -*- lexical-binding: t; -*-
;;; Commentary:

;; This package estimates delivery time for an Org-mode monologue by counting
;; Org paragraph elements and multiplying that count by a configurable number
;; of minutes per paragraph.
;;
;; It is intended for rough performance or presentation timing, not for
;; memorization planning.  For memorization workload, see the companion file
;; `org-monologue-memo.el'.
;;
;; By default, the calculation is scoped to the current top-level Org
;; heading, allowing a longer monologue to be divided into practical delivery
;; sections.  The result is displayed in the mode line as:
;;
;;   ¶14~28m
;;
;; meaning fourteen paragraphs, estimated at twenty-eight minutes of delivery
;; time.

;;; code:
(require 'org)
(require 'org-element)

(defgroup org-monologue-time nil
  "Estimate speaking time from paragraph count."
  :group 'org)

(defcustom org-monologue-time-minutes-per-paragraph 2
  "Estimated minutes per paragraph."
  :type 'number)

(defcustom org-monologue-time-scope 'top-level
  "Scope to measure.
- `top-level` = current level-1 subtree
- `subtree`   = current subtree at point
- `buffer`    = whole buffer"
  :type '(choice (const :tag "Top-level subtree" top-level)
                 (const :tag "Current subtree" subtree)
                 (const :tag "Whole buffer" buffer)))

(defcustom org-monologue-time-idle-delay 1.0
  "Seconds to wait after edits before recomputing."
  :type 'number)

(defvar-local org-monologue-time--modeline ""
  "Cached modeline string.")

(defvar-local org-monologue-time--idle-timer nil)

(defun org-monologue-time--bounds ()
  "Return (BEG . END) for the configured scope."
  (save-excursion
    (save-restriction
      (widen)
      (pcase org-monologue-time-scope
        ('buffer (cons (point-min) (point-max)))
        ((or 'subtree 'top-level)
         (if (org-before-first-heading-p)
             (cons (point-min) (point-max))
           (org-back-to-heading t)
           (when (eq org-monologue-time-scope 'top-level)
             (while (> (org-current-level) 1)
               (org-up-heading-safe)))
           (let ((beg (point)))
             (org-end-of-subtree t t)
             (cons beg (point)))))))))

(defun org-monologue-time--count-paragraphs (beg end)
  "Count Org paragraph elements between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ast (org-element-parse-buffer)))
        (length (org-element-map ast 'paragraph #'identity))))))

(defun org-monologue-time--recompute ()
  "Recompute cached modeline string."
  (when (derived-mode-p 'org-mode)
    (let* ((b (org-monologue-time--bounds))
           (p (org-monologue-time--count-paragraphs (car b) (cdr b)))
           (mins (* p org-monologue-time-minutes-per-paragraph)))
      (setq org-monologue-time--modeline
            (format " ¶%d~%dm" p mins))
      (force-mode-line-update))))

(defun org-monologue-time--schedule ()
  "Schedule recompute after idle."
  (when org-monologue-time--idle-timer
    (cancel-timer org-monologue-time--idle-timer))
  (setq org-monologue-time--idle-timer
        (run-with-idle-timer org-monologue-time-idle-delay nil
                             #'org-monologue-time--recompute)))

;;;###autoload
(define-minor-mode org-monologue-time-mode
  "Show paragraph count and estimated minutes in the mode line."
  :lighter ""
  (if org-monologue-time-mode
      (progn
        (add-to-list 'mode-line-format '(:eval org-monologue-time--modeline) t)
        (add-hook 'after-save-hook #'org-monologue-time--recompute nil t)
        (add-hook 'after-change-functions (lambda (&rest _) (org-monologue-time--schedule)) nil t)
        (org-monologue-time--recompute))
    (setq mode-line-format (delq '(:eval org-monologue-time--modeline) mode-line-format))
    (remove-hook 'after-save-hook #'org-monologue-time--recompute t)
    (remove-hook 'after-change-functions (lambda (&rest _) (org-monologue-time--schedule)) t)
    (when org-monologue-time--idle-timer
      (cancel-timer org-monologue-time--idle-timer)
      (setq org-monologue-time--idle-timer nil))
    (setq org-monologue-time--modeline "")
    (force-mode-line-update)))

(provide 'org-monologue-time)
;;; org-monologue-time.el ends here
