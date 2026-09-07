;;; year-calendar.el --- Rolling twelve-month calendar  -*- lexical-binding: t; -*-

;;; Commentary:
;; Display a rolling twelve-month calendar in a dedicated buffer.
;;
;; The current month appears in the second position, with the previous
;; month first and the following ten months after it.
;;
;; The display borrows Emacs Calendar's month generator and faces, but
;; uses `special-mode' rather than `calendar-mode' so that its 4-by-3
;; layout does not interfere with the ordinary Calendar buffer or its
;; navigation state.
;;
;; Keys:
;;
;;   <  Move backward one month.
;;   >  Move forward one month.
;;   .  Return to the current month, in the second position.

;;; Code:

(require 'calendar)

(defconst year-calendar-buffer "*Year Calendar*"
  "Name of the buffer used by `year-calendar'.")

(defvar-local year-calendar-month nil
  "First month displayed in the yearly calendar.")

(defvar-local year-calendar-year nil
  "Year of the first month displayed in the yearly calendar.")

(defun year-calendar--month-width ()
  "Return the display width of one Calendar month."
  (+ (* 7 calendar-column-width)
     calendar-intermonth-spacing))

(defun year-calendar--generate ()
  "Generate the yearly calendar from the current buffer state."
  (let ((inhibit-read-only t)
        (month year-calendar-month)
        (year year-calendar-year)
        (month-width (year-calendar--month-width)))
    (erase-buffer)
    (dotimes (_row 4)
      (let ((row-start (point-max)))
        (save-restriction
          (narrow-to-region row-start row-start)
          (dotimes (column 3)
            (calendar-generate-month
             month year
             (+ calendar-left-margin (* month-width column)))
            (calendar-increment-month month year 1))))
      (goto-char (point-max))
      (insert "\n\n"))
    (delete-char -2)
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun year-calendar-current-month ()
  "Display the current month in the second position."
  (interactive)
  (pcase-let ((`(,month ,_day ,year) (calendar-current-date)))
    (calendar-increment-month month year -1)
    (setq year-calendar-month month
          year-calendar-year year)
    (year-calendar--generate)))

(defun year-calendar-forward (&optional arg)
  "Move the yearly calendar forward ARG months."
  (interactive "p")
  (calendar-increment-month year-calendar-month year-calendar-year
                            (or arg 1))
  (year-calendar--generate))

(defun year-calendar-backward (&optional arg)
  "Move the yearly calendar backward ARG months."
  (interactive "p")
  (year-calendar-forward (- (or arg 1))))

(defvar-keymap year-calendar-mode-map
  :doc "Keymap for `year-calendar-mode'."
  :parent special-mode-map
  "<" #'year-calendar-backward
  ">" #'year-calendar-forward
  "." #'year-calendar-current-month
  "y" #'year-calendar-start-of-year)

(define-derived-mode year-calendar-mode special-mode "Year Calendar"
  "Major mode for displaying a rolling twelve-month calendar."
  (setq-local truncate-lines t))

;;;###autoload
(defun year-calendar ()
  "Display a rolling twelve-month calendar.

The previous month is displayed first, placing the current month in
the second position."
  (interactive)
  (pop-to-buffer (get-buffer-create year-calendar-buffer))
  (unless (derived-mode-p 'year-calendar-mode)
    (year-calendar-mode))
  (year-calendar-current-month))

(defun year-calendar-start-of-year ()
  "Display January through December of the currently displayed year."
  (interactive)
  (setq year-calendar-month 1)
  (year-calendar--generate))

(provide 'year-calendar)

;;; year-calendar.el ends here
