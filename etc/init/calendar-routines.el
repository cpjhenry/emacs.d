;;; calendar --- settings / functions
;;; Commentary:

;;; Code:
(require 'calendar)

(defun calendar-exit-kill () "Kill Calendar when exiting."
	(interactive)
	(calendar-exit 'kill)
	(let ((buffer "*wclock*"))(and (get-buffer buffer) (kill-buffer buffer))))

(defun save-diary-before-calendar-exit (_)
	(let ((diary-buffer (get-file-buffer diary-file))) (or (not diary-buffer)
		(not (buffer-modified-p diary-buffer))
		(with-current-buffer diary-buffer (save-buffer)))))

(defun display-current-date-and-time () "Display current date and time."
	(interactive)
	(message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun list-holidays-this-year () "Display holidays for displayed (or current) year."
	(interactive)
	(if (boundp 'displayed-year) (list-holidays displayed-year)
	(list-holidays (string-to-number (format-time-string "%Y")))))

(defun calendar-world-clock () "Display a world clock buffer with times in various time zones."
	(interactive)
	(world-clock)
	(next-window-any-frame)
	(fit-window-to-buffer))

;; https://www.emacswiki.org/emacs/DiaryMode
(defun alt-clean-equal-signs () "Make lines of = signs invisible."
	(goto-char (point-min))
	(let ((state buffer-read-only))
		(when state (setq buffer-read-only nil))
		(while (not (eobp)) (search-forward-regexp "^=+$" nil 'move)
		(add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
		(when state (setq buffer-read-only t))))

;; https://tlestang.github.io/blog/customizing-calendar.html
(defun is-weekend (date)
  "Evaluate to t or nil whether of not DATE falls in a weekend.
DATE is a list of the form (MONTH DAY YEAR)"
  (or (equal (calendar-day-of-week date) 6)
      (equal (calendar-day-of-week date) 0)))

(defun add-one-day (date)
  "Evaluate to (MONTH DAY YEAR) for the day following DATE. DATE should also
be provided in the (MONTH DAY YEAR) format."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date) 1)))

(defun find-next-weekday (date)
  "Evaluate to (MONTH DAY YEAR) for the first next weekday after or including DATE.

If DATE corresponds to a Saturday or Sunday, then evaluates to
following Monday.  If DATE is a weekday, then evaluates to DATE.

This function is useful to compute bank holidays in the United
Kingdom, which are pushed to first following non-bank holiday
weekday (usually Monday) if the bank holiday falls inside a
weekend. Caveat: do not use function to compute the substitute
day for a Sunday bank holiday that follows a Saturday bank
holiday (e.g. boxing day on a Sunday) or the substitute day for a
Saturday bank holiday t hat precedes a Monday bank
holiday (e.g. Christmas day on a Saturday). See functions
`set-boxing-day` and `set-christmas-day` for dealing with these
special cases."
  (let ((next-day-date
         (add-one-day date)))
    (if (is-weekend date)
        (find-next-weekday next-day-date)
      date)))

;; Add sunrise/sunset times to the agenda
;; https://orgmode.org/worg/org-hacks.html
;; You also need to add a couple of diary s-expressions in one of your agenda files:

;;	%%(diary-sunrise) Sunrise in %s
;;	%%(diary-sunset)

;; This will show sunrise with the location and sunset without it.

(defun diary-sunrise ()
  "Local time of sunrise as a diary entry.
The diary entry can contain `%s' which will be replaced with
`calendar-location-name'."
  (let ((l (solar-sunrise-sunset date)))
    (when (car l)
      (concat
       (if (string= entry "")
           "Sunrise"
         (format entry (eval calendar-location-name))) " "
         (solar-time-string (caar l) nil)))))

(defun diary-sunset ()
  "Local time of sunset as a diary entry.
The diary entry can contain `%s' which will be replaced with
`calendar-location-name'."
  (let ((l (solar-sunrise-sunset date)))
    (when (cadr l)
      (concat
       (if (string= entry "")
           "Sunset"
         (format entry (eval calendar-location-name))) " "
         (solar-time-string (caadr l) nil)))))

(defun diary-lunar-phase-symbol-only (&optional mark)
  "Moon phases diary entry.
An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  ;; This function is designed to be used in sexp diary entries, and
  ;; may be present in users' diary files, so suppress the warning
  ;; about this prefix-less dynamic variable.  It's called from
  ;; `diary-list-sexp-entries', which binds the variable.
  (with-suppressed-warnings ((lexical date))
    (defvar date))
  (let* ((index (lunar-index date))
         (phase (lunar-phase index)))
    (while (calendar-date-compare phase (list date))
      (setq index (1+ index)
            phase (lunar-phase index)))
    (and (calendar-date-equal (car phase) date)
         (cons mark
               (let ((eclipse (nth 3 phase)))
                 (lunar-phase-name (nth 2 phase)))))))

(provide 'calendar-routines)
;;; calendar-routines.el ends here.

;; Local Variables:
;; truncate-lines: -1
;; End:
