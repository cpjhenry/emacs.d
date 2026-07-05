;;; hanke-henry-calendar.el --- Hanke-Henry Permanent Calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert dates between the Gregorian calendar and the modern,
;; Monday-based Hanke-Henry Permanent Calendar.
;;
;; The Hanke-Henry calendar consists of four identical 91-day quarters.
;; In each quarter, the first two months have 30 days and the third has
;; 31 days.  An additional seven-day mini-month, Xtr, follows December
;; in years corresponding to Gregorian years that begin or end on a
;; Thursday.
;;
;; A Hanke-Henry date is represented as:
;;
;;     (MONTH DAY YEAR)
;;
;; MONTH is an integer from 1 through 12, or the symbol `xtr'.
;;
;; The modern Hanke-Henry year is equivalent to an ISO week-numbering
;; year: its first day is the Monday of the week containing January 4,
;; and Xtr is ISO week 53.
;;
;; Examples:
;;
;;     (hanke-henry-calendar-from-gregorian '(6 21 2026))
;;     => (6 24 2026)
;;
;;     (hanke-henry-calendar-to-gregorian '(xtr 1 2026))
;;     => (12 28 2026)

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup hanke-henry-calendar nil
  "The Hanke-Henry Permanent Calendar."
  :group 'calendar)

(defconst hanke-henry-calendar-month-names
  ["January" "February" "March"
   "April" "May" "June"
   "July" "August" "September"
   "October" "November" "December"]
  "Names of the twelve regular Hanke-Henry months.")

(defconst hanke-henry-calendar--month-offsets
  [0 30 60 91 121 151 182 212 242 273 303 333]
  "Zero-based day offsets of regular months within the year.")

(defun hanke-henry-calendar-leap-year-p (year)
  "Return non-nil when Hanke-Henry YEAR contains Xtr.

A year contains Xtr when the corresponding Gregorian year begins
or ends on a Thursday."
  (or (= 4 (calendar-day-of-week (list 1 1 year)))
      (= 4 (calendar-day-of-week (list 12 31 year)))))

(defun hanke-henry-calendar--year-start (year)
  "Return the absolute date of Hanke-Henry New Year's Day in YEAR.

This is the Monday of the Gregorian week containing January 4."
  (let* ((january-4
          (calendar-absolute-from-gregorian (list 1 4 year)))
         (day-of-week
          (calendar-day-of-week (list 1 4 year))))
    (- january-4 (mod (1- day-of-week) 7))))

(defun hanke-henry-calendar--year-and-offset (absolute)
  "Return the Hanke-Henry year and zero-based day for ABSOLUTE.

The return value is (YEAR OFFSET)."
  (let* ((gregorian-year
          (nth 2 (calendar-gregorian-from-absolute absolute)))
         (this-start
          (hanke-henry-calendar--year-start gregorian-year))
         (next-start
          (hanke-henry-calendar--year-start (1+ gregorian-year)))
         year
         start)
    (cond
     ((< absolute this-start)
      (setq year (1- gregorian-year)
            start (hanke-henry-calendar--year-start year)))
     ((>= absolute next-start)
      (setq year (1+ gregorian-year)
            start next-start))
     (t
      (setq year gregorian-year
            start this-start)))
    (list year (- absolute start))))

(defun hanke-henry-calendar--month-length (month)
  "Return the number of days in regular Hanke-Henry MONTH."
  (if (= 0 (mod month 3)) 31 30))

(defun hanke-henry-calendar-valid-date-p (date)
  "Return non-nil when DATE is a valid Hanke-Henry date.

DATE has the form (MONTH DAY YEAR), where MONTH is 1 through 12
or the symbol `xtr'."
  (pcase-let ((`(,month ,day ,year) date))
    (and (integerp year)
         (integerp day)
         (cond
          ((eq month 'xtr)
           (and (hanke-henry-calendar-leap-year-p year)
                (<= 1 day 7)))
          ((and (integerp month) (<= 1 month 12))
           (<= 1 day (hanke-henry-calendar--month-length month)))
          (t nil)))))

(defun hanke-henry-calendar-from-absolute (absolute)
  "Convert ABSOLUTE to a Hanke-Henry date.

Return (MONTH DAY YEAR), with MONTH equal to `xtr' for dates in
the intercalary week."
  (pcase-let* ((`(,year ,offset)
                (hanke-henry-calendar--year-and-offset absolute)))
    (if (>= offset 364)
        (list 'xtr (1+ (- offset 364)) year)
      (let* ((quarter (/ offset 91))
             (day-in-quarter (mod offset 91))
             (month-in-quarter
              (cond
               ((< day-in-quarter 30) 0)
               ((< day-in-quarter 60) 1)
               (t 2)))
             (month (+ 1 (* quarter 3) month-in-quarter))
             (month-offset
              (aref hanke-henry-calendar--month-offsets (1- month))))
        (list month (1+ (- offset month-offset)) year)))))

(defun hanke-henry-calendar-to-absolute (date)
  "Convert Hanke-Henry DATE to an absolute date.

DATE has the form (MONTH DAY YEAR), where MONTH is 1 through 12
or the symbol `xtr'.  Signal `wrong-type-argument' when DATE is
invalid."
  (unless (hanke-henry-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'hanke-henry-calendar-valid-date-p date)))
  (pcase-let ((`(,month ,day ,year) date))
    (+ (hanke-henry-calendar--year-start year)
       (if (eq month 'xtr)
           (+ 364 (1- day))
         (+ (aref hanke-henry-calendar--month-offsets (1- month))
            (1- day))))))

(defun hanke-henry-calendar-from-gregorian (date)
  "Convert Gregorian DATE to a Hanke-Henry date.

Both dates use Emacs calendar ordering: (MONTH DAY YEAR)."
  (hanke-henry-calendar-from-absolute
   (calendar-absolute-from-gregorian date)))

(defun hanke-henry-calendar-to-gregorian (date)
  "Convert Hanke-Henry DATE to a Gregorian date.

Both dates use Emacs calendar ordering: (MONTH DAY YEAR)."
  (calendar-gregorian-from-absolute
   (hanke-henry-calendar-to-absolute date)))

(defun hanke-henry-calendar-month-name (month)
  "Return the name of Hanke-Henry MONTH.

MONTH may be an integer from 1 through 12 or the symbol `xtr'."
  (cond
   ((eq month 'xtr) "Xtr")
   ((and (integerp month) (<= 1 month 12))
    (aref hanke-henry-calendar-month-names (1- month)))
   (t
    (signal 'wrong-type-argument
            (list '(or (integer 1 12) (const xtr)) month)))))

(defun hanke-henry-calendar-format-date (date)
  "Format Hanke-Henry DATE as a human-readable string."
  (unless (hanke-henry-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'hanke-henry-calendar-valid-date-p date)))
  (pcase-let ((`(,month ,day ,year) date))
    (format "%d %s %d"
            year
            (hanke-henry-calendar-month-name month)
            day)))

(defun hanke-henry-calendar-date-string (&optional gregorian-date)
  "Return the Hanke-Henry equivalent of GREGORIAN-DATE as a string.

GREGORIAN-DATE defaults to today."
  (hanke-henry-calendar-format-date
   (hanke-henry-calendar-from-gregorian
    (or gregorian-date (calendar-current-date)))))

;;;###autoload
(defun hanke-henry-calendar-show-date (&optional gregorian-date)
  "Display the Hanke-Henry equivalent of GREGORIAN-DATE.

When called interactively in a Calendar buffer, use the date at
point.  Otherwise use today's date."
  (interactive
   (list
    (when (derived-mode-p 'calendar-mode)
      (calendar-cursor-to-date t))))
  (message "Hanke-Henry date: %s"
           (hanke-henry-calendar-date-string gregorian-date)))

;;;###autoload
(defun hanke-henry-calendar-insert-date (&optional gregorian-date)
  "Insert the Hanke-Henry equivalent of GREGORIAN-DATE.

GREGORIAN-DATE defaults to today."
  (interactive)
  (insert (hanke-henry-calendar-date-string gregorian-date)))

(provide 'hanke-henry-calendar)

;;; hanke-henry-calendar.el ends here
