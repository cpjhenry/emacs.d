;;; nanakshahi-calendar.el --- Nanakshahi calendar conversions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert dates between the Gregorian calendar and the original
;; fixed solar Nanakshahi calendar.
;;
;; Nanakshahi year 1 began on March 14, 1469 CE.  Each Nanakshahi
;; year begins on March 14.
;;
;; The first five months contain 31 days.  The remaining months
;; contain 30 days, except that Phagun contains 31 days when the
;; Gregorian year in which it ends is a leap year.
;;
;; A Nanakshahi date is represented as:
;;
;;     (MONTH DAY YEAR)
;;
;; MONTH is an integer from 1 through 12.
;;
;; Examples:
;;
;;     (nanakshahi-calendar-from-gregorian '(3 14 1469))
;;     => (1 1 1)
;;
;;     (nanakshahi-calendar-from-gregorian '(6 27 2026))
;;     => (4 13 558)
;;
;;     (nanakshahi-calendar-to-gregorian '(4 13 558))
;;     => (6 27 2026)

;;; Code:

(require 'calendar)

(defgroup nanakshahi-calendar nil
  "The fixed solar Nanakshahi calendar."
  :group 'calendar)

(defconst nanakshahi-calendar-epoch-year 1469
  "Gregorian year in which Nanakshahi year 1 began.")

(defconst nanakshahi-calendar-month-names
  ["Chet" "Vaisakh" "Jeth" "Harh" "Sawan" "Bhadon"
   "Assu" "Katak" "Maghar" "Poh" "Magh" "Phagun"]
  "Names of the twelve Nanakshahi months.")

(defconst nanakshahi-calendar--month-offsets
  [0 31 62 93 124 155 185 215 245 275 305 335]
  "Zero-based day offsets of Nanakshahi months within the year.")

(defun nanakshahi-calendar--gregorian-year (year)
  "Return the Gregorian year in which Nanakshahi YEAR begins."
  (+ year (1- nanakshahi-calendar-epoch-year)))

(defun nanakshahi-calendar--year-start (year)
  "Return the absolute date of the beginning of Nanakshahi YEAR."
  (calendar-absolute-from-gregorian
   (list 3 14 (nanakshahi-calendar--gregorian-year year))))

(defun nanakshahi-calendar-leap-year-p (year)
  "Return non-nil when Nanakshahi YEAR contains an extra day.

The extra day is Phagun 31.  Phagun ends in the Gregorian year
following the year in which the Nanakshahi year began."
  (calendar-leap-year-p
   (1+ (nanakshahi-calendar--gregorian-year year))))

(defun nanakshahi-calendar--month-length (month year)
  "Return the number of days in Nanakshahi MONTH of YEAR."
  (cond
   ((<= 1 month 5) 31)
   ((<= 6 month 11) 30)
   ((= month 12)
    (if (nanakshahi-calendar-leap-year-p year) 31 30))
   (t
    (signal 'args-out-of-range (list month 1 12)))))

(defun nanakshahi-calendar-valid-date-p (date)
  "Return non-nil when DATE is a valid Nanakshahi date.

DATE has the form (MONTH DAY YEAR)."
  (pcase date
    (`(,month ,day ,year)
     (and (integerp month)
          (<= 1 month 12)
          (integerp day)
          (integerp year)
          (>= year 1)
          (<= 1 day
              (nanakshahi-calendar--month-length month year))))
    (_ nil)))

(defun nanakshahi-calendar--year-and-offset (absolute)
  "Return the Nanakshahi year and zero-based day for ABSOLUTE.

The return value has the form (YEAR OFFSET)."
  (let* ((gregorian-date
          (calendar-gregorian-from-absolute absolute))
         (gregorian-year
          (nth 2 gregorian-date))
         (candidate-year
          (- gregorian-year
             (1- nanakshahi-calendar-epoch-year)))
         (candidate-start
          (nanakshahi-calendar--year-start candidate-year)))
    (if (< absolute candidate-start)
        (let* ((year (1- candidate-year))
               (start (nanakshahi-calendar--year-start year)))
          (list year (- absolute start)))
      (list candidate-year (- absolute candidate-start)))))

(defun nanakshahi-calendar-from-absolute (absolute)
  "Convert absolute date ABSOLUTE to a Nanakshahi date."
  (pcase-let* ((`(,year ,offset)
                (nanakshahi-calendar--year-and-offset absolute))
               (month
                (cond
                 ((< offset 31) 1)
                 ((< offset 62) 2)
                 ((< offset 93) 3)
                 ((< offset 124) 4)
                 ((< offset 155) 5)
                 ((< offset 185) 6)
                 ((< offset 215) 7)
                 ((< offset 245) 8)
                 ((< offset 275) 9)
                 ((< offset 305) 10)
                 ((< offset 335) 11)
                 (t 12)))
               (month-offset
                (aref nanakshahi-calendar--month-offsets
                      (1- month))))
    (list month
          (1+ (- offset month-offset))
          year)))

(defun nanakshahi-calendar-to-absolute (date)
  "Convert Nanakshahi DATE to an absolute date.

DATE has the form (MONTH DAY YEAR).  Signal
`wrong-type-argument' when DATE is invalid."
  (unless (nanakshahi-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'nanakshahi-calendar-valid-date-p date)))
  (pcase-let ((`(,month ,day ,year) date))
    (+ (nanakshahi-calendar--year-start year)
       (aref nanakshahi-calendar--month-offsets (1- month))
       (1- day))))

(defun nanakshahi-calendar-from-gregorian (date)
  "Convert Gregorian DATE to a Nanakshahi date.

Both dates use Emacs calendar ordering: (MONTH DAY YEAR)."
  (nanakshahi-calendar-from-absolute
   (calendar-absolute-from-gregorian date)))

(defun nanakshahi-calendar-to-gregorian (date)
  "Convert Nanakshahi DATE to a Gregorian date.

Both dates use Emacs calendar ordering: (MONTH DAY YEAR)."
  (calendar-gregorian-from-absolute
   (nanakshahi-calendar-to-absolute date)))

(defun nanakshahi-calendar-month-name (month)
  "Return the name of Nanakshahi MONTH."
  (unless (and (integerp month)
               (<= 1 month 12))
    (signal 'args-out-of-range (list month 1 12)))
  (aref nanakshahi-calendar-month-names (1- month)))

(defun nanakshahi-calendar-format-date (date)
  "Format Nanakshahi DATE as a human-readable string."
  (unless (nanakshahi-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'nanakshahi-calendar-valid-date-p date)))
  (pcase-let ((`(,month ,day ,year) date))
    (format "%d %s %d"
            day
            (nanakshahi-calendar-month-name month)
            year)))

(defun nanakshahi-calendar-date-string (&optional gregorian-date)
  "Return the Nanakshahi equivalent of GREGORIAN-DATE as a string.

GREGORIAN-DATE defaults to the current local civil date."
  (nanakshahi-calendar-format-date
   (nanakshahi-calendar-from-gregorian
    (or gregorian-date
        (calendar-current-date)))))

;;;###autoload
(defun nanakshahi-calendar-show-date ()
  "Display the current local Nanakshahi date."
  (interactive)
  (message "Nanakshahi date: %s"
           (nanakshahi-calendar-date-string)))

;;;###autoload
(defun nanakshahi-calendar-show-date-at-point ()
  "Display the Nanakshahi equivalent of the Calendar date at point."
  (interactive)
  (unless (derived-mode-p 'calendar-mode)
    (user-error "This command must be used in a Calendar buffer"))
  (message "Nanakshahi date: %s"
           (nanakshahi-calendar-date-string
            (calendar-cursor-to-date t))))

;;;###autoload
(defun nanakshahi-calendar-insert-date (&optional gregorian-date)
  "Insert the Nanakshahi equivalent of GREGORIAN-DATE.

GREGORIAN-DATE defaults to the current local civil date."
  (interactive)
  (insert (nanakshahi-calendar-date-string gregorian-date)))

(provide 'nanakshahi-calendar)

;;; nanakshahi-calendar.el ends here
