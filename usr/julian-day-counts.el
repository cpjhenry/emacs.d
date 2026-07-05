;;; julian-day-counts.el --- MJD, TJD, and Rata Die conversions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert between Gregorian dates and three continuous day-count systems:
;;
;;   Modified Julian Date (MJD)
;;   Truncated Julian Date (TJD), using the NASA convention
;;   Rata Die (RD)
;;
;; Whole-number MJD and TJD values refer to midnight UTC.
;;
;; Definitions:
;;
;;   MJD = JD - 2400000.5
;;   TJD = MJD - 40000
;;       = JD - 2440000.5
;;
;; Rata Die counts Gregorian calendar days from:
;;
;;   RD 1 = 0001-01-01
;;
;; Emacs calendar absolute dates use the same whole-day numbering as
;; Rata Die.
;;
;; Examples:
;;
;;   (modified-julian-date-from-gregorian '(6 23 2026))
;;   => 61214
;;
;;   (truncated-julian-date-from-gregorian '(6 23 2026))
;;   => 21214
;;
;;   (rata-die-from-gregorian '(1 1 1))
;;   => 1

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup julian-day-counts nil
  "Modified Julian Date, Truncated Julian Date, and Rata Die."
  :group 'calendar)

(defconst modified-julian-date-rd-offset 678576
  "Rata Die value corresponding to MJD 0.

MJD 0 is 1858-11-17 00:00 UTC.")

(defconst truncated-julian-date-rd-offset 718576
  "Rata Die value corresponding to NASA TJD 0.

TJD 0 is 1968-05-24 00:00 UTC, equivalent to MJD 40000.")

(defun julian-day-counts--current-utc-date ()
  "Return the current UTC date in Emacs calendar form."
  (pcase-let ((`(,_second ,_minute ,_hour ,day ,month ,year . ,_)
                (decode-time nil t)))
    (list month day year)))

(defun julian-day-counts--utc-day-fraction (&optional time)
  "Return the fraction of the current UTC day represented by TIME.

TIME defaults to the current time."
  (pcase-let ((`(,second ,minute ,hour . ,_)
                (decode-time time t)))
    (/ (+ (* hour 3600.0)
          (* minute 60.0)
          second)
       86400.0)))

(defun rata-die-from-gregorian (date)
  "Return the Rata Die number for Gregorian DATE.

DATE uses Emacs calendar ordering: (MONTH DAY YEAR)."
  (calendar-absolute-from-gregorian date))

(defun rata-die-to-gregorian (rd)
  "Convert whole-number Rata Die value RD to a Gregorian date."
  (unless (integerp rd)
    (signal 'wrong-type-argument (list 'integerp rd)))
  (calendar-gregorian-from-absolute rd))

(defun modified-julian-date-from-rata-die (rd)
  "Convert Rata Die value RD to Modified Julian Date."
  (- rd modified-julian-date-rd-offset))

(defun modified-julian-date-to-rata-die (mjd)
  "Convert Modified Julian Date MJD to Rata Die."
  (+ mjd modified-julian-date-rd-offset))

(defun truncated-julian-date-from-rata-die (rd)
  "Convert Rata Die value RD to NASA Truncated Julian Date."
  (- rd truncated-julian-date-rd-offset))

(defun truncated-julian-date-to-rata-die (tjd)
  "Convert NASA Truncated Julian Date TJD to Rata Die."
  (+ tjd truncated-julian-date-rd-offset))

(defun modified-julian-date-from-gregorian (date)
  "Return the MJD at midnight UTC beginning Gregorian DATE."
  (modified-julian-date-from-rata-die
   (rata-die-from-gregorian date)))

(defun modified-julian-date-to-gregorian (mjd)
  "Convert whole-number MJD to a Gregorian date.

Fractional values are accepted; the containing UTC civil date is
returned."
  (calendar-gregorian-from-absolute
   (floor (modified-julian-date-to-rata-die mjd))))

(defun truncated-julian-date-from-gregorian (date)
  "Return the NASA TJD at midnight UTC beginning Gregorian DATE."
  (truncated-julian-date-from-rata-die
   (rata-die-from-gregorian date)))

(defun truncated-julian-date-to-gregorian (tjd)
  "Convert NASA TJD to a Gregorian date.

Fractional values are accepted; the containing UTC civil date is
returned."
  (calendar-gregorian-from-absolute
   (floor (truncated-julian-date-to-rata-die tjd))))

(defun modified-julian-date-from-time (&optional time)
  "Return the fractional MJD for Emacs TIME.

TIME defaults to the current instant.  UTC is used."
  (+ (modified-julian-date-from-gregorian
      (pcase-let ((`(,_second ,_minute ,_hour ,day ,month ,year . ,_)
                    (decode-time time t)))
        (list month day year)))
     (julian-day-counts--utc-day-fraction time)))

(defun truncated-julian-date-from-time (&optional time)
  "Return the fractional NASA TJD for Emacs TIME.

TIME defaults to the current instant.  UTC is used."
  (- (modified-julian-date-from-time time) 40000))

(defun modified-julian-date-today ()
  "Return today's whole-number MJD in UTC."
  (modified-julian-date-from-gregorian
   (julian-day-counts--current-utc-date)))

(defun truncated-julian-date-today ()
  "Return today's whole-number NASA TJD in UTC."
  (truncated-julian-date-from-gregorian
   (julian-day-counts--current-utc-date)))

(defun rata-die-today ()
  "Return today's whole-number Rata Die in UTC."
  (rata-die-from-gregorian
   (julian-day-counts--current-utc-date)))

(defun julian-day-counts-date-string (&optional date)
  "Return MJD, TJD, and Rata Die for Gregorian DATE.

DATE defaults to today's UTC civil date."
  (let ((date (or date (julian-day-counts--current-utc-date))))
    (format "MJD %d  TJD %d  RD %d"
            (modified-julian-date-from-gregorian date)
            (truncated-julian-date-from-gregorian date)
            (rata-die-from-gregorian date))))

;;;###autoload
(defun julian-day-counts-show-date ()
  "Display today's MJD, TJD, and Rata Die in UTC."
  (interactive)
  (message "%s" (julian-day-counts-date-string)))

;;;###autoload
(defun julian-day-counts-show-date-at-point ()
  "Display MJD, TJD, and Rata Die for the Calendar date at point."
  (interactive)
  (unless (derived-mode-p 'calendar-mode)
    (user-error "This command must be used in a Calendar buffer"))
  (message "%s"
           (julian-day-counts-date-string
            (calendar-cursor-to-date t))))

;;;###autoload
(defun julian-day-counts-insert-date (&optional date)
  "Insert MJD, TJD, and Rata Die for Gregorian DATE.

DATE defaults to today's UTC civil date."
  (interactive)
  (insert (julian-day-counts-date-string date)))

(provide 'julian-day-counts)

;;; julian-day-counts.el ends here
