;;; indiction-years.el --- Byzantine and Common Year Indictions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun indiction-ce (year)
  "Return the Common Era indiction number for YEAR."
  (1+ (mod (+ year 2) 15)))

(defun byzantine-year (date)
  "Return the Byzantine Anno Mundi year for Gregorian DATE.

The Byzantine year begins on 1 September in the Julian calendar."
  (pcase-let* ((absolute
                (calendar-absolute-from-gregorian date))
               (`(,month ,_day ,year)
                (calendar-julian-from-absolute absolute)))
    (+ year
       (if (>= month 9)
           5509
         5508))))

(defun indiction-byzantine (date)
  "Return the Byzantine indiction number for Gregorian DATE."
  (let ((year (byzantine-year date)))
    (let ((indiction (mod year 15)))
      (if (zerop indiction)
          15
        indiction))))


(provide 'indiction-years)

;;; indiction-years.el ends here
