;;; hindu-diwali.el --- Diwali dates for the Emacs calendar -*- lexical-binding: t; -*-

;;; Commentary:

;; Provide Diwali dates for the Emacs calendar.
;;
;; Published civil dates are used when available.  For later years,
;; the date is estimated from Kartika amavasya using `hindu-calendar'.

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'hindu-calendar)

(defconst holiday-diwali-dates
  '((2022 . (10 24 2022))
    (2023 . (11 12 2023))
    (2024 . (11 1 2024))
    (2025 . (10 21 2025))
    (2026 . (11 8 2026))
    (2027 . (10 29 2027))
    (2028 . (10 17 2028))
    (2029 . (11 5 2029))
    (2030 . (10 26 2030))
    (2031 . (11 14 2031))
    (2032 . (11 2 2032))
    (2033 . (10 22 2033))
    (2034 . (11 10 2034))
    (2035 . (10 30 2035))
    (2036 . (10 18 2036))
    (2037 . (11 6 2037))
    (2038 . (10 27 2038))
    (2039 . (11 15 2039))
    (2040 . (11 4 2040)))
  "Published civil dates for Diwali.

Diwali (Deepavali) is a Hindu festival determined by the lunisolar
calendar and the timing of Kartik Amavasya.  Because the observed
civil date may vary by tradition, location, and the timing of lunar
events, this table records published observance dates rather than
attempting a full panchang calculation.")

(defun holiday-diwali--calculated-date (year)
  "Return the calculated Gregorian date of Diwali in YEAR.

Identify Diwali as Kartika amavasya.  When amavasya is skipped
at sunrise, return the preceding civil date."
  (let ((hindu-calendar-month-type "Chaitra")
        (hindu-calendar-lunar-type "Purnimanta")
        previous-date
        previous-hindu-date)
    (catch 'diwali
      (dolist (month '(10 11))
        (dotimes (index
                  (calendar-last-day-of-month month year))
          (let* ((day (1+ index))
                 (date (list month day year))
                 (hindu-date
                  (hindu-calendar-sidereal-lunar
                   year month day)))
            (when (string-prefix-p "Kartika-K15," hindu-date)
              (throw 'diwali date))
            (when (and previous-hindu-date
                       (string-prefix-p "Kartika-K14,"
                                        previous-hindu-date)
                       (string-prefix-p "Kartika-S01,"
                                        hindu-date))
              (throw 'diwali previous-date))
            (setq previous-date date
                  previous-hindu-date hindu-date))))
      nil)))

(defun holiday-diwali ()
  "Return Diwali for the displayed year.

Use published civil dates when available.  For later years,
calculate Kartika amavasya using the Hindu sidereal lunar
calendar."
  (when-let* ((date
               (or (cdr (assq displayed-year
                              holiday-diwali-dates))
                   (holiday-diwali--calculated-date
                    displayed-year))))
    (holiday-filter-visible-calendar
     (list (list date "Diwali"))))) ; दीपावली

(defvar holiday-hindu-holidays
  '((holiday-diwali))
  "Hindu holidays and observances.")

(defun holiday-diwali-compare-dates ()
  "Compare calculated and published Diwali dates."
  (interactive)
  (with-output-to-temp-buffer "*Diwali Comparison*"
    (princ "Year  Published    Calculated   Result\n")
    (princ "----  -----------  -----------  --------\n")
    (dolist (entry holiday-diwali-dates)
      (let* ((year (car entry))
             (published (cdr entry))
             (calculated
              (holiday-diwali--calculated-date year)))
        (princ
         (format "%4d  %-11s  %-11s  %s\n"
                 year
                 (calendar-date-string published)
                 (if calculated
                     (calendar-date-string calculated)
                   "Not found")
                 (cond
                  ((null calculated) "NO MATCH")
                  ((equal published calculated) "agree")
                  (t
                   (format "%+d day"
                           (- (calendar-absolute-from-gregorian
                               calculated)
                              (calendar-absolute-from-gregorian
                               published)))))))))))


(provide 'hindu-diwali)

;;; hindu-diwali.el ends here

; LocalWords:  amavasya Deepavali Kartik panchang Kartika purnimanta
; LocalWords:  Chaitra दीपावली hindu diwali
