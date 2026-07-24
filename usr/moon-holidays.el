;;; moon-holidays.el --- Holidays based on first full moon -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Define holidays based on the first full moon in selected Gregorian
;; months.
;;
;; `moon-holidays-first-full-moon' provides the underlying calculation
;; independently of the Emacs holiday display machinery.
;;
;; `holiday-named-full-moons' adapts that calculation to the active
;; Calendar holiday window, whether called from Calendar, `list-holidays',
;; `holiday-in-range', or another holiday consumer.

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'lunar)
(require 'cl-lib)

(defun moon-holidays-first-full-moon (month year)
  "Return the first full moon in Gregorian MONTH and YEAR.

Return the date in standard Emacs calendar form, or nil if no
full moon is found."
  (let ((candidates
         (cl-loop for (date _time phase)
                  in (lunar-phase-list month year)
                  when (and (= phase 2)
                            (= month
                               (calendar-extract-month date))
                            (= year
                               (calendar-extract-year date)))
                  collect date)))
    (car
     (sort candidates
           (lambda (a b)
             (< (calendar-absolute-from-gregorian a)
                (calendar-absolute-from-gregorian b)))))))

(defun moon-holidays--years-in-window ()
  "Return the Gregorian years represented in the calendar window."
  (pcase-let ((`(,_month-1 ,year-1 ,_month-2 ,year-2)
               (calendar-get-month-range)))
    (number-sequence year-1 year-2)))

(defun holiday-named-full-moons (&rest moon-specs)
  "Return visible holidays based on first full moons.

Each element of MOON-SPECS has the form:

  (MONTH FULL-NAME [NEXT-DAY-NAME])

MONTH is a Gregorian month number from 1 through 12.

FULL-NAME is the holiday name assigned to the first full moon in
that month.

If NEXT-DAY-NAME is non-nil, also return a holiday on the day
after the first full moon with that name.

Return only dates visible in the active Calendar holiday window.
This makes the function suitable for Calendar, `list-holidays',
`holiday-in-range', and other Emacs holiday consumers."
  (let (holidays)
    (dolist (year (moon-holidays--years-in-window))
      (dolist (spec moon-specs)
        (pcase-let ((`(,month ,full-name . ,maybe-next) spec))
          (when-let* ((first-full
                       (moon-holidays-first-full-moon month year)))
            (push (list first-full full-name)
                  holidays)
            (when-let* ((next-name (car maybe-next)))
              (push
               (list
                (calendar-gregorian-from-absolute
                 (1+ (calendar-absolute-from-gregorian
                      first-full)))
                next-name)
               holidays))))))
    (holiday-filter-visible-calendar holidays)))

(defun holiday-buddhist ()
  "Return Theravāda Buddhist full-moon holidays.

Return Magha, Vesak, Asalha, Vassa, and Pavarana when they fall
within the active Calendar holiday window."
  (holiday-named-full-moons
   '(2  "Magha (Sangha)")
   '(5  "Vesak (Buddha)")
   '(7  "Asalha (Dhamma)" "Vassa")
   '(10 "Pavarana")))

(defvar holiday-buddhist-holidays
  '((holiday-buddhist))
  "Buddhist holidays and observances.")

(provide 'moon-holidays)

;;; moon-holidays.el ends here
