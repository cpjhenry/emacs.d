;;; moon-holidays.el --- Holidays based on first full moon -*- lexical-binding: t; -*-

;;; Commentary:

;; Define holidays based on the first full moon in selected Gregorian
;; months.
;;
;; `moon-holidays-first-full-moon' provides the underlying calculation
;; independently of the Emacs holiday display machinery.
;;
;; `holiday-named-full-moons' adapts that calculation to the active
;; Calendar holiday window for general first-full-moon observances.
;;
;; Theravāda Buddhist observances are defined separately in
;; `moon-holidays-buddhist-observances'.  Their dates are available
;; programmatically through `moon-holidays-buddhist-date' and to the
;; Emacs holiday machinery through `holiday-buddhist'.

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

(defconst moon-holidays-buddhist-observances
  '((magha    2 0 "Magha")
    (vesak    5 0 "Vesak")
    (asalha   7 0 "Asalha")
    (vassa    7 1 "Vassa")
    (pavarana 10 0 "Pavarana"))
  "Theravāda Buddhist observance specifications.

Each entry has the form:

  (KEY MONTH OFFSET NAME)

MONTH is the Gregorian month whose first full moon determines
the observance.  OFFSET is the number of days after that full
moon.")

(defun moon-holidays-buddhist-date (observance year)
  "Return the Gregorian date of Buddhist OBSERVANCE in YEAR.

OBSERVANCE is a key in `moon-holidays-buddhist-observances'.
Return nil when OBSERVANCE is unknown or when the relevant full
moon cannot be calculated."
  (when-let* ((spec
               (assq observance
                     moon-holidays-buddhist-observances))
              (month (nth 1 spec))
              (offset (nth 2 spec))
              (date
               (moon-holidays-first-full-moon month year)))
    (calendar-gregorian-from-absolute
     (+ offset
        (calendar-absolute-from-gregorian date)))))

(defun holiday-buddhist ()
  "Return Theravāda Buddhist full-moon holidays.

Return Magha, Vesak, Asalha, Vassa, and Pavarana when they fall
within the active Calendar holiday window."
  (let (holidays)
    (dolist (year (moon-holidays--years-in-window))
      (dolist (spec moon-holidays-buddhist-observances)
        (pcase-let ((`(,key ,_month ,_offset ,name) spec))
          (when-let* ((date
                       (moon-holidays-buddhist-date key year)))
            (push (list date name)
                  holidays)))))
    (holiday-filter-visible-calendar holidays)))

(defvar holiday-buddhist-holidays
  '((holiday-buddhist))
  "Buddhist holidays and observances.")

(provide 'moon-holidays)

;;; moon-holidays.el ends here
