;;; moon-holidays.el --- holidays based on first full moon
;;; commentary:

;;; code:
(require 'calendar)
(require 'lunar)
(require 'cl-lib)

(defun holiday-named-full-moons (&rest moon-specs)
  "Return holiday entries for first full moons in selected months.

Each element of MOON-SPECS is:

  (MONTH FULL-NAME [NEXT-DAY-NAME])

MONTH is a number 1–12.
FULL-NAME is the holiday name for the first full moon in that month.
If NEXT-DAY-NAME is non-nil, also add a holiday for the day
*after* that full moon with NEXT-DAY-NAME.

This function is meant to be used in `calendar-holidays` /
`holiday-other-holidays` as a (function ...) holiday form, and it
only returns dates that are visible in the current 3-month
calendar window."

  (let* ((mid-m displayed-month)
         (mid-y displayed-year)
         ;; Compute previous and next months for the 3-month window
         (prev-m mid-m)
         (prev-y mid-y)
         (next-m mid-m)
         (next-y mid-y))
    (calendar-increment-month prev-m prev-y -1)
    (calendar-increment-month next-m next-y 1)
    (let* ((visible-months
            ;; ((month . year) ...) for the three columns
            (list (cons prev-m prev-y)
                  (cons mid-m  mid-y)
                  (cons next-m next-y)))
           (holidays '()))
      (dolist (spec moon-specs)
        (pcase-let ((`(,month ,full-name . ,maybe-next) spec))
          ;; Is this month in the visible 3-month window?
          (when-let* ((my (assoc month visible-months))
                      (year (cdr my)))
            ;; Collect full moons for this month/year
            (let ((candidates
                   (cl-loop for (date _time phase)
                            in (lunar-phase-list month year)
                            ;; phase = 2 => full moon
                            when (and (= phase 2)
                                      (= month (car date)))
                            collect date)))
              (when candidates
                ;; First full moon = earliest absolute date
                (let* ((first-full
                        (car (sort candidates
                                   (lambda (a b)
                                     (< (calendar-absolute-from-gregorian a)
                                        (calendar-absolute-from-gregorian b))))))
                       (entry (list first-full full-name)))
                  (push entry holidays)
                  ;; Optional extra day after the first full moon
                  (when maybe-next
                    (let* ((next-name (car maybe-next))
                           (next-abs (1+ (calendar-absolute-from-gregorian first-full)))
                           (next-date (calendar-gregorian-from-absolute next-abs)))
                      (push (list next-date next-name) holidays)))))))))
      holidays)))

(defun holiday-buddhist-moons ()
  "Return all Theravadin full-moon holidays for the visible window.

This uses `holiday-named-full-moons' for each relevant month and
concatenates the results."
  (append
   (holiday-fixed 4 13 "Traditional Buddhist Calendar")
   (holiday-named-full-moons '(5  "Vesak (Buddha Day)"))
   (holiday-named-full-moons '(7  "Asalha (Dhamma Day)" "Vassa"))
   (holiday-named-full-moons '(10 "Pavarana"))
   (holiday-named-full-moons '(2  "Magha (Sangha Day)"))))

;;;###autoload
(defcustom holiday-buddhist-holidays
  '(holiday-buddhist-moons)
  "Buddhist (Theravadin) lunar holidays based on full moons."
  :type 'sexp)

(provide 'moon-holidays)
;;; moon-holidays.el ends here
