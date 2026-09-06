(defun years-on-earth (&optional date)
  "Return age as completed years plus fraction of current year of life.

DATE is a Gregorian date in (MONTH DAY YEAR) form and defaults
to the current date.  Integral ages occur exactly on birthdays."
  (let* ((date (or date (calendar-current-date)))
         (month (calendar-extract-month date))
         (day   (calendar-extract-day date))
         (year  (calendar-extract-year date))
         (birth-month (calendar-extract-month user-birthdate))
         (birth-day   (calendar-extract-day user-birthdate))
         (birth-year  (calendar-extract-year user-birthdate))

         (today
          (calendar-absolute-from-gregorian date))
         (birthday-this-year
          (calendar-absolute-from-gregorian
           (list birth-month birth-day year)))

         (age
          (if (>= today birthday-this-year)
              (- year birth-year)
            (1- (- year birth-year))))

         (last-birthday
          (calendar-absolute-from-gregorian
           (list birth-month birth-day (+ birth-year age))))
         (next-birthday
          (calendar-absolute-from-gregorian
           (list birth-month birth-day (+ birth-year age 1)))))

    (+ age
       (/ (- today last-birthday)
          (float (- next-birthday last-birthday))))))
