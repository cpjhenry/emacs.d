(defun holiday-hebrew-rosh-hashanah (&optional _all)
  "Return major autumn observances for the visible calendar.

This is a simplified variant of the corresponding Emacs holiday
function. The original implementation includes a broader selection of
observances and optional detail controlled by
`calendar-hebrew-all-holidays-flag'.

The spelling \"Rosh Hashana\" follows the convention used by `hebcal'
and related calendar references."
  (holiday-filter-visible-calendar
   (mapcan
    (lambda (year)
      (let ((abs-r-h
             (calendar-hebrew-to-absolute
              (list 7 1 (+ year 3761)))))
        (list
         (list (calendar-gregorian-from-absolute abs-r-h)
               (format "Rosh Hashana %d" (+ 3761 year)))
         (list (calendar-gregorian-from-absolute (+ abs-r-h 9))
               "Yom Kippur")
         (list (calendar-gregorian-from-absolute (+ abs-r-h 14))
               "Sukkot"))))
    (calendar-month-visible-p 8 2))))
