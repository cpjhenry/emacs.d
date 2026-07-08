;;; mercury-retrograde.el --- Mercury retrograde calendar dates -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides published Mercury retrograde station dates for use with
;; the Emacs calendar and holiday system.

;;; Code:

(require 'calendar)
(require 'holidays)

(defconst mercury-retrograde-periods
  '((2016 ((1 5)  (1 25))
          ((4 28) (5 22))
          ((8 30) (9 22))
          ((12 19) (1 8)))
    (2017 ((4 9)  (5 3))
          ((8 12) (9 5))
          ((12 3) (12 22)))
    (2018 ((3 22) (4 15))
          ((7 26) (8 18))
          ((11 16) (12 6)))
    (2019 ((3 5)  (3 28))
          ((7 7)  (7 31))
          ((10 31) (11 20)))
    (2020 ((2 16) (3 9))
          ((6 17) (7 12))
          ((10 13) (11 3)))
    (2021 ((1 30) (2 20))
          ((5 29) (6 22))
          ((9 27) (10 18)))
    (2022 ((1 14) (2 3))
          ((5 10) (6 3))
          ((9 9)  (10 2))
          ((12 29) (1 18)))
    (2023 ((4 21) (5 14))
          ((8 23) (9 15))
          ((12 13) (1 1)))
    (2024 ((4 1)  (4 25))
          ((8 4)  (8 28))
          ((11 25) (12 15)))
    (2025 ((3 14) (4 7))
          ((7 18) (8 10))
          ((11 9) (11 29)))
    (2026 ((2 25) (3 20))
	  ((6 29) (7 23))
	  ((10 24)(11 13)))
    (2027 ((2 9)  (3 3))
	  ((6 10) (7 4))
	  ((10 7) (10 28)))
    (2028 ((1 24) (2 14))
	  ((5 21) (6 13))
	  ((9 19) (10 11)))
    (2029 ((1 7)  (1 27))
	  ((5 1)  (5 25))
	  ((9 2)  (9 24))
	  ((12 21) (1 10)))
    (2030 ((4 12) (5 6))
	  ((8 15) (9 8))
	  ((12 5) (12 25))))
  "Published Mercury retrograde periods.

Each period is of the form:

  ((START-MONTH START-DAY)
   (END-MONTH   END-DAY))

where the first date is the station retrograde and the second date
is the station direct.")

(defun holiday-mercury-retrograde ()
  "Return Mercury retrograde station dates for the displayed year."
  (let (holidays)
    (dolist (year (list (1- displayed-year) displayed-year))
      (dolist (period (cdr (assq year mercury-retrograde-periods)))
        (pcase-let ((`((,start-month ,start-day)
                       (,end-month ,end-day))
                     period))
          (let ((end-year
                 (if (< end-month start-month)
                     (1+ year)
                   year)))
            (when (= year displayed-year)
              (push (list (list start-month start-day year)
                          "Mercury Retrograde")
                    holidays))
            (when (= end-year displayed-year)
              (push (list (list end-month end-day end-year)
                          "Mercury Retrograde Ends")
                    holidays))))))
    (holiday-filter-visible-calendar
     (nreverse holidays))))

(provide 'mercury-retrograde)

;;; mercury-retrograde.el ends here
