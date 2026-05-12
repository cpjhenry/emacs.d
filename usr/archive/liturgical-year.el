;;; liturgical-year.el --- Roman Rite liturgical year boundaries -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: calendar

;;; Commentary:

;; This package provides calculated holiday entries for the principal
;; seasons of the Roman Rite liturgical year.
;;
;; The liturgical year is treated as a sequence of season boundaries,
;; beginning with Advent and continuing through Christmastide, Ordinary
;; Time, Lent, the Easter Triduum, Eastertide, and the return to
;; Ordinary Time before the next Advent.
;;
;; The calculations are intentionally derived directly from calendar
;; rules rather than by relying on higher-level holiday helpers.  This
;; keeps the logic visible: Advent begins on the Sunday on or after
;; November 27; observed Epiphany falls on the Sunday from January 2
;; through January 8; the Baptism of the Lord follows Epiphany according
;; to the current Roman Rite rule; and Lent, Triduum, Eastertide, and
;; Pentecost are derived from Easter.
;;
;; To use:
;;
;;   (require 'liturgical-year)
;;
;; Then add `holiday-liturgical-year' to `calendar-holidays', for example:
;;
;;   (setq calendar-holidays
;;         (append holiday-christian-holidays
;;                 holiday-liturgical-year
;;                 holiday-local-holidays))
;;
;; To display the calculated liturgical seasons and their boundaries
;; for the current or displayed year:
;;
;;   (list-liturgical-year)

;;; Code:
(require 'calendar)
(require 'holidays)
(defvar displayed-year)

(defun holiday-liturgical--sunday-on-or-after (absolute-date)
  "Return the absolute date of the Sunday on or after ABSOLUTE-DATE."
  (+ absolute-date
     (mod (- 0
             (calendar-day-of-week
              (calendar-gregorian-from-absolute absolute-date)))
          7)))

(defun holiday-liturgical--advent-start (year)
  "Return the absolute date of the First Sunday of Advent for YEAR."
  (holiday-liturgical--sunday-on-or-after
   (calendar-absolute-from-gregorian (list 11 27 year))))

(defun holiday-liturgical--epiphany-observed (year)
  "Return the absolute date of observed Epiphany for YEAR.

This assumes Epiphany is observed on the Sunday falling from
January 2 through January 8, inclusive."
  (holiday-liturgical--sunday-on-or-after
   (calendar-absolute-from-gregorian (list 1 2 year))))

(defun holiday-liturgical--baptism (year)
  "Return the absolute date of the Baptism of the Lord for YEAR.

When observed Epiphany falls on January 7 or January 8, the
Baptism of the Lord is celebrated on the following Monday.
Otherwise, it is celebrated on the Sunday after Epiphany."
  (let* ((epiphany-abs (holiday-liturgical--epiphany-observed year))
         (epiphany-date (calendar-gregorian-from-absolute epiphany-abs))
         (epiphany-day (cadr epiphany-date)))
    (if (member epiphany-day '(7 8))
        (1+ epiphany-abs)
      (+ epiphany-abs 7))))

(defun holiday-baptism ()
  "Return the date of the Baptism of the Lord for `displayed-year'."
  (list
   (list
    (calendar-gregorian-from-absolute
     (holiday-liturgical--baptism displayed-year))
    "Baptism of the Lord")))

(defun holiday-liturgical--easter (year)
  "Return the absolute date of Western Easter Sunday for YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact
          (% (+ 14
                (* 11 (% year 19))
                (- (/ (* 3 century) 4))
                (/ (+ 5 (* 8 century)) 25)
                (* 30 century))
             30))
         (adjusted-epact
          (if (or (= shifted-epact 0)
                  (and (= shifted-epact 1)
                       (< 10 (% year 19))))
              (1+ shifted-epact)
            shifted-epact))
         (paschal-moon
          (- (calendar-absolute-from-gregorian (list 4 19 year))
             adjusted-epact)))
    (holiday-liturgical--sunday-on-or-after (1+ paschal-moon))))

(defun holiday-liturgical-boundaries-for-year (year)
  "Return Roman Rite liturgical season boundaries for YEAR."
  (let* ((advent-start (holiday-liturgical--advent-start year))
         (advent-end (calendar-absolute-from-gregorian (list 12 24 year)))

         (christmastide-start advent-end)
         (baptism (holiday-liturgical--baptism year))

         (ordinary-1-start
          (if (= (calendar-day-of-week
                  (calendar-gregorian-from-absolute baptism))
                 1)
              baptism
            (1+ baptism)))

	 (easter (holiday-liturgical--easter year))
         (ash-wednesday (- easter 46))
         (holy-thursday (- easter 3))
         (pentecost (+ easter 49))

         (ordinary-1-end (1- ash-wednesday))
         (lent-end holy-thursday)

         (triduum-start holy-thursday)
         (triduum-end easter)

         (eastertide-start easter)
         (eastertide-end pentecost)

         (ordinary-2-start (1+ pentecost))
         (ordinary-2-end (1- advent-start)))

    (list
     (list (calendar-gregorian-from-absolute advent-start)
           "Advent begins")
     (list (calendar-gregorian-from-absolute advent-end)
           "Advent ends")

     (list (calendar-gregorian-from-absolute christmastide-start)
           "Christmastide begins")
     (list (calendar-gregorian-from-absolute baptism)
           "Christmastide ends / The Baptism")

     (list (calendar-gregorian-from-absolute ordinary-1-start)
           "Ordinary Time begins")
     (list (calendar-gregorian-from-absolute ordinary-1-end)
           "Ordinary Time ends")

     (list (calendar-gregorian-from-absolute ash-wednesday)
           "Lent begins")
     (list (calendar-gregorian-from-absolute lent-end)
           "Lent ends")

     (list (calendar-gregorian-from-absolute triduum-start)
           "Easter Triduum begins")
     (list (calendar-gregorian-from-absolute triduum-end)
           "Easter Triduum ends")

     (list (calendar-gregorian-from-absolute eastertide-start)
           "Eastertide begins")
     (list (calendar-gregorian-from-absolute eastertide-end)
           "Eastertide ends / Pentecost")

     (list (calendar-gregorian-from-absolute ordinary-2-start)
           "Ordinary Time resumes")
     (list (calendar-gregorian-from-absolute ordinary-2-end)
           "Ordinary Time ends"))))

(defun holiday-liturgical-boundaries ()
  "Return Roman Rite liturgical season boundaries for `displayed-year'."
  (holiday-liturgical-boundaries-for-year displayed-year))

(defvar holiday-liturgical-year
  '((holiday-liturgical-boundaries))
  "Roman Rite liturgical season boundary holidays.")

(defun list-liturgical-year ()
  "Display liturgical year boundaries for the current or displayed year."
  (interactive)
  (let* ((year (or (and (boundp 'displayed-year) displayed-year)
                   (nth 2 (calendar-current-date))))
         (holidays (sort (holiday-liturgical-boundaries-for-year year)
                         #'calendar-date-compare)))
    (with-output-to-temp-buffer "*Liturgical Year*"
      (princ (format "Liturgical Year Boundaries — %d\n\n" year))
      (dolist (holiday holidays)
        (princ
         (format "%s  %s\n"
                 (calendar-date-string (car holiday) nil t)
                 (cadr holiday)))))))

(provide 'liturgical-year)

;;; liturgical-year.el ends here
