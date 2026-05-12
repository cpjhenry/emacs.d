;;; liturgical-year.el --- Liturgical year season boundaries -*- lexical-binding: t; -*-

;; Author: cpj
;; Package-Requires: ((emacs "29.1"))
;; Keywords: calendar, holidays

;;; Commentary:

;; This package provides calculated holiday entries for liturgical
;; season boundaries.
;;
;; Two calendars are currently provided:
;;
;;   `holiday-roman-liturgical-year'
;;   `holiday-anglican-canada-liturgical-year'
;;
;; The Roman Rite calendar begins with Advent, then proceeds through
;; Christmastide, Ordinary Time, Lent, the Easter Triduum, Eastertide,
;; and Ordinary Time resumed.
;;
;; The Anglican Church of Canada calendar uses the same basic Advent,
;; Christmas, Lent, Holy Week, Easter, and Pentecost structure, but uses
;; "Season after Epiphany" and "Season after Pentecost" rather than
;; "Ordinary Time".
;;
;; To add the Roman Rite boundaries to `calendar-holidays':
;;
;;   (setq calendar-holidays
;;         (append holiday-christian-holidays
;;                 holiday-roman-liturgical-year
;;                 holiday-local-holidays))
;;
;; To add the Anglican Church of Canada boundaries instead:
;;
;;   (setq calendar-holidays
;;         (append holiday-christian-holidays
;;                 holiday-anglican-canada-liturgical-year
;;                 holiday-local-holidays))
;;
;; To display the calculated liturgical seasons and their boundaries
;; for the current or displayed year:
;;
;;   (list-roman-liturgical-year)
;;
;; or:
;;
;;   (list-anglican-canada-liturgical-year)

;; Future expansion ideas:
;; - Liturgical colours
;; - Feast rank / precedence handling
;; - Sanctoral cycle integration
;; - BAS / BCP variants
;; - Roman pre- and post-Vatican II variants
;; - Orthodox calendrical variants
;; - calfw integration
;; - Org agenda integration
;; - Export/report generation
;; - “Current liturgical season” query helpers

;;; Code:

(require 'calendar)
(require 'holidays)

(defvar displayed-year)

(defun holiday-liturgical--current-or-displayed-year ()
  "Return `displayed-year' if bound, otherwise the current year."
  (or (and (boundp 'displayed-year) displayed-year)
      (nth 2 (calendar-current-date))))

(defun holiday-liturgical--date< (a b)
  "Return non-nil if holiday entry A occurs before holiday entry B."
  (< (calendar-absolute-from-gregorian (car a))
     (calendar-absolute-from-gregorian (car b))))

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

(defun holiday-liturgical--roman-epiphany-observed (year)
  "Return the absolute date of observed Roman Epiphany for YEAR.

This assumes Epiphany is observed on the Sunday falling from
January 2 through January 8, inclusive."
  (holiday-liturgical--sunday-on-or-after
   (calendar-absolute-from-gregorian (list 1 2 year))))

(defun holiday-liturgical--roman-baptism (year)
  "Return the absolute date of the Roman Baptism of the Lord for YEAR.

When observed Epiphany falls on January 7 or January 8, the
Baptism of the Lord is celebrated on the following Monday.
Otherwise, it is celebrated on the Sunday after Epiphany."
  (let* ((epiphany-abs (holiday-liturgical--roman-epiphany-observed year))
         (epiphany-date (calendar-gregorian-from-absolute epiphany-abs))
         (epiphany-day (cadr epiphany-date)))
    (if (member epiphany-day '(7 8))
        (1+ epiphany-abs)
      (+ epiphany-abs 7))))

(defun holiday-liturgical--anglican-canada-baptism (year)
  "Return the absolute date of the Baptism of the Lord for YEAR.

For the Anglican Church of Canada, this is treated here as the
Sunday after Epiphany, with Epiphany fixed on January 6."
  (holiday-liturgical--sunday-on-or-after
   (calendar-absolute-from-gregorian (list 1 7 year))))

(defun holiday-baptism ()
  "Return the Roman Rite Baptism of the Lord for `displayed-year'."
  (list
   (list
    (calendar-gregorian-from-absolute
     (holiday-liturgical--roman-baptism displayed-year))
    "The Baptism")))

(defun holiday-roman-liturgical-boundaries-for-year (year)
  "Return Roman Rite liturgical season boundaries for YEAR."
  (let* ((advent-start (holiday-liturgical--advent-start year))
         (advent-end (calendar-absolute-from-gregorian (list 12 24 year)))

         (christmastide-start advent-end)
         (baptism (holiday-liturgical--roman-baptism year))

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
     (list (calendar-gregorian-from-absolute holy-thursday)
           "Lent ends")

     (list (calendar-gregorian-from-absolute holy-thursday)
           "Easter Triduum begins")
     (list (calendar-gregorian-from-absolute easter)
           "Easter Triduum ends")

     (list (calendar-gregorian-from-absolute easter)
           "Eastertide begins")
     (list (calendar-gregorian-from-absolute pentecost)
           "Eastertide ends / Pentecost")

     (list (calendar-gregorian-from-absolute ordinary-2-start)
           "Ordinary Time resumes")
     (list (calendar-gregorian-from-absolute ordinary-2-end)
           "Ordinary Time ends"))))

(defun holiday-roman-liturgical-boundaries ()
  "Return Roman Rite liturgical season boundaries for `displayed-year'."
  (holiday-roman-liturgical-boundaries-for-year displayed-year))

(defun holiday-anglican-canada-liturgical-boundaries-for-year (year)
  "Return Anglican Church of Canada liturgical season boundaries for YEAR."
  (let* ((advent-start (holiday-liturgical--advent-start year))
         (advent-end (calendar-absolute-from-gregorian (list 12 24 year)))

         (christmas-start advent-end)
         (christmas-end (calendar-absolute-from-gregorian (list 1 5 year)))

         (epiphany (calendar-absolute-from-gregorian (list 1 6 year)))
         (baptism (holiday-liturgical--anglican-canada-baptism year))
         (season-after-epiphany-start (1+ epiphany))

         (easter (holiday-liturgical--easter year))
         (ash-wednesday (- easter 46))
         (palm-sunday (- easter 7))
         (holy-thursday (- easter 3))
         (pentecost (+ easter 49))

         (season-after-epiphany-end (1- ash-wednesday))
         (easter-season-end pentecost)
         (season-after-pentecost-start (1+ pentecost))
         (season-after-pentecost-end (1- advent-start)))

    (list
     (list (calendar-gregorian-from-absolute advent-start)
           "Advent begins")
     (list (calendar-gregorian-from-absolute advent-end)
           "Advent ends")

     (list (calendar-gregorian-from-absolute christmas-start)
           "Christmas begins")
     (list (calendar-gregorian-from-absolute christmas-end)
           "Christmas ends")

     (list (calendar-gregorian-from-absolute epiphany)
           "Epiphany")
     (list (calendar-gregorian-from-absolute season-after-epiphany-start)
           "Season after Epiphany begins")
     (list (calendar-gregorian-from-absolute baptism)
           "The Baptism")
     (list (calendar-gregorian-from-absolute season-after-epiphany-end)
           "Season after Epiphany ends")

     (list (calendar-gregorian-from-absolute ash-wednesday)
           "Lent begins")
     (list (calendar-gregorian-from-absolute palm-sunday)
           "Holy Week begins / Palm Sunday")
     (list (calendar-gregorian-from-absolute holy-thursday)
           "Lent ends")

     (list (calendar-gregorian-from-absolute holy-thursday)
           "Easter Triduum begins")
     (list (calendar-gregorian-from-absolute easter)
           "Easter Triduum ends")

     (list (calendar-gregorian-from-absolute easter)
           "Easter season begins")
     (list (calendar-gregorian-from-absolute easter)
           "Easter Day")
     (list (calendar-gregorian-from-absolute easter-season-end)
           "Easter season ends / Pentecost")

     (list (calendar-gregorian-from-absolute season-after-pentecost-start)
           "Season after Pentecost begins")
     (list (calendar-gregorian-from-absolute season-after-pentecost-end)
           "Season after Pentecost ends"))))

(defun holiday-anglican-canada-liturgical-boundaries ()
  "Return Anglican Church of Canada season boundaries for `displayed-year'."
  (holiday-anglican-canada-liturgical-boundaries-for-year displayed-year))

(defvar holiday-roman-liturgical-year
  '((holiday-roman-liturgical-boundaries))
  "Roman Rite liturgical season boundary holidays.")

(defvar holiday-anglican-canada-liturgical-year
  '((holiday-anglican-canada-liturgical-boundaries))
  "Anglican Church of Canada liturgical season boundary holidays.")

(defun liturgical-year--display (year holidays buffer-name title)
  "Display HOLIDAYS for YEAR in BUFFER-NAME using TITLE."
  (with-output-to-temp-buffer buffer-name
    (princ (format "%s — %d\n\n" title year))
    (dolist (holiday (sort holidays #'holiday-liturgical--date<))
      (princ
       (format "%s  %s\n"
               (calendar-date-string (car holiday) nil t)
               (cadr holiday))))))

(defun list-roman-liturgical-year ()
  "Display Roman Rite liturgical year boundaries."
  (interactive)
  (let ((year (holiday-liturgical--current-or-displayed-year)))
    (liturgical-year--display
     year
     (holiday-roman-liturgical-boundaries-for-year year)
     "*Roman Liturgical Year*"
     "Roman Rite Liturgical Year Boundaries")))

(defun list-anglican-canada-liturgical-year ()
  "Display Anglican Church of Canada liturgical year boundaries."
  (interactive)
  (let ((year (holiday-liturgical--current-or-displayed-year)))
    (liturgical-year--display
     year
     (holiday-anglican-canada-liturgical-boundaries-for-year year)
     "*Anglican Canada Liturgical Year*"
     "Anglican Church of Canada Liturgical Year Boundaries")))

(provide 'liturgical-year)

;;; liturgical-year.el ends here

; LocalWords:  canada anglican
