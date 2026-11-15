;;; calendar-holiday-extras.el --- Additional calendar holiday calculations -*- lexical-binding: t; -*-

;;; Commentary:
;; Reusable holiday generators and date helpers extracted from
;; `local-holidays.el'.  No user holiday selections or global settings
;; are applied merely by loading this library.
;;
;; This is a candidate for further generalization and testing before
;; submission to GNU ELPA, not yet an independent ELPA-ready package.

;;; Code:
(require 'calendar)
(require 'holidays)
(require 'cal-china)
(require 'cal-hebrew)
(require 'solar)
(require 'seq)

;;; Corrected and additional holiday generators

(defun holiday-bahai-naw-ruz ()
  "Return Bahá’í New Year for the visible calendar.

Naw-Rúz is formally defined by the Bahá’í calendar as occurring at the
Vernal Equinox as observed in Tehran. Earlier Emacs holiday definitions
approximated this by fixing the observance on 21 March.

This implementation improves upon that approximation by deriving the
date from the calculated Vernal Equinox for the relevant Gregorian year
and using the resulting day in March."
  (holiday-filter-visible-calendar
   (mapcar
    (lambda (y)
      (let ((day (floor (nth 1 (solar-equinoxes/solstices 1 y)))))
        (list (list 3 day y)
              (format "Bahá’í New Year (Naw-Rúz) %d"
                      (- y 1843)))))
    (calendar-month-visible-p 3 1))))

(defun holiday-hebrew-tisha-b-av ()
  "Return Tisha B'Av for the displayed year.

This function replaces the broader Emacs Hebrew holiday listing with
a single observance of Tisha B'Av.

When 9 Av falls on Shabbat, the fast is deferred until the following
day. The returned date reflects the actual observance rather than the
nominal date in the Hebrew calendar."
  (let ((h (holiday-hebrew 5 9 "Tisha B'Av")))
    (when h
      (let ((date (caar h)))
        (if (= (calendar-day-of-week date) 6) ; Saturday
            (list
             (list
              (calendar-gregorian-from-absolute
               (1+ (calendar-absolute-from-gregorian date)))
              "Tisha B'Av (observed)"))
          h)))))

;;; Additional holiday calculations and observances
;;
;; Custom calculations, helper functions, and observances not provided
;; by the standard Emacs holiday libraries.

(defun holiday-hebrew-fast-of-esther ()
  "Return Fast of Esther, moved earlier if it would fall on Shabbat."
  (let ((h (holiday-hebrew 12 13 "Fast of Esther")))
    (when h
      (let ((date (caar h)))
        (if (= (calendar-day-of-week date) 6) ; Saturday
            (list
             (list
              (calendar-gregorian-from-absolute
               (- (calendar-absolute-from-gregorian date) 2))
              "Fast of Esther (observed)"))
          h)))))

(defun holiday-hebrew-yom-hashoah ()
  "Return Yom HaShoah, moved away from Shabbat.

Yom HaShoah normally falls on 27 Nisan.  If that date is
Friday, observe it on Thursday; if Sunday, observe it on Monday."
  (let ((h (holiday-hebrew 1 27 "Holocaust Memorial Day")))
    (when h
      (let* ((date (caar h))
             (offset (pcase (calendar-day-of-week date)
                       (5 -1)              ; Friday → Thursday
                       (0  1)              ; Sunday → Monday
                       (_  0))))
        (if (zerop offset)
            h
          (list
           (list
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian date)
                offset))
            "Holocaust Memorial Day (observed)")))))))

(defconst chinese-zodiac-elements
["Wood" "Wood" "Fire" "Fire" "Earth" "Earth"
 "Metal" "Metal" "Water" "Water"])

(defconst chinese-zodiac-animals
["Rat" "Ox" "Tiger" "Rabbit" "Dragon" "Snake"
 "Horse" "Goat" "Monkey" "Rooster" "Dog" "Pig"])

(defun chinese-zodiac-for-year (year)
"Return Chinese zodiac element and animal for Chinese year beginning in YEAR."
(let* ((n (+ year 57))
       (stem-index (% (1- n) 10))
       (branch-index (% (1- n) 12)))
  (format "%s %s"
          (aref chinese-zodiac-elements stem-index)
          (aref chinese-zodiac-animals branch-index))))

(defun holiday-chinese-new-years-eve ()
"Return Chinese New Year's Eve, the day before Chinese New Year."
(let ((h (holiday-chinese-new-year)))
  (when h
    (mapcar
     (lambda (entry)
       (let* ((date (car entry))
              (abs (calendar-absolute-from-gregorian date))
              (eve (calendar-gregorian-from-absolute (1- abs))))
         (list eve "Chinese New Year's Eve")))
     h))))

(defun holiday-chinese-new-year-with-zodiac ()
"Return Chinese New Year with element and animal."
(let ((h (holiday-chinese-new-year)))
  (when h
    (mapcar
     (lambda (entry)
       (let* ((date (car entry))
              (year (nth 2 date)))
         (list date
               (format "Chinese New Year (%s)"
                       (chinese-zodiac-for-year year)))))
     h))))

(defun solar-equinoxes-only ()
  "Return only equinox entries from `solar-equinoxes-solstices'."
  ;; Keep equinoxes, drop solstices (preserve upstream time formatting)
  (let (out)
    (dolist (entry (solar-equinoxes-solstices) (nreverse out))
      (let ((label (cadr entry)))
        (when (and (stringp label)
                   (string-match-p "Equinox" label))
          (push entry out))))))

(defun holiday-galactic-tick-day ()
  "Return Galactic Tick Day for the displayed year, if one occurs.

Galactic Tick Day marks each centi-arcsecond of the Solar System's
orbit around the Milky Way.  This implementation follows the modern
published civil-date sequence, anchored on Galactic Tick Day 237
(21 March 2020), and projects backward and forward at 634-day intervals.

Published explanations describe the interval in theoretical terms, but
civil dates may reflect rounding conventions, event times near midnight,
time-zone conversions, leap-year effects, or the general tendency of the
International Date Line to leave astronomers slightly unhinged."
  (let* ((interval 634)
         (anchor-tick 237)
         (anchor-abs (calendar-absolute-from-gregorian '(3 21 2020)))
         (year-start (calendar-absolute-from-gregorian
                      (list 1 1 displayed-year)))
         (year-end (calendar-absolute-from-gregorian
                    (list 12 31 displayed-year)))
         (k-start (floor (/ (- year-start anchor-abs) interval)))
         (k-end (ceiling (/ (- year-end anchor-abs) interval)))
         out)
    (dotimes (i (1+ (- k-end k-start)))
      (let* ((k (+ k-start i))
             (tick (+ anchor-tick k))
             (abs (+ anchor-abs (* k interval))))
        (when (and (<= year-start abs)
                   (<= abs year-end))
          (push
           (list (calendar-gregorian-from-absolute abs)
                 (format "Galactic Tick Day %d" tick))
           out))))
    (holiday-filter-visible-calendar (nreverse out))))

;; archives weeks -- International and local
(defun archives-week-date (year)
  "Return the beginning of International Archives Week in YEAR.

International Archives Day is June 9.  International Archives
Week begins on the Monday of the ISO-style week containing June 9."
  (let* ((iad (list 6 9 year))
         (dow (calendar-day-of-week iad)))
    (calendar-gregorian-from-absolute
     (- (calendar-absolute-from-gregorian iad)
        (mod (1- dow) 7)))))

(defun archives-awareness-week-date (year)
  "Return the beginning of Archives Awareness Week in YEAR.

Archives Awareness Week normally begins on the first Monday in April.
If that Monday is Easter Monday, the observance begins on the following
Tuesday instead."
  (let* ((first-monday
          (calendar-gregorian-from-absolute
           (calendar-dayname-on-or-before
            1
            (+ 6 (calendar-absolute-from-gregorian
                  (list 4 1 year))))))
         (easter-monday
          (calendar-gregorian-from-absolute
           (1+ (holiday-easter-etc-abs year)))))
    (if (equal first-monday easter-monday)
        (calendar-gregorian-from-absolute
         (1+ (calendar-absolute-from-gregorian first-monday)))
      first-monday)))

;; holiday forms
(defun holiday-archives-week ()
  "Return the beginning of International Archives Week."
  (holiday-filter-visible-calendar
   (list
    (list (archives-week-date displayed-year)
          "International Archives Week begins"))))

(defun holiday-archives-awareness-week ()
  "Return the beginning of Archives Awareness Week."
  (holiday-filter-visible-calendar
   (list
    (list (archives-awareness-week-date displayed-year)
          "Archives Awareness Week begins"))))

;; diary forms of same
(defun diary-archives-week ()
  "Return non-nil on the beginning of International Archives Week."
  (equal date
         (archives-week-date
          (calendar-extract-year date))))

(defun diary-archives-awareness-week ()
  "Return non-nil on the beginning of Archives Awareness Week."
  (equal date
         (archives-awareness-week-date
          (calendar-extract-year date))))

;;; Solar zodiac ingress calculations

(defconst holiday-zodiacal-marker-names
  '("Aquarius" "Pisces" "Aries" "Taurus"
    "Gemini" "Cancer" "Leo" "Virgo"
    "Libra" "Scorpio" "Sagittarius" "Capricorn")
  "Names of the zodiacal signs, indexed by Gregorian month.")

(defun holiday-zodiacal-marker ()
  "Return zodiacal ingress dates and times visible in the calendar."
  (let (result)
    (dolist (month (number-sequence 1 12))
      (when-let* ((year (calendar-month-visible-p month))
                  (start
                   (calendar-astro-from-absolute
                    (calendar-absolute-from-gregorian
                     (list month 1 year))))
                  (jd (solar-date-next-longitude start 30))
                  (abs-day (calendar-astro-to-absolute jd))
                  (date (calendar-gregorian-from-absolute
                         (floor abs-day)))
                  (name (nth (1- month)
                             holiday-zodiacal-marker-names)))
        (push
         (list date
               (format "%s %s"
                       name
                       (solar-time-string
                        (* 24 (- abs-day (floor abs-day)))
                        (if (dst-in-effect abs-day)
                            calendar-daylight-time-zone-name
                          calendar-standard-time-zone-name))))
         result)))
    (nreverse result)))

(defvar holiday-zodiacal-markers
  '((holiday-zodiacal-marker))
  "Zodiacal ingress dates and times.")

;;; Holiday lists and observances

(defvar scottish-quarter-days
  '((holiday-fixed  2  2 "Candlemas")
    (holiday-fixed  5 15 "Whitsun")
    (holiday-fixed  8  1 "Lammas")
    (holiday-fixed 11 11 "Martinmas"))
  "Holiday forms for the Scottish quarter days.")

(defvar holiday-scottish-observances
  (append
   '((holiday-fixed  1 25 "Robert Burns Day")
     (holiday-fixed 11 30 "St. Andrew's Day")
     (holiday-fixed 12 31 "Hogmanay"))
   scottish-quarter-days)
  "Scottish cultural and traditional observances, including quarter days.")

(defvar british-quarter-cross-days
  '((holiday-fixed  2  2 "Candlemas")
    (holiday-fixed  3 25 "Lady Day")
    (holiday-fixed  5  1 "May Day")
    (holiday-fixed  6 24 "Johnmas")
    (holiday-fixed  8  1 "Lammas")
    (holiday-fixed  9 29 "Michaelmas")
    (holiday-fixed 11  1 "All Saints'")
    (holiday-fixed 12 25 "Nativity"))
  "British quarter and cross-quarter days.")

(defvar holiday-roman-observances
  '((holiday-fixed 1 13  "Ides of January")
    (holiday-fixed 2 13  "Ides of February")
    (holiday-fixed 3 15  "Ides of March")
    (holiday-fixed 4 13  "Ides of April")
    (holiday-fixed 5 15  "Ides of May")
    (holiday-fixed 6 13  "Ides of June")
    (holiday-fixed 7 15  "Ides of July")
    (holiday-fixed 8 13  "Ides of August")
    (holiday-fixed 9 13  "Ides of September")
    (holiday-fixed 10 15 "Ides of October")
    (holiday-fixed 11 13 "Ides of November")
    (holiday-fixed 12 13 "Ides of December"))
  "Roman calendrical observances.")

(provide 'calendar-holiday-extras)
;;; calendar-holiday-extras.el ends here

; LocalWords:  Johnmas
