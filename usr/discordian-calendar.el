;;; discordian-calendar.el --- Discordian calendar conversions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Convert dates between the Gregorian and Discordian calendars.
;;
;; An ordinary Discordian date is represented as:
;;
;;     (SEASON DAY YEAR)
;;
;; SEASON is one of `chaos', `discord', `confusion', `bureaucracy',
;; or `aftermath'.  YEAR is expressed in YOLD.
;;
;; St. Tib's Day is represented as:
;;
;;     (st-tibs-day YEAR)
;;
;; With no date supplied, commands referring to the current date use
;; the local civil date.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup discordian-calendar nil
  "The Discordian calendar."
  :group 'calendar)

(defconst discordian-calendar-year-offset 1166
  "Number added to a Gregorian year to obtain its YOLD year.")

(defconst discordian-calendar-seasons
  [chaos discord confusion bureaucracy aftermath]
  "Discordian seasons in calendar order.")

(defconst discordian-calendar-season-names
  ["Chaos" "Discord" "Confusion" "Bureaucracy" "The Aftermath"]
  "Display names of the Discordian seasons.")

(defconst discordian-calendar-weekdays
  ["Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle"
   "Setting Orange"]
  "Discordian weekdays in calendar order.")

(defconst discordian-calendar-holydays
  '(((chaos 5) . "Mungday")
    ((chaos 50) . "Chaoflux")
    ((discord 5) . "Mojoday")
    ((discord 50) . "Discoflux")
    ((confusion 5) . "Syaday")
    ((confusion 50) . "Confuflux")
    ((bureaucracy 5) . "Zaraday")
    ((bureaucracy 50) . "Bureflux")
    ((aftermath 5) . "Maladay")
    ((aftermath 50) . "Afflux"))
  "Discordian Apostle and Season Holydays.")

(defun discordian-calendar--season-index (season)
  "Return the zero-based index of Discordian SEASON, or nil."
  (cl-position season discordian-calendar-seasons))

(defun discordian-calendar-season-name (season)
  "Return the display name of Discordian SEASON."
  (let ((index (discordian-calendar--season-index season)))
    (unless index
      (signal 'wrong-type-argument
              (list '(member chaos discord confusion bureaucracy aftermath)
                    season)))
    (aref discordian-calendar-season-names index)))

(defun discordian-calendar-valid-date-p (date)
  "Return non-nil when DATE is a valid Discordian date."
  (pcase date
    (`(st-tibs-day ,year)
     (and (integerp year)
          (calendar-leap-year-p
           (- year discordian-calendar-year-offset))))
    (`(,season ,day ,year)
     (and (discordian-calendar--season-index season)
          (integerp day)
          (<= 1 day 73)
          (integerp year)))
    (_ nil)))

(defun discordian-calendar-from-gregorian (date)
  "Convert Gregorian DATE to a Discordian date.

DATE uses Emacs calendar ordering: (MONTH DAY YEAR)."
  (pcase-let ((`(,month ,day ,year) date))
    (if (and (= month 2)
             (= day 29))
        (list 'st-tibs-day
              (+ year discordian-calendar-year-offset))
      (let* ((absolute
              (calendar-absolute-from-gregorian date))
             (year-start
              (calendar-absolute-from-gregorian (list 1 1 year)))
             (offset (- absolute year-start))
             (adjusted-offset
              (if (and (calendar-leap-year-p year)
                       (> month 2))
                  (1- offset)
                offset))
             (season-index (/ adjusted-offset 73))
             (season-day (1+ (mod adjusted-offset 73))))
        (list (aref discordian-calendar-seasons season-index)
              season-day
              (+ year discordian-calendar-year-offset))))))

(defun discordian-calendar-to-gregorian (date)
  "Convert Discordian DATE to a Gregorian date.

Return the Gregorian date in Emacs calendar ordering."
  (unless (discordian-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'discordian-calendar-valid-date-p date)))
  (pcase date
    (`(st-tibs-day ,year)
     (list 2 29 (- year discordian-calendar-year-offset)))
    (`(,season ,day ,year)
     (let* ((gregorian-year
             (- year discordian-calendar-year-offset))
            (season-index
             (discordian-calendar--season-index season))
            (offset
             (+ (* season-index 73) (1- day)))
            (gregorian-offset
             (if (and (calendar-leap-year-p gregorian-year)
                      (>= offset 59))
                 (1+ offset)
               offset)))
       (calendar-gregorian-from-absolute
        (+ (calendar-absolute-from-gregorian
            (list 1 1 gregorian-year))
           gregorian-offset))))))

(defun discordian-calendar-weekday (date)
  "Return the weekday name for Discordian DATE.

Return nil for St. Tib's Day, which is outside the Discordian week."
  (unless (discordian-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'discordian-calendar-valid-date-p date)))
  (pcase date
    (`(st-tibs-day ,_) nil)
    (`(,season ,day ,_)
     (let ((offset
            (+ (* (discordian-calendar--season-index season) 73)
               (1- day))))
       (aref discordian-calendar-weekdays (mod offset 5))))))

(defun discordian-calendar-holyday (date)
  "Return the holyday name for Discordian DATE, or nil."
  (unless (discordian-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'discordian-calendar-valid-date-p date)))
  (pcase date
    (`(st-tibs-day ,_) "St. Tib's Day")
    (`(,season ,day ,_)
     (alist-get (list season day)
                discordian-calendar-holydays
                nil nil #'equal))))

(defun discordian-calendar--ordinal (number)
  "Return NUMBER formatted as an English ordinal."
  (format "%d%s"
          number
          (if (memq (% number 100) '(11 12 13))
              "th"
            (pcase (% number 10)
              (1 "st")
              (2 "nd")
              (3 "rd")
              (_ "th")))))

(defun discordian-calendar-format-date (date)
  "Format Discordian DATE as a human-readable string."
  (unless (discordian-calendar-valid-date-p date)
    (signal 'wrong-type-argument
            (list 'discordian-calendar-valid-date-p date)))
  (pcase date
    (`(st-tibs-day ,year)
     (format "St. Tib's Day, YOLD %d" year))
    (`(,season ,day ,year)
     (let ((holyday (discordian-calendar-holyday date)))
       (format "%s, the %s day of %s in the YOLD %d%s"
               (discordian-calendar-weekday date)
               (discordian-calendar--ordinal day)
               (discordian-calendar-season-name season)
               year
               (if holyday
                   (format " — %s" holyday)
                 ""))))))

(defun discordian-calendar-date-string (&optional gregorian-date)
  "Return the Discordian equivalent of GREGORIAN-DATE as a string.

GREGORIAN-DATE defaults to the current local civil date."
  (discordian-calendar-format-date
   (discordian-calendar-from-gregorian
    (or gregorian-date
        (calendar-current-date)))))

;;;###autoload
(defun discordian-calendar-show-date ()
  "Display the current local Discordian date."
  (interactive)
  (message "%s" (discordian-calendar-date-string)))

;;;###autoload
(defun discordian-calendar-show-date-at-point ()
  "Display the Discordian equivalent of the Calendar date at point."
  (interactive)
  (unless (derived-mode-p 'calendar-mode)
    (user-error "This command must be used in a Calendar buffer"))
  (message "%s"
           (discordian-calendar-date-string
            (calendar-cursor-to-date t))))

;;;###autoload
(defun discordian-calendar-insert-date (&optional gregorian-date)
  "Insert the Discordian equivalent of GREGORIAN-DATE.

GREGORIAN-DATE defaults to the current local civil date."
  (interactive)
  (insert (discordian-calendar-date-string gregorian-date)))

(provide 'discordian-calendar)

;;; discordian-calendar.el ends here
