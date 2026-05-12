;;; local-holidays.el --- Set locally observed holidays and commemorations

;;; commentary:
;; Guard against non-visible holidays returning nil in calendar windows.
;; Use displayed-year/displayed-month-aware helpers for computed holidays.

;;; code:
(require 'calendar)
(require 'holidays)
(require 'cal-china)
(require 'cal-hebrew)
(require 'lunar)
(require 'solar)
(require 'time)
(defvar displayed-year)
(defvar displayed-month)

(message "→ Setting holidays and time defaults.")

(defun holiday-bahai-naw-ruz ()
  "Return Bahá’í New Year for the displayed year."
  (when (memq displayed-month '(3 4))
    (let* ((y displayed-year)
           (day (floor (nth 1 (solar-equinoxes/solstices 1 y)))))
      (holiday-filter-visible-calendar
       (list
        (list (list 3 day y)
              (format "Bahá’í New Year (Naw-Rúz) %d" (- y (1- 1844)))))))))

(defun holiday-hebrew-rosh-hashanah (&optional all)
  "List of dates related to Rosh Hashana, as visible in calendar window.
Shows only the major holidays, unless `calendar-hebrew-all-holidays-flag'
or ALL is non-nil."
  (when (memq displayed-month '(8 9 10 11))
    (let ((abs-r-h (calendar-hebrew-to-absolute
                    (list 7 1 (+ displayed-year 3761)))))
      (holiday-filter-visible-calendar
       (append
        (list
         (list (calendar-gregorian-from-absolute abs-r-h)
               (format "Rosh Hashana %d" (+ 3761 displayed-year)))
         (list (calendar-gregorian-from-absolute (+ abs-r-h 9))
               "Yom Kippur")
         (list (calendar-gregorian-from-absolute (+ abs-r-h 14))
               "Sukkot")))))))

(defun holiday-hebrew-tisha-b-av ()
  "Return only Tisha B'Av, observed on 10 Av if 9 Av falls on Shabbat."
  (let ((h (holiday-hebrew 5 9 "Tisha B'Av")))
    (when h
      (let ((date (caar h)))
        (if (= (calendar-day-of-week date) 6)
            (list
             (list
              (calendar-gregorian-from-absolute
               (1+ (calendar-absolute-from-gregorian date)))
              "Tisha B'Av (observed)"))
          h)))))

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
         (list eve "Chinese New Year's Eve 除夕")))
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
               (format "Chinese New Year (%s) 春節"
                       (chinese-zodiac-for-year year)))))
     h))))

(defun holiday-chinese-qingming-with-native-name ()
"Return Qingming with English and Chinese names."
(let ((h (holiday-chinese-qingming)))
  (when h
    (mapcar
     (lambda (entry)
       (list (car entry)
             "Qingming Festival 清明"))
     h))))

(defun scottish-quarter-days ()
  "Return Scottish Quarter Days."
  (append
   (holiday-fixed 2  2  "Candlemas (¼)")
   (holiday-fixed 5 15  "Whitsun (¼)")
   (holiday-fixed 8  1  "Lammas (¼)")
   (holiday-fixed 11 11 "Martinmas (¼)")))

(defun solar-equinoxes-only ()
  "Return only equinox entries from `solar-equinoxes-solstices'."
  ;; Keep equinoxes, drop solstices (preserve upstream time formatting)
  (let (out)
    (dolist (entry (solar-equinoxes-solstices) (nreverse out))
      (let ((label (cadr entry)))
        (when (and (stringp label)
                   (string-match-p "Equinox" label))
          (push entry out))))))

;; -- Set holiday variables --
;; Static holiday lists vs computed holiday generators (solar, Bahá’í, etc.)
(let ((inhibit-message t))
  (setopt
   holiday-general-holidays nil
   calendar-chinese-all-holidays-flag t
   calendar-christian-all-holidays-flag t
   world-clock-time-format "%a %e %b %R %Z"

   holiday-local-holidays ;; National / Provincial Holidays and Commemorations
   '((holiday-fixed 1 1    "New Year's Day")
     (holiday-fixed 2 2    "Groundhog Day")
     (holiday-fixed 2 14   "Valentine's Day")
     (holiday-fixed 4 1    "April Fools' Day")
     (holiday-float 5 0 2  "Mother's Day")
     (holiday-float 6 0 3  "Father's Day")
     (holiday-fixed 7 1    "Canada Day")
     (holiday-float 8 1 1  "Civic Holiday")
     (holiday-float 9 1 1  "Labour Day")
     (holiday-float 10 1 2 "Thanksgiving")
     (holiday-fixed 10 31  "Halloween")
     (holiday-fixed 11 11  "Remembrance Day")
     (holiday-fixed 12 26  "Boxing Day")

     (holiday-fixed 1 21   "Lincoln Alexander Day")
     (holiday-float 2 1 3  "Family Day")
     (holiday-fixed 2 15   "National Flag Day")
     (holiday-float 3 1 2  "Commonwealth Day")
     (holiday-fixed 4 6    "Tartan Day")
     (holiday-fixed 4 9    "Vimy Ridge Day")
     (holiday-fixed 6 21   "Indigenous Peoples Day")
     (holiday-fixed 9 30   "Truth and Reconciliation")
     (holiday-fixed 12 11  "Statute of Westminster")
     (holiday-fixed 12 31  "Hogmanay"))

   holiday-other-holidays
     ;; third Monday of January
   '((holiday-float 1 1 3 "Martin Luther King Day")

     ;; first Saturday of June following the Summer Solstice
     (holiday-float 6 6 1 "Midsummer"
                    (floor (nth 1 (solar-equinoxes/solstices 1 displayed-year))))

     ;; first Tuesday in November after the first Monday, every four even-numbered years
     (holiday-sexp
      '(if (zerop (% year 4))
           (calendar-gregorian-from-absolute
            (1+ (calendar-dayname-on-or-before 1
                 (+ 6 (calendar-absolute-from-gregorian (list 11 1 year)))))))
      "US Presidential Election")

     (holiday-float 11 0 2 "Remembrance Sunday")

     ;; first Tuesday in November after the first Monday, every four years,
     ;; offset by two from presidential elections
     (holiday-sexp
      '(if (= (% year 4) 2)
           (calendar-gregorian-from-absolute
            (1+ (calendar-dayname-on-or-before 1
                 (+ 6 (calendar-absolute-from-gregorian (list 11 1 year)))))))
      "US Midterm Election")

     ;; Black Friday follows US Thanksgiving (4th Thursday/Friday of November)
     (holiday-float 11 4 4 "US Thanksgiving")
     (holiday-float 11 5 4 "Black Friday")

     (holiday-fixed 12 (floor (nth 1 (solar-equinoxes/solstices 3 displayed-year)))
      "Midwinter")
     (scottish-quarter-days)

     ;; HACK -- add to 'calendar-holidays'?
     ;;      -- create 'holiday-buddhist-holidays' variable
     (holiday-buddhist))

   holiday-bahai-holidays
   '((holiday-bahai-naw-ruz))

   holiday-christian-holidays
   '((holiday-easter-etc)
     (holiday-easter-etc 1 "Easter Monday")
     (holiday-fixed 11 1 "All Saints' Day")
     (holiday-fixed 11 2 "All Souls' Day")
     (holiday-advent -11 "Prayer & Repentance")
     (holiday-fixed 12 25 "Christmas")
     (if calendar-christian-all-holidays-flag
         (append
          (holiday-fixed 1 6 "Epiphany")
          (holiday-greek-orthodox-easter)
          (holiday-advent 0 "Advent"))))

   holiday-hebrew-holidays
   '((holiday-hebrew-fast-of-esther)
     (holiday-hebrew-passover)
     (holiday-hebrew-tisha-b-av)
     (holiday-hebrew-rosh-hashanah)
     (holiday-hebrew 9 25 "Chanukah"))

   holiday-islamic-holidays
   '((holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan Begins")
     (holiday-islamic 10 1 "Id-al-Fitr"))

   holiday-oriental-holidays
   '((holiday-chinese-new-years-eve)
     (holiday-chinese-new-year-with-zodiac)
     (if calendar-chinese-all-holidays-flag
         (append
          (holiday-chinese 1 15 "Lantern Festival 元宵")
          (holiday-chinese-qingming-with-native-name)
          (holiday-chinese 5 5 "Dragon Boat Festival 端午")
          (holiday-chinese 7 7 "Double Seventh Festival 七夕")
          (holiday-chinese 7 15 "Ghost Festival 中元")
          (holiday-chinese 8 15 "Mid-Autumn Festival 中秋")
          (holiday-chinese 9 9 "Double Ninth Festival 重阳"))))

   lunar-phase-names
   '("New Moon" "First Qtr" "Full Moon" "Last Qtr")

   holiday-solar-holidays
   '((solar-equinoxes-only)
     (holiday-sexp calendar-daylight-savings-starts
                   (format "Daylight Saving Time Begins %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-starts-time (float 60))
                            calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
                   (format "Daylight Saving Time Ends %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-ends-time (float 60))
                            calendar-daylight-time-zone-name))))

   zoneinfo-style-world-list
   '(("Pacific/Honolulu" "Hawai'i")
     ("America/Los_Angeles" "Cupertino")
     ("America/Vancouver" "Vancouver")
     ("America/Edmonton" "Edmonton")
     ("America/Regina" "Saskatoon")
     ("America/Winnipeg" "Winnipeg")
     ("America/Toronto" "Ottawa")
     ("America/Halifax" "Halifax")
     ("America/St_Johns" "St. John's")
     ("America/Marigot" "St. Martin")
     ("UTC" "UTC")
     ("Europe/London" "Edinburgh")
     ("Europe/Lisbon" "Lisbon")
     ("Europe/Paris" "Paris")
     ("Europe/Rome" "Rome")
     ("Europe/Istanbul" "Ankara")
     ("Asia/Kolkata" "New Delhi")
     ("Asia/Shanghai" "Beijing")
     ("Asia/Tokyo" "Tokyo")
     ("Australia/Sydney" "Sydney")
     ("NZ" "Wellington"))))

(setopt calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays
                holiday-christian-holidays
                holiday-hebrew-holidays
                holiday-islamic-holidays
                holiday-bahai-holidays
                holiday-oriental-holidays
                holiday-solar-holidays))

(provide 'local-holidays)
;;; local-holidays.el ends here
