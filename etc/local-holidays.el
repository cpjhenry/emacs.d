;;; local-holidays.el --- Set locally observed holidays and commemorations -*- lexical-binding: t; -*-

;;; Commentary:

;; Guard against non-visible holidays returning nil in calendar windows.
;; Use displayed-year/displayed-month-aware helpers for computed holidays.

;;; Code:
(require 'calendar)
(require 'holidays)
(require 'cal-china)
(require 'cal-hebrew)
(require 'cal-tex)
(require 'lunar)
(require 'solar)
(require 'time)
(defvar displayed-year)
(defvar displayed-month)
(defvar holiday-buddhist-holidays)
(defvar holiday-mercury-retrograde)

(message "→ Setting holidays and time defaults.")

;; Replacement holiday definitions
;;
;; These functions replace or simplify selected holiday definitions
;; supplied by Emacs. They either correct observance rules, improve
;; calculations, or reduce output to the observances included in
;; this calendar.

(defun holiday-bahai-naw-ruz ()
  "Return Bahá’í New Year for the displayed year.

Naw-Rúz is formally defined by the Bahá’í calendar as occurring at the
Vernal Equinox as observed in Tehran. Earlier Emacs holiday definitions
approximated this by fixing the observance on 21 March.

This implementation improves upon that approximation by deriving the
date from the calculated Vernal Equinox for the displayed year and using
the resulting day in March."
  (when (memq displayed-month '(3 4))
    (let* ((y displayed-year)
           (day (floor (nth 1 (solar-equinoxes/solstices 1 y)))))
      (holiday-filter-visible-calendar
       (list
        (list (list 3 day y)
              (format "Bahá’í New Year (Naw-Rúz) %d"
                      (- y (1- 1844)))))))))

;; Prefer Hebcal-style English transliterations for Hebrew month names.
;; https://www.hebcal.com/
(with-eval-after-load 'cal-hebrew
  (setq calendar-hebrew-month-name-array-common-year
        ["Nisan" "Iyyar" "Sivan" "Tamuz" "Av" "Elul"
         "Tishrei" "Cheshvan" "Kislev" "Tevet" "Sh'vat" "Adar"]
        calendar-hebrew-month-name-array-leap-year
        ["Nisan" "Iyyar" "Sivan" "Tamuz" "Av" "Elul"
         "Tishrei" "Cheshvan" "Kislev" "Tevet" "Sh'vat"
         "Adar I" "Adar II"]))

(defun holiday-hebrew-rosh-hashanah (&optional _all)
  "Return major autumn observances for the displayed year.

This is a simplified variant of the corresponding Emacs holiday
function. The original implementation includes a broader selection of
observances and optional detail controlled by
`calendar-hebrew-all-holidays-flag'.

The spelling \"Rosh Hashana\" follows the convention used by `hebcal'
and related calendar references."
  (when (memq displayed-month '(8 9 10 11))
    (let ((abs-r-h (calendar-hebrew-to-absolute
                    (list 7 1 (+ displayed-year 3761)))))
      (holiday-filter-visible-calendar
       (list
        (list (calendar-gregorian-from-absolute abs-r-h)
              (format "Rosh Hashana %d" (+ 3761 displayed-year)))
        (list (calendar-gregorian-from-absolute (+ abs-r-h 9))
              "Yom Kippur")
        (list (calendar-gregorian-from-absolute (+ abs-r-h 14))
              "Sukkot"))))))

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


;; Additional holiday calculations and observances
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

(defun scottish-quarter-days ()
  "Return Scottish Quarter Days."
  (append
   (holiday-fixed 2  2  "Candlemas")
   (holiday-fixed 5 15  "Whitsun")
   (holiday-fixed 8  1  "Lammas")
   (holiday-fixed 11 11 "Martinmas")))

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

(defun holiday-archives-week ()
  "Return the beginning of International Archives Week.
International Archives Day is June 9.  International Archives
Week begins on the Monday of the ISO-style week containing June 9."
  (let* ((iad (list 6 9 displayed-year))
         (dow (calendar-day-of-week iad)) ; 0 Sunday ... 6 Saturday
         (start (calendar-gregorian-from-absolute
                 (- (calendar-absolute-from-gregorian iad)
                    (mod (1- dow) 7)))))
    (holiday-filter-visible-calendar
     (list
      (list start "International Archives Week begins")))))

(defun holiday-archives-awareness-week ()
  "Return the beginning of Archives Awareness Week.

Archives Awareness Week normally begins on the first Monday in April.
If that Monday is Easter Monday, the observance begins on the following
Tuesday instead."
  (let* ((year displayed-year)
         (first-monday
          (calendar-gregorian-from-absolute
           (calendar-dayname-on-or-before
            1
            (+ 6 (calendar-absolute-from-gregorian
                  (list 4 1 year))))))
         (easter-monday
          (caar (holiday-easter-etc 1 "Easter Monday")))
         (start
          (if (equal first-monday easter-monday)
              (calendar-gregorian-from-absolute
               (1+ (calendar-absolute-from-gregorian first-monday)))
            first-monday)))
    (holiday-filter-visible-calendar
     (list
      (list start "Archives Awareness Week begins")))))


;; -- Set holiday variables --
;; Static holiday lists vs computed holiday generators (solar, Bahá’í, etc.)

(let ((inhibit-message t))
  (setopt
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
     (holiday-fixed 3 18   "Sheelagh's Day")
     (holiday-fixed 4 6    "Tartan Day")
     (holiday-fixed 4 9    "Vimy Ridge Day")
     (holiday-float 5 0 1  "Bereaved Mother's Day")
     (holiday-fixed 6 21   "Indigenous Peoples Day")
     (holiday-fixed 6 24   "Saint-Jean-Baptiste")
     (holiday-float 7 0 -1 "Garland Sunday")
     (holiday-fixed 9 30   "Truth and Reconciliation")
     (holiday-float 11 0 2 "Remembrance Sunday")
     (holiday-fixed 12 11  "Statute of Westminster"))

   holiday-general-holidays ;; US Holidays and Commemorations
   '(;; third Monday of January
     (holiday-float 1 1 3 "Martin Luther King Day")
     ;; first Tuesday in November after the first Monday, every four even-numbered years
     (holiday-sexp
      '(if (zerop (% year 4))
           (calendar-gregorian-from-absolute
            (1+ (calendar-dayname-on-or-before 1
                 (+ 6 (calendar-absolute-from-gregorian (list 11 1 year)))))))
      "US Presidential Election")
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
     (holiday-float 11 5 4 "Black Friday"))

   holiday-other-holidays nil

   holiday-bahai-holidays
  '((holiday-bahai-new-year)
    (if calendar-bahai-all-holidays-flag
        (append
	 (holiday-bahai-ridvan) ; respects calendar-bahai-all-holidays-flag
	 (holiday-fixed  5 23 "Declaration of the Báb")
	 (holiday-fixed  5 29 "Ascension of Bahá’u’lláh")
	 (holiday-fixed  7  9 "Martyrdom of the Báb")
	 (holiday-fixed 10 20 "Birth of the Báb")
	 (holiday-fixed 11 12 "Birth of Bahá’u’lláh")
         (holiday-fixed 11 26 "Day of the Covenant")
         (holiday-fixed 11 28 "Ascension of `Abdu’l-Bahá"))))

   holiday-christian-holidays
   '((holiday-easter-etc)
     (holiday-easter-etc 1 "Easter Monday")
     (holiday-fixed 12 25 "Christmas")
     (if calendar-christian-all-holidays-flag
         (append
	  (holiday-fixed 1 5 "Twelfth Night")
          (holiday-fixed 1 6 "Epiphany")
          (holiday-greek-orthodox-easter)
	  (holiday-fixed 11 1 "All Saints' Day")
	  (holiday-fixed 11 2 "All Souls' Day")
	  (holiday-advent -11 "Prayer & Repentance")
          (holiday-advent 0 "Advent"))))

   holiday-hebrew-holidays
   '((holiday-hebrew-fast-of-esther)
     (holiday-hebrew-passover)
     (holiday-hebrew-yom-hashoah)
     (holiday-hebrew-tisha-b-av)
     (holiday-hebrew-rosh-hashanah)
     (holiday-hebrew 9 25 "Chanukah"))

   holiday-islamic-holidays
   '((holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan Begins")
     (holiday-islamic 10 1 "Id-al-Fitr"))

   holiday-oriental-holidays
   '((holiday-chinese-new-year-with-zodiac) ; 春節
     (if calendar-chinese-all-holidays-flag
         (append
	  (holiday-chinese-new-years-eve) ; 除夕
          (holiday-chinese 1 15 "Lantern Festival") ; 元宵 / Last Day of New Year's Celebration
          (holiday-chinese-qingming) ; 清明 / Tomb-Sweeping Day
          (holiday-chinese 5 5 "Dragon Boat Festival") ; 端午 / Duanwu Festival
          (holiday-chinese 7 7 "Double Seventh Festival") ; 七夕 / Valentine's Day
          (holiday-chinese 7 15 "Ghost Festival") ; 中元 / All Hallows' Eve
          (holiday-chinese 8 15 "Mid-Autumn Festival") ; 中秋 / Thanksgiving
          (holiday-chinese 9 9 "Double Ninth Festival")))) ; 重阳 / Double Yang Festival

   lunar-phase-names
   '("New Moon" "First Quarter" "Full Moon" "Last Quarter")

   holiday-solar-holidays nil

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


(defvar holiday-seasonal-observances
   '((solar-equinoxes-only)
     ;; first Saturday of June following the Summer Solstice
     (holiday-float 6 6 1 "Midsummer" (floor (nth 1 (solar-equinoxes/solstices 1 displayed-year))))
     (holiday-fixed 12 (floor (nth 1 (solar-equinoxes/solstices 3 displayed-year))) "Midwinter")
     (holiday-sexp calendar-daylight-savings-starts
                   (format "Daylight Saving Time Begins %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-starts-time (float 60))
                            calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
                   (format "Daylight Saving Time Ends %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-ends-time (float 60))
                            calendar-daylight-time-zone-name)))
     (holiday-mercury-retrograde))
   "Seasonal and astronomical observances.")

(defvar holiday-scottish-observances
  '((holiday-fixed 1 25  "Robert Burns Day")
    (holiday-fixed 11 30 "St. Andrew's Day")
    (holiday-fixed 12 31 "Hogmanay")
    (scottish-quarter-days))
  "Scottish cultural and traditional observances.")

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

;; calendar-holidays: What should be observed?
;;     Observances, commemorations, feast days,
;;     anniversaries that are actively celebrated.

;; diary: What should be remembered?
;;     Historical events, "on this day" notes,
;;     births, deaths, milestones, curiosities.

(defvar holiday-observances
  '((holiday-fixed 1 10   "Peculiar People Day")
    (holiday-fixed 1 19   "Dead of Winter")
    (holiday-fixed 1 23   "National Handwriting Day")
    (holiday-fixed 1 28   "Data Privacy Day")
    (holiday-float 1 4 4  "NASA Day of Remembrance")
    (holiday-fixed 1 31   "Hell is Freezing Over Day")

    (holiday-fixed 2 12   "Darwin Day")
    (holiday-float 2 3 -1 "Anti-Bullying Day")
    (holiday-fixed 2 20   "Clean Out Your Bookcase Day")
    (holiday-fixed 2 22   "Be Humble Day")
    (holiday-fixed 2 26   "For Pete's Sake Day")

    (holiday-fixed 3 3    "Talk in Third Person Day")
    (holiday-fixed 3 8    "International Women's Day")
    (holiday-fixed 3 11   "Pandemic Observance Day")
    (holiday-fixed 3 14   "Pi Day")
    (holiday-fixed 3 26   "Purple Day")

    (holiday-archives-awareness-week)
    (holiday-fixed 4 7    "International Beaver Day")
    (holiday-float 4 3 2  "Pink Day")
    (holiday-fixed 4 18   "World Amateur Radio Day")
    (holiday-fixed 4 22   "Earth Day")
    (holiday-fixed 4 28   "Workers' Memorial Day")

    (holiday-float 5 0 1  "Emergency Preparedness Week Begins")
    (holiday-fixed 5 4    "May the Fourth Be With You")
    (holiday-fixed 5 5    "Dutch National Heritage Day")
    (holiday-fixed 5 8    "Victory in Europe Day")
    (holiday-fixed 5 22   "Harvey Milk Day")
    (holiday-float 5 0 -1 "National Accessibility Week Begins")

    (holiday-float 6 5 1  "National Doughnut Day")
    (holiday-fixed 6 8    "Name Your Poison Day")
    (holiday-archives-week)
    (holiday-fixed 6 9    "International Archives Day")
    (holiday-fixed 6 20   "World Refugee Day")
    (holiday-fixed 6 27   "Canadian Multiculturalism Day")
    (holiday-fixed 6 28   "Tau Day")

    (holiday-fixed 7 14   "Bastille Day")

    (holiday-fixed 8 9    "National Peacekeepers' Day")
    (holiday-fixed 8 15   "Victory over Japan Day")
    (holiday-fixed 8 19   "World Photography Day")
    (holiday-fixed 8 23   "Black Ribbon Day")

    (holiday-float 9 6 3  "Batman Day")
    (holiday-fixed 9 19   "Talk Like a Pirate Day")
    (holiday-fixed 9 25   "One-Hit Wonder Day")
    (holiday-fixed 9 25   "Franco-Ontarian Day")

    (holiday-fixed 10 2   "Gandhi Jayanti")
    (holiday-fixed 10 3   "German Reunification Day")
    (holiday-fixed 10 6   "Mad Hatter Day")
    (holiday-fixed 10 14  "World Standards Day")
    (holiday-fixed 10 18  "Persons' Day")
    (holiday-float 10 3 3 "Global Ethics Day")

    (holiday-float 11 5 1 "Fountain Pen Day")
    (holiday-fixed 11 8   "Indigenous Veterans Day")
    (holiday-fixed 11 10  "Lost Mariners Remembrance")

    (holiday-fixed 12 5   "Day of the Ninja")

    (holiday-fixed 12 17  "Saturnalia")
    (holiday-fixed 12 23  "Festivus")
    (holiday-fixed 12 26  "Kwanzaa")

    (holiday-julian 1 1   "Old New Year")
    (holiday-julian 2 14  "Old St. Valentine's Day")

    (holiday-galactic-tick-day))
  "Observances.
(International, cultural, commemorative, professional, and special-interest)")

(defvar holiday-saints-days
  '((holiday-fixed 1 28  "St. Thomas Aquinas (Academics)")
    (holiday-fixed 2 5   "St. Agatha (Martyr)")
    (holiday-fixed 3 1   "St. David (Wales)")
    (holiday-fixed 3 16  "St. Urho (Finland)")
    (holiday-fixed 3 17  "St. Patrick (Ireland)")
    (holiday-fixed 3 19  "St. Joseph (Fathers)")
    (holiday-fixed 4 17  "St. Kateri Tekakwitha (Indigenous Peoples)")
    (holiday-fixed 4 23  "St. George (Soldiers)")
    (holiday-fixed 5 4   "St. Florian (Firefighters)")
    (holiday-fixed 5 30  "St. Joan of Arc (Martyr)")
    (holiday-fixed 6 13  "St. Anthony (Lost Souls & Items)")
    ;; (holiday-fixed 6 24 "St. John the Baptist") ; see Saint-Jean-Baptiste
    (holiday-fixed 6 29  "St. Peter (Apostle)")
    (holiday-fixed 7 11  "St. Benedict (Monasticism)")
    (holiday-fixed 7 17  "St. Nicholas Passion-Bearer (Martyr)")
    (holiday-fixed 7 25  "St. Christopher (Travellers)")
    (holiday-fixed 8 10  "St. Lawrence (Archives)")
    (holiday-fixed 8 28  "St. Augustine (Theologians)")
    (holiday-fixed 9 18  "St. Joseph of Cupertino (Astronauts)")
    (holiday-fixed 9 29  "St. Michael (Police)")
    (holiday-fixed 9 30  "St. Jerome (Libraries)")
    (holiday-fixed 10 16 "St. Marguerite d'Youville (Poverty)")
    (holiday-fixed 10 22 "St. John Paul II (Pope)")
    (holiday-fixed 10 25 "St. Minias (Martyr)")
    (holiday-fixed 11 30 "St. Andrew (Scotland)")
    (holiday-fixed 12 6  "St. Nicholas (Children)")
    (holiday-fixed 12 13 "St. Lucy (Festival of Light)")
    (holiday-fixed 12 26 "St. Stephen (Proto-martyr)")
    (holiday-fixed 12 31 "St. Sylvester (Pope)"))
  "Saints' days and patronal commemorations.")

(setopt calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
		holiday-scottish-observances
		holiday-roman-observances
		holiday-discordian-observances
                holiday-other-holidays
                holiday-christian-holidays
                holiday-hebrew-holidays
                holiday-islamic-holidays
                holiday-bahai-holidays
		holiday-buddhist-holidays
                holiday-oriental-holidays
		holiday-hindu-holidays
                holiday-seasonal-observances
		holiday-observances))

(defun cpj/holiday-available-holiday-lists (holiday-lists)
  "Add custom holiday categories to HOLIDAY-LISTS."
  (append
   holiday-lists
   (delq nil
         (list
	  (and holiday-scottish-observances
	       (cons "Scottish" holiday-scottish-observances))

	  (and holiday-roman-observances
	       (cons "Roman" holiday-roman-observances))

	  (and holiday-discordian-observances
	       (cons "Discordian" holiday-discordian-observances))

	  (and holiday-buddhist-holidays
	       (cons "Buddhist" holiday-buddhist-holidays))

	  (and holiday-hindu-holidays
	       (cons "Hindu" holiday-hindu-holidays))

	  (and holiday-seasonal-observances
	       (cons "Seasonal" holiday-seasonal-observances))

	  (and holiday-observances
	       (cons "Observances" holiday-observances))

	  (and holiday-saints-days
	       (cons "Saints" holiday-saints-days))
	  ))))

(remove-function (symbol-function 'holiday-available-holiday-lists)
		 #'cpj/holiday-available-holiday-lists)

(add-function :filter-return (symbol-function 'holiday-available-holiday-lists)
              #'cpj/holiday-available-holiday-lists)

(provide 'local-holidays)
;;; local-holidays.el ends here

; LocalWords:  bahai Deepavali Kartik Amavasya panchang दीपावली Iyyar
; LocalWords:  Jayanti Tamuz Tishrei Cheshvan Sh'vat amavasya Kartika
