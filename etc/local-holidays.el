;;; local-holidays.el --- Local holiday selections and calendar settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Personal holiday lists, display preferences and configuration.
;; Reusable holiday calculations live in `calendar-holiday-extras.el'.
;;
;; Guard against non-visible holidays returning nil in calendar windows.
;; Use displayed-year/displayed-month-aware helpers for computed holidays.

;; Holiday lists and diary entries serve different purposes.
;;
;; `calendar-holidays' answers:
;;     What is formally observed?
;;
;; Use holiday lists for recognized civic, religious, professional,
;; cultural, or institutional observances, including movable observances
;; and named days whose significance lies in their continuing observance.
;;
;; The diary answers:
;;     What should be remembered?
;;
;; Use the diary for specific historical events, anniversaries, births,
;; deaths, milestones, curiosities, and idiosyncratic "on this day" notes.
;;
;; Solemnity alone is not the deciding factor. A grave historical event
;; may still belong in the diary when it marks a specific occurrence,
;; while a lighter item may belong in a holiday list if it is genuinely
;; observed as a recurring named day.
;;
;; Example:
;;     Yom HaShoah -> holiday list: formal annual observance
;;     Kristallnacht -> diary: specific historical remembrance
;;
;; In short:
;;     observance -> holiday list
;;     remembrance -> diary

;;; Code:
(require 'calendar-holiday-extras)
(require 'cal-tex)
(require 'lunar)
(require 'time)

;; Supplied by separately loaded local calendar modules.
(defvar holiday-buddhist-holidays)
(defvar holiday-hindu-holidays)
(defvar holiday-discordian-observances)
(defvar holiday-mercury-retrograde)

(message "→ Setting holidays and time defaults.")

;;; Local spelling and selection overrides

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

(defun my/holiday-hebrew-rosh-hashanah ()
  "Return selected major holidays related to Rosh Hashanah.

The spelling \"Rosh Hashana\" follows the convention used by `hebcal'
and related calendar references."
  (mapcar
   (lambda (holiday)
     (if (string-prefix-p "Rosh HaShanah" (cadr holiday))
         (list (car holiday)
               (replace-regexp-in-string
                "\\`Rosh HaShanah"
                "Rosh Hashana"
                (cadr holiday)))
       holiday))
   (seq-remove
    (lambda (holiday)
      (member (cadr holiday)
              '("Shemini Atzeret"
                "Simchat Torah")))
    (holiday-hebrew-rosh-hashanah))))


;;; -- Set holiday variables --
;; Static holiday lists vs computed holiday generators (solar, Bahá’í, etc.)

(let ((inhibit-message t))
  (setopt
   calendar-chinese-all-holidays-flag t
   calendar-christian-all-holidays-flag t
   world-clock-time-format "%a %e %b %R %Z"

   holiday-local-holidays ;; (Canadian) National / Provincial Holidays and Commemorations
   '((holiday-fixed 1 1     "New Year's Day")
     (holiday-float 2 1 3   "Family Day")
     (holiday-float 5 1 -1  "Victoria Day" 24)
     (holiday-fixed 7 1     "Canada Day")
     (holiday-float 8 1 1   "Civic Holiday")
     (holiday-float 9 1 1   "Labour Day")
     (holiday-fixed 9 30    "Truth and Reconciliation")
     (holiday-float 10 1 2  "Thanksgiving")
     (holiday-fixed 11 11   "Remembrance Day")
     (holiday-fixed 12 26   "Boxing Day")

     (holiday-fixed 1 21    "Lincoln Alexander Day")
     (holiday-fixed 2 2     "Groundhog Day")
     (holiday-fixed 2 14    "Valentine's Day")
     (holiday-fixed 2 15    "Flag of Canada Day")
     (holiday-fixed 3 8     "Women's Day")
     (holiday-float 3 1 2   "Commonwealth Day")
     (holiday-fixed 3 18    "Sheelagh's Day")
     (holiday-fixed 4 1     "April Fools' Day")
     (holiday-fixed 4 6     "Tartan Day")
     (holiday-fixed 4 9     "Vimy Ridge Day")
     (holiday-fixed 4 22    "Earth Day")
     (holiday-float 5 0 1   "Bereaved Mother's Day")
     (holiday-float 5 0 2   "Mother's Day")
     (holiday-fixed 5 5     "Red Dress Day")
     (holiday-float 6 0 3   "Father's Day")
     (holiday-fixed 6 21    "Indigenous Peoples Day")
     (holiday-fixed 6 24    "Saint-Jean-Baptiste")
     (holiday-float 7 0 -1  "Garland Sunday")
     (holiday-fixed 8 9     "Peacekeepers' Day")
     (holiday-float 9 0 2   "Grandparents' Day")
     (holiday-fixed 10 31   "Halloween")
     (holiday-float 11 0 -1 "Remembrance Sunday" 14)
     (holiday-fixed 12 6    "White Ribbon Day")
     (holiday-fixed 12 11   "Statute of Westminster"))

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

   holiday-other-holidays
   '((holiday-fixed 12 26  "Kwanzaa")

     (holiday-julian 1 1  "Old New Year")
     (holiday-julian 2 14 "Old St. Valentine's Day"))

   holiday-bahai-holidays
  '((holiday-bahai-naw-ruz))

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
     (holiday-hebrew-passover) ; includes 'Shavuot'
     (holiday-hebrew-yom-hashoah)
     (holiday-hebrew-tisha-b-av)
     (my/holiday-hebrew-rosh-hashanah)
     (holiday-hebrew 9 25 "Chanukah"))

   holiday-islamic-holidays
   '((holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan")
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
          (holiday-chinese 9 9 "Double Ninth Festival");  ; 重阳 / Double Yang Festival
	  (holiday-chinese 10 1 "Winter Clothes Festival")))) ; 寒衣 / Hanyi Festival

   lunar-phase-names
   '("New Moon" "First Quarter" "Full Moon" "Last Quarter")

   holiday-solar-holidays
   '((holiday-galactic-tick-day))

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

     ;; Winter Solstice
     (holiday-fixed 12 (floor (nth 1 (solar-equinoxes/solstices 3 displayed-year))) "Midwinter")

     ;; Daylight Saving Time
     (holiday-sexp calendar-daylight-savings-starts
                   (format "Daylight Saving Time begins %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-starts-time (float 60))
                            calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
                   (format "Daylight Saving Time ends %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-ends-time (float 60))
                            calendar-daylight-time-zone-name)))

     (holiday-mercury-retrograde))
   "Seasonal and astronomical observances.")

(defvar holiday-saints-days
  '((holiday-fixed 1 28  "St. Thomas Aquinas (Academics)")
    (holiday-fixed 2 5   "St. Agatha")
    (holiday-fixed 3 1   "St. David (Wales)")
    (holiday-fixed 3 16  "St. Urho (Finland)")
    (holiday-fixed 3 17  "St. Patrick (Ireland)")
    (holiday-fixed 4 17  "St. Kateri Tekakwitha (Indigenous)")
    (holiday-fixed 4 23  "St. George (Soldiers)")
    (holiday-fixed 5 4   "St. Florian (Firefighters)")
    (holiday-fixed 5 30  "St. Joan of Arc")
    (holiday-fixed 6 13  "St. Anthony (Lost Souls)")
    ;(holiday-fixed 6 24 "St. John the Baptist") ; see Saint-Jean-Baptiste
    (holiday-fixed 7 11  "St. Benedict (Monasticism)")
    (holiday-fixed 7 17  "St. Nicholas Passion-Bearer")
    (holiday-fixed 7 25  "St. Christopher (Travellers)")
    (holiday-fixed 8 10  "St. Lawrence (Archives)")
    (holiday-fixed 8 28  "St. Augustine (Theologians)")
    (holiday-fixed 9 18  "St. Joseph of Cupertino (Astronauts)")
    (holiday-fixed 9 29  "St. Michael (Police)")
    (holiday-fixed 9 30  "St. Jerome (Libraries)")
    (holiday-fixed 10 16 "St. Marguerite d'Youville (Poverty)")
    (holiday-fixed 10 22 "St. John Paul II")
    (holiday-fixed 10 25 "St. Minias")
    ;(holiday-fixed 11 30 "St. Andrew (Scotland)") ; see Scottish Observances
    (holiday-fixed 12 6  "St. Nicholas")
    (holiday-fixed 12 13 "St. Lucy (Light)")
    (holiday-fixed 12 26 "St. Stephen")
    (holiday-fixed 12 31 "St. Sylvester"))
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
		holiday-solar-holidays
                holiday-seasonal-observances
		holiday-zodiacal-markers
		holiday-saints-days))

(defun cpj/holiday-available-holiday-lists (holiday-lists)
  "Add custom holiday categories to HOLIDAY-LISTS."
  (setq holiday-lists
        (assoc-delete-all "Bahá’í" holiday-lists))
  (append
   holiday-lists
   (delq nil
         (list
          ;; Work around `list-holidays' capitalizing "Bahá’í" to "Bahá’Í".
          (and holiday-bahai-holidays
               (cons "Bahai" holiday-bahai-holidays))

          (and holiday-scottish-observances
               (cons "Scottish" holiday-scottish-observances))

	  (and british-quarter-cross-days
	       (cons "Quarter" british-quarter-cross-days))

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

	  (and holiday-zodiacal-markers
               (cons "Zodiacal" holiday-zodiacal-markers))

          (and holiday-saints-days
               (cons "Saints" holiday-saints-days))))))

;; Extend the available holiday categories.
(remove-function (symbol-function 'holiday-available-holiday-lists)
                 #'cpj/holiday-available-holiday-lists)

(add-function :filter-return
              (symbol-function 'holiday-available-holiday-lists)
              #'cpj/holiday-available-holiday-lists)

(provide 'local-holidays)
;;; local-holidays.el ends here

; LocalWords:  bahai Deepavali Kartik Amavasya panchang दीपावली Iyyar
; LocalWords:  Jayanti Tamuz Tishrei Cheshvan Sh'vat amavasya Kartika
; LocalWords:  HaShanah Shemini Atzeret Simchat Hanyi Bahá’Í 寒衣
