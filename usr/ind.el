;;; ind.el --- A compact concordance of human chronologies -*- lexical-binding: t; -*-

;;; Commentary:

;; `ind' displays the current date through a compact concordance of
;; historical, religious, astronomical, civic, and idiosyncratic
;; chronologies.
;;
;; Its daily section includes Gregorian and Julian dates, the Roman
;; calendar, regnal and pontifical years, continuous day counts, and
;; dates in the Hebrew, French Republican, Hanke-Henry Permanent, and
;; Discordian calendars.
;;
;; A separate era-year concordance places several systems of historical
;; reckoning alongside one another, including the Hijri, Bahá’í,
;; Byzantine, Anno Lucis, Anno Inventionis, Anno Foederis, and Buddhist
;; eras.  The extended display includes additional Roman, Egyptian,
;; Japanese, Chinese, Tibetan, Indian, Sikh, and astronomical
;; reckonings.
;;
;; `ind-diagnostics' displays annual calendrical diagnostics inherited
;; from the verbose output of the author's original Bash implementation.
;; These include computistical values, selected religious observances,
;; calendar new years, and sexagenary year names.
;;
;; The package is an Emacs Lisp successor to the author's Bash program
;; of the same name.

;;; Code:

(require 'calendar)
(require 'cal-iso)
(require 'cal-julian)
(require 'cal-french)
(require 'cal-hebrew)
(require 'cal-islam)
(require 'cal-bahai)
(require 'cal-china)
(require 'holidays)

(require 'cl-lib)
(require 'subr-x)

(require 'discordian-calendar)
(require 'hanke-henry-calendar)
(require 'hindu-calendar)
(require 'indiction-years)
(require 'julian-day-counts)
(require 'moon-holidays)
(require 'regnal-years)
(require 'roman-clock)
(require 'tibdate)

(defconst ind--line-width 31
  "Width of the aligned `ind' display lines.")

(defconst ind--epacts
  [0 29 10 21 2 13 24 5 16 27 8 19 30 11 22 3 14 25 6 17]
  "Traditional epacts indexed by golden number.")

(defconst ind--dominical-letters
  ["G" "A" "B" "C" "D" "E" "F"]
  "Dominical letters indexed by the traditional calculation.")

(defconst ind--japanese-eras
  '(((5 1 2019)  "R" "Reiwa ㋿")
    ((1 8 1989)  "H" "Heisei ㍻")
    ((12 25 1926) "S" "Shōwa ㍼")
    ((7 30 1912) "T" "Taishō ㍽")
    ((1 25 1868) "M" "Meiji ㍾"))
  "Modern Japanese eras, newest first.

Each entry has the form:

  (START-DATE ABBREVIATION DISPLAY-NAME)

START-DATE is an Emacs calendar date in (MONTH DAY YEAR) form.")

(defconst ind--chinese-elements
  [nil "Wood" "Wood" "Fire" "Fire" "Earth" "Earth"
       "Metal" "Metal" "Water" "Water"]
  "Elements corresponding to the ten Chinese heavenly stems.")

(defconst ind--chinese-animals
  [nil "Rat" "Ox" "Tiger" "Rabbit" "Dragon" "Snake"
       "Horse" "Goat" "Monkey" "Rooster" "Dog" "Pig"]
  "Animals corresponding to the twelve Chinese earthly branches.")

(defconst ind--tibetan-elements
  [nil "Wood" "Wood" "Fire" "Fire" "Earth" "Earth"
       "Iron" "Iron" "Water" "Water"]
  "Elements corresponding to the ten Tibetan heavenly stems.")

(defconst ind--tibetan-animals
  [nil "Mouse" "Ox" "Tiger" "Rabbit" "Dragon" "Snake"
       "Horse" "Sheep" "Monkey" "Bird" "Dog" "Pig"]
  "Animals corresponding to the twelve Tibetan earthly branches.")

(defun ind--utc-date ()
  "Return the current UTC date in Emacs calendar form."
  (pcase-let ((`(,_second ,_minute ,_hour ,day ,month ,year . ,_)
                (decode-time nil t)))
    (list month day year)))

(defun ind--ordinal (number)
  "Return NUMBER as an ordinal string with a raised suffix."
  (let* ((n100 (% number 100))
         (suffix
          (cond
           ((memq n100 '(11 12 13)) "th")
           ((= (% number 10) 1) "st")
           ((= (% number 10) 2) "nd")
           ((= (% number 10) 3) "rd")
           (t "th"))))
    (concat
     (number-to-string number)
     (propertize suffix
                 'display '(raise 0.3)
                 'face '(:height 0.8)))))

(defun ind--month-day-string (date)
  "Return Gregorian DATE formatted as MM/DD."
  (pcase-let ((`(,month ,day ,_year) date))
    (format "%02d/%02d" month day)))

(defun ind--date-plus-days (date days)
  "Return Gregorian DATE shifted by DAYS."
  (calendar-gregorian-from-absolute
   (+ (calendar-absolute-from-gregorian date)
      days)))

(defun ind--gregorian-heading (date)
  "Return the main Gregorian heading for DATE."
  (pcase-let ((`(,month ,day ,year) date))
    (format "%s %d %s %d"
            (calendar-day-name date)
            day
            (calendar-month-name month)
            year)))

(defun ind--ce-line (date)
  "Return the Common Era and CE indiction line for DATE."
  (let ((year (calendar-extract-year date)))
    (format "CE %d %s %s ind."
            year
            (roman-clock--int-to-roman year)
            (ind--ordinal (indiction-ce year)))))

(defun ind--day-line (date)
  "Return day-of-year, ISO week, quarter, and time zone for DATE."
  (let* ((month (calendar-extract-month date))
         (absolute (calendar-absolute-from-gregorian date))
         (iso-date (calendar-iso-from-absolute absolute))
         (week (car iso-date))
         (quarter (1+ (/ (1- month) 3)))
         (zone (format-time-string "%Z")))
    (format "Day %d Week %d Q%d %s"
            (calendar-day-number date)
            week
            quarter
            zone)))

(defun ind--julian-day-number (julian-date)
  "Return the Julian calendar day-of-year for JULIAN-DATE."
  (pcase-let ((`(,_month ,_day ,year) julian-date))
    (1+ (- (calendar-julian-to-absolute julian-date)
           (calendar-julian-to-absolute
            (list 1 1 year))))))

(defun ind--old-style-line (date)
  "Return the Julian Old Style line for Gregorian DATE."
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (julian (calendar-julian-from-absolute absolute))
         (month (calendar-extract-month julian))
         (day (calendar-extract-day julian)))
    (format "O.S. %02d/%02d Day %d"
            month
            day
            (ind--julian-day-number julian))))

(defun ind--roman-calendar-line (&optional extended)
  "Return the Roman ante-diem date string.

When EXTENDED is non-nil, use the unabbreviated form."
  (roman-clock-ante-diem-string
   (unless extended '(4))))

(defun ind--regnal-line (date)
  "Return the royal and papal regnal line for DATE."
  (regnal-years-date-string date))

(defun ind--julian-count-line ()
  "Return today's UTC MJD and TJD line."
  (format "MJD %d · TJD %d"
          (modified-julian-date-today)
          (truncated-julian-date-today)))

(defun ind--two-column-line (left right)
  "Return LEFT and right-aligned RIGHT within `ind--line-width'."
  (let ((padding
         (max 1
              (- ind--line-width
                 (string-width left)
                 (string-width right)))))
    (concat left
            (make-string padding ?\s)
            right)))

(defun ind--dated-description-line (label date description)
  "Return LABEL followed by aligned DATE and DESCRIPTION.

DATE is a five-character MM/DD string aligned with the date
values produced by `ind--two-column-line'."
  (concat
   label
   (make-string
    (max 1
         (- ind--line-width
            (string-width label)
            (string-width date)))
    ?\s)
   date
   " "
   description))

(defun ind--french-line (date)
  "Return the French Republican calendar line for DATE."
  (pcase-let* ((absolute (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-french-from-absolute absolute))
               (month-name
                (aref calendar-french-month-name-array
                      (1- month))))
    (ind--two-column-line
     (format "%d %s" day month-name)
     (roman-clock--int-to-roman year))))

(defun ind--hanke-henry-line (utc-date)
  "Return the Hanke-Henry line for UTC-DATE."
  (pcase-let ((`(,month ,day ,year)
               (hanke-henry-calendar-from-gregorian utc-date)))
    (ind--two-column-line
     (format "%d %s"
             day
             (hanke-henry-calendar-month-name month))
     (format "H-H %d" year))))

(defun ind--discordian-line (date)
  "Return the Discordian calendar line for DATE."
  (pcase (discordian-calendar-from-gregorian date)
    (`(st-tibs-day ,year)
     (ind--two-column-line
      "St. Tib's Day"
      (format "YOLD %d" year)))
    (`(,season ,day ,year)
     (ind--two-column-line
      (format "%d %s"
              day
              (discordian-calendar-season-name season))
      (format "YOLD %d" year)))))

(defun ind--hebrew-line (date)
  "Return the Hebrew calendar line for DATE."
  (pcase-let* ((absolute (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-hebrew-from-absolute absolute))
               (months
                (if (calendar-hebrew-leap-year-p year)
                    calendar-hebrew-month-name-array-leap-year
                  calendar-hebrew-month-name-array-common-year))
               (month-name (aref months (1- month))))
    (ind--two-column-line
     (format "%d %s" day month-name)
     (format "AM %d" year))))

;;; Era-year concordance

(defun ind--era-line (label era year)
  "Return an aligned era-year line for LABEL, ERA, and YEAR."
  (ind--two-column-line
   label
   (format "%s %4d" era year)))

(defun ind--fractional-space (width)
  "Return a display space WIDTH columns wide."
  (propertize "\u200b"
              'display `(space :width ,width)))

(defun ind--hijri-line (date)
  "Return the Islamic civil year line for Gregorian DATE."
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (islamic (calendar-islamic-from-absolute absolute)))
    (ind--era-line
     "Hijri"
     "AH"
     (calendar-extract-year islamic))))

(defun ind--bahai-line (date)
  "Return the Bahá’í Era year line for Gregorian DATE."
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (bahai (calendar-bahai-from-absolute absolute)))
    (ind--era-line
     "Bahá’í"
     "BE"
     (calendar-extract-year bahai))))

(defun ind--byzantine-year (date)
  "Return the Byzantine Anno Mundi year for Gregorian DATE.

The Byzantine year begins on September 1 in the Julian
calendar."
  (let* ((absolute (calendar-absolute-from-gregorian date))
         (julian (calendar-julian-from-absolute absolute))
         (month (calendar-extract-month julian))
         (year (calendar-extract-year julian)))
    (+ year
       5508
       (if (>= month 9) 1 0))))

(defun ind--byzantine-indiction (year)
  "Return the Byzantine indiction number for Anno Mundi YEAR."
  (let ((indiction (% year 15)))
    (if (zerop indiction)
        15
      indiction)))

(defun ind--byzantine-line (date)
  "Return the Byzantine Anno Mundi and indiction line for DATE."
  (let* ((year (ind--byzantine-year date))
         (indiction (ind--byzantine-indiction year)))
    (ind--era-line
     (concat
      (format "Byzantine %s" (ind--ordinal indiction))
      (ind--fractional-space 0.45))
     "AM"
     year)))

(defun ind--anno-lucis-line (date)
  "Return the Anno Lucis year line for DATE."
  (ind--era-line
   "Anno Lucis"
   "AL"
   (+ (calendar-extract-year date) 4000)))

(defun ind--anno-inventionis-line (date)
  "Return the Anno Inventionis year line for DATE."
  (ind--era-line
   "Anno Inventionis"
   "AI"
   (+ (calendar-extract-year date) 530)))

(defun ind--anno-foederis-line (date)
  "Return the Anno Foederis year line for DATE."
  (ind--era-line
   "Anno Foederis"
   "AF"
   (+ (calendar-extract-year date) 1250)))

(defun ind--buddhist-line (date)
  "Return the Thai solar Buddhist Era year line for DATE."
  (ind--era-line
   "Buddhist"
   "BE"
   (+ (calendar-extract-year date) 543)))

;;; Extended era-year concordance

(defun ind--date-on-or-after-p (date reference)
  "Return non-nil when DATE is on or after REFERENCE."
  (>= (calendar-absolute-from-gregorian date)
      (calendar-absolute-from-gregorian reference)))

(defun ind--auc-year (date)
  "Return the Ab Urbe Condita year for Gregorian DATE."
  (+ (calendar-extract-year date) 753))

(defun ind--diocletian-year (date)
  "Return the Diocletian Era year for Gregorian DATE.

The year begins on 29 August in the Julian calendar."
  (pcase-let* ((absolute
                (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-julian-from-absolute absolute)))
    (- year
       (if (or (> month 8)
               (and (= month 8)
                    (>= day 29)))
           283
         284))))

(defun ind--julian-period-year (date)
  "Return the Julian Period year for Gregorian DATE.

The calculation follows the corresponding Julian calendar year."
  (pcase-let* ((absolute
                (calendar-absolute-from-gregorian date))
               (`(,_month ,_day ,year)
                (calendar-julian-from-absolute absolute)))
    (+ year 4713)))

(defun ind--years-after-present (date)
  "Return years after the conventional 1950 present for DATE."
  (- (calendar-extract-year date) 1950))

(defun ind--japanese-era-entry (date)
  "Return the modern Japanese era entry containing DATE.

Return nil when DATE precedes the modern era table."
  (cl-find-if
   (lambda (entry)
     (ind--date-on-or-after-p date (car entry)))
   ind--japanese-eras))

(defun ind--japanese-era (date)
  "Return Japanese era abbreviation and year for DATE.

The return value is (ABBREVIATION YEAR), or nil before the modern
era table."
  (when-let* ((entry (ind--japanese-era-entry date))
              (start-date (car entry)))
    (list
     (nth 1 entry)
     (1+ (- (calendar-extract-year date)
            (calendar-extract-year start-date))))))

(defun ind--japanese-era-name (date)
  "Return the full Japanese era display name for DATE."
  (when-let* ((entry (ind--japanese-era-entry date)))
    (nth 2 entry)))

(defun ind--japanese-imperial-year (date)
  "Return the Japanese Imperial year for Gregorian DATE."
  (+ (calendar-extract-year date) 660))

(defun ind--roc-year (date)
  "Return the Republic of China year for Gregorian DATE."
  (- (calendar-extract-year date) 1911))

(defun ind--chinese-imperial-year (date)
  "Return the Yellow Emperor year for Gregorian DATE.

The year changes at Chinese New Year."
  (pcase-let* ((absolute
                (calendar-absolute-from-gregorian date))
               (`(,cycle ,year ,_month ,_day)
                (calendar-chinese-from-absolute absolute)))
    (+ (* 60 (1- cycle))
       year
       61)))

(defun ind--nanakshahi-year (date)
  "Return the Nanakshahi year for Gregorian DATE.

The Nanakshahi calendar, or Sikh calendar, is a tropical solar
calendar used in Sikhism.  Its year begins on March 14 in the
Gregorian calendar."
  (let ((year (calendar-extract-year date))
        (month (calendar-extract-month date))
        (day (calendar-extract-day date)))
    (- year
       (if (or (< month 3)
               (and (= month 3)
                    (< day 14)))
           1469
         1468))))

(defun ind--auc-line (date)
  "Return the City of Rome era line for DATE."
  (ind--era-line
   "City of Rome"
   "AUC"
   (ind--auc-year date)))

(defun ind--diocletian-line (date)
  "Return the Diocletian Era line for DATE."
  (ind--era-line
   "Diocletian"
   "RD"
   (ind--diocletian-year date)))

(defun ind--julian-period-line (date)
  "Return the Julian Period line for DATE."
  (ind--era-line
   "Julian Period"
   "JP"
   (ind--julian-period-year date)))

(defun ind--after-present-line (date)
  "Return the Years After Present line for DATE."
  (ind--era-line
   "Years After Present"
   "AP"
   (ind--years-after-present date)))

(defun ind--japanese-line (date)
  "Return the Japanese era and Imperial year line for DATE."
  (when-let* ((era (ind--japanese-era date)))
    (ind--era-line
     "Japanese Imperial"
     (format "%s%d" (car era) (cadr era))
     (ind--japanese-imperial-year date))))

(defun ind--chinese-line (date)
  "Return the Republic of China and Yellow Emperor year line for DATE."
  (ind--era-line
   "Chinese"
   (format "ROC %d YE" (ind--roc-year date))
   (ind--chinese-imperial-year date)))

(defun ind--tibetan-line (date)
  "Return the Tibetan Rabjung cycle and Tibetan Era line for DATE."
  (pcase-let
      ((`(,cycle ,year ,_month ,_leap-month ,_day ,_leap-day)
        (tibdate-from-gregorian date)))
    (let* ((rabjung-year
            (+ (* (1- cycle) 60)
               year))
           (tibetan-era
            (+ rabjung-year 1153)))
      (ind--era-line
       "Tibetan"
       (format "%d/%d TE" cycle year)
       tibetan-era))))

(defun ind--hindu-line (date)
  "Return the Indian National Calendar era line for DATE."
  (pcase-let* ((`(,month ,day ,year) date)
               (`(,saka-year ,_saka-month ,_saka-day)
                (hindu-calendar--indian-national-from-gregorian
                 year month day)))
    (ind--era-line
     "Indian National"
     "Śaka"
     saka-year)))

(defun ind--nanakshahi-line (date)
  "Return the Nanakshahi year line for DATE."
  (ind--era-line
   "Nanakshahi (Sikh)"
   "NS"
   (ind--nanakshahi-year date)))

;;; Diagnostics

(defun ind--golden-number (year)
  "Return the golden number for Gregorian YEAR."
  (1+ (% year 19)))

(defun ind--epact (year)
  "Return the traditional epact for Gregorian YEAR."
  (aref ind--epacts
        (ind--golden-number year)))

(defun ind--dominical-letter (year)
  "Return the dominical letter or letters for Gregorian YEAR."
  (let* ((year-1 (1- year))
         (century-year (% year-1 100))
         (index
          (% (+ (* 2 (% century-year 4))
                (* 4 (% century-year 7))
                (* 2 (% (/ year-1 100) 4)))
             7))
         (first
          (aref ind--dominical-letters index)))
    (if (calendar-leap-year-p year)
        (concat
         first
         (aref ind--dominical-letters
               (if (zerop index)
                   6
                 (1- index))))
      first)))

(defun ind--julian-delta (date)
  "Return the Gregorian-to-Julian nominal date difference for DATE."
  (- (calendar-julian-to-absolute date)
     (calendar-absolute-from-gregorian date)))

(defun ind--hebrew-observances (year)
  "Return selected Hebrew observances occurring in Gregorian YEAR.

The return value is a list:

  (PASSOVER ROSH-HASHANA YOM-KIPPUR)

Each value is a Gregorian date."
  (let* ((midyear-absolute
          (calendar-absolute-from-gregorian
           (list 7 1 year)))
         (hebrew-year
          (calendar-extract-year
           (calendar-hebrew-from-absolute
            midyear-absolute))))
    (list
     (calendar-gregorian-from-absolute
      (calendar-hebrew-to-absolute
       (list 1 15 hebrew-year)))
     (calendar-gregorian-from-absolute
      (calendar-hebrew-to-absolute
       (list 7 1 (1+ hebrew-year))))
     (calendar-gregorian-from-absolute
      (calendar-hebrew-to-absolute
       (list 7 10 (1+ hebrew-year)))))))

(defun ind--easter-date (year)
  "Return Gregorian Easter Sunday in YEAR."
  (calendar-gregorian-from-absolute
   (holiday-easter-etc-abs year)))

(defun ind--lent-date (year)
  "Return Ash Wednesday in YEAR."
  (calendar-gregorian-from-absolute
   (- (holiday-easter-etc-abs year) 46)))

(defun ind--advent-date (year)
  "Return the first Sunday of Advent in YEAR."
  (calendar-gregorian-from-absolute
   (calendar-dayname-on-or-before
    0
    (calendar-absolute-from-gregorian
     (list 12 3 year)))))

(defun ind--islamic-new-year-date (year)
  "Return the first Islamic New Year occurring in Gregorian YEAR."
  (let* ((start
          (calendar-absolute-from-gregorian
           (list 1 1 year)))
         (end
          (calendar-absolute-from-gregorian
           (list 12 31 year)))
         (first-islamic-year
          (calendar-extract-year
           (calendar-islamic-from-absolute start)))
         (last-islamic-year
          (calendar-extract-year
           (calendar-islamic-from-absolute end)))
         result)
    (cl-loop
     for islamic-year
     from first-islamic-year
     to (1+ last-islamic-year)
     for absolute =
     (calendar-islamic-to-absolute
      (list 1 1 islamic-year))
     when (and (<= start absolute)
               (<= absolute end))
     do (setq result
              (calendar-gregorian-from-absolute absolute))
     and return result)
    result))

(defun ind--chinese-new-year-date (year)
  "Return Chinese New Year occurring in Gregorian YEAR."
  (calendar-gregorian-from-absolute
   (cadr
    (assoc 1
           (calendar-chinese-year year)))))

(defun ind--chinese-year-description (year)
  "Return the Chinese sexagenary description for Gregorian YEAR.

The description belongs to the Chinese year whose New Year occurs
during Gregorian YEAR."
  (let* ((stem
          (1+ (% (- year 4) 10)))
         (branch
          (1+ (% (- year 4) 12)))
         (polarity
          (if (cl-oddp stem)
              "Yang"
            "Yin")))
    (format "%s %s %s"
            polarity
            (aref ind--chinese-elements stem)
            (aref ind--chinese-animals branch))))

(defun ind--tibetan-losar-data (year)
  "Return Tibetan Losar data for Gregorian YEAR.

The return value is:

  (DATE DESCRIPTION)

DATE is the Gregorian date of Losar.  DESCRIPTION is the gender,
element, and animal of the Tibetan year whose Losar falls within
Gregorian YEAR.

Return nil when `tibdate-program' cannot be found."
  (when (executable-find tibdate-program)
    (pcase-let*
        ((`(,cycle ,tibetan-year
                   ,_month ,_leap-month ,_day ,_leap-day)
          (tibdate-from-gregorian
           (list 7 1 year)))
         (rabjung-year
          (+ (* (1- cycle) 60)
             tibetan-year))
         (stem
          (1+ (% (+ rabjung-year 2) 10)))
         (branch
          (1+ (% (+ rabjung-year 2) 12)))
         (gender
          (if (cl-oddp stem)
              "Male"
            "Female"))
         (description
          (format "%s %s %s"
                  gender
                  (aref ind--tibetan-elements stem)
                  (aref ind--tibetan-animals branch))))
      (list
       (tibdate-losar cycle tibetan-year)
       description))))

(defun ind--vassa-date (year)
  "Return the Gregorian date on which Vassa begins in YEAR.

Vassa begins on the day after the first full moon in July."
  (when-let* ((full-moon
               (moon-holidays-first-full-moon 7 year)))
    (ind--date-plus-days full-moon 1)))

(defun ind--pavarana-date (year)
  "Return the Gregorian date of Pavarana in YEAR.

Pavarana falls on the first full moon in October."
  (moon-holidays-first-full-moon 10 year))

(defun ind-diagnostics-string (&optional date)
  "Return calendrical diagnostics for the year containing DATE.

DATE defaults to the current Gregorian date."
  (let* ((date
          (or date
              (calendar-current-date)))
         (year
          (calendar-extract-year date))
         (hebrew
          (ind--hebrew-observances year))
         (passover
          (nth 0 hebrew))
         (rosh-hashana
          (nth 1 hebrew))
         (yom-kippur
          (nth 2 hebrew))
         (hijra
          (ind--islamic-new-year-date year))
         (japanese-era
          (ind--japanese-era-name date))
         (chinese-new-year
          (ind--chinese-new-year-date year))
         (tibetan-losar
          (ind--tibetan-losar-data year))
         (vassa
          (ind--vassa-date year))
         (pavarana
          (ind--pavarana-date year)))
    (string-join
     (delq
      nil
      (list
       (ind--two-column-line
        "Dominical letter"
        (ind--dominical-letter year))
       (ind--two-column-line
        "Epact"
        (number-to-string
         (ind--epact year)))
       (ind--two-column-line
        "Golden number"
        (number-to-string
         (ind--golden-number year)))
       (ind--two-column-line
        "Julian delta"
        (number-to-string
         (ind--julian-delta date)))
       (ind--two-column-line
        "Passover"
        (ind--month-day-string passover))
       (ind--two-column-line
        "Rosh Hashana"
        (ind--month-day-string rosh-hashana))
       (ind--two-column-line
        "Yom Kippur"
        (ind--month-day-string yom-kippur))
       (ind--two-column-line
        "Lent"
        (ind--month-day-string
         (ind--lent-date year)))
       (ind--two-column-line
        "Easter"
        (ind--month-day-string
         (ind--easter-date year)))
       (ind--two-column-line
        "Advent"
        (ind--month-day-string
         (ind--advent-date year)))
       (when hijra
         (ind--two-column-line
          "Hijra"
          (ind--month-day-string hijra)))
       (when japanese-era
         (ind--two-column-line
          "Japanese Era"
          japanese-era))
       (ind--dated-description-line
	"Chinese New Year"
	(ind--month-day-string chinese-new-year)
	(ind--chinese-year-description year))
       (ind--dated-description-line
	"Tibetan Losar"
	(ind--month-day-string (car tibetan-losar))
	(cadr tibetan-losar))
       (when vassa
         (ind--two-column-line
          "Vassa"
          (ind--month-day-string vassa)))
       (when pavarana
         (ind--two-column-line
          "Pavarana"
          (ind--month-day-string pavarana)))))
     "\n")))

;;; Summary construction and display

(defun ind-summary-string (&optional extended)
  "Return the compact daily `ind' summary as a string.

When EXTENDED is non-nil, include the extended era-year dates."
  (let ((date (calendar-current-date))
        (utc-date (ind--utc-date)))
    (string-join
     (delq
      nil
      (append
       (list
        (ind--gregorian-heading date)
        (ind--ce-line date)
        (ind--day-line date)
        (ind--old-style-line date)
        (ind--regnal-line date)
        (ind--julian-count-line)
        (ind--roman-calendar-line extended)
        (ind--french-line date)
        (ind--hanke-henry-line utc-date)
        (ind--discordian-line date)
        (ind--hebrew-line date)

        ;; Era-year concordance.
        (make-string ind--line-width ?-)
        (ind--hijri-line date)
        (ind--bahai-line date)
        (ind--byzantine-line date)
        (ind--anno-lucis-line date)
        (ind--anno-inventionis-line date)
        (ind--anno-foederis-line date)
        (ind--buddhist-line date))

       (when extended
         (list
          (ind--auc-line date)
          (ind--diocletian-line date)
          (ind--julian-period-line date)
          (ind--after-present-line date)
          (ind--japanese-line date)
          (ind--chinese-line date)
          (ind--tibetan-line date)
          (ind--hindu-line date)
          (ind--nanakshahi-line date)))))
     "\n")))

(defun ind--display (contents)
  "Display CONTENTS in the `*ind*' buffer."
  (let ((buffer (get-buffer-create "*ind*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert contents)
        (unless (bolp)
          (insert "\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun ind (&optional extended)
  "Display a multi-calendar summary for today.

Show civil, historical, religious, astronomical, regnal, and
idiosyncratic representations of the current date.

With prefix argument EXTENDED, also show the extended era-year
dates."
  (interactive "P")
  (ind--display
   (ind-summary-string extended)))

;;;###autoload
(defun ind-extended ()
  "Display the extended multi-calendar summary for today."
  (interactive)
  (ind t))

;;;###autoload
(defun ind-diagnostics ()
  "Display calendrical diagnostics for the current year."
  (interactive)
  (ind--display
   (ind-diagnostics-string)))

(provide 'ind)

;;; ind.el ends here
