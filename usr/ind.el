;;; ind.el --- A compact concordance of human chronologies -*- lexical-binding: t; -*-

;;; Commentary:

;; `ind' displays a Gregorian date through a compact concordance of
;; historical, religious, astronomical, civic, and idiosyncratic
;; chronologies.
;;
;; It displays the current date by default, but may also be called
;; programmatically for an arbitrary date.
;;
;; Its daily section includes Gregorian and Julian dates, ordinal
;; and ISO week reckonings, regnal and pontifical years, continuous
;; day counts, the lunar phase and age, and dates in the Roman,
;; Hebrew, French Republican, Hanke-Henry Permanent, and Discordian
;; calendars.
;;
;; A separate era-year concordance places several systems of
;; historical reckoning alongside one another, including the Hijri,
;; Bahá'í, Byzantine, Anno Lucis, Anno Inventionis, Anno Foederis,
;; and Buddhist eras.
;;
;; The extended display expands the Roman and French Republican
;; entries and adds Persian, Roman, Egyptian, Japanese, Chinese,
;; Tibetan, Mayan, Indian, Sikh, and astronomical reckonings.
;;
;; The extended display can optionally include the ages of selected
;; historic and present Lodges or similar organizations; this is
;; disabled by default and controlled by `ind-show-lodge-dates'.
;; The default entries may be extended or replaced through
;; `ind-lodge-dates' to reflect locally relevant institutions.
;;
;; `ind-diagnostics' displays annual calendrical diagnostics inherited
;; from the verbose output of the author's original Bash
;; implementation. These include computistical values, selected
;; religious observances, calendar new years, and sexagenary
;; year names.
;;
;; The package is an Emacs Lisp successor to the author's Bash program
;; of the same name.

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'lunar)
(require 'parse-time)

(require 'cal-iso)
(require 'cal-julian)
(require 'cal-french)
(require 'cal-hebrew)
(require 'cal-islam)
(require 'cal-bahai)
(require 'cal-china)
(require 'cal-persia)
(require 'cal-mayan)

(require 'discordian-calendar)
(require 'hanke-henry-calendar)
(require 'hindu-calendar)
(require 'indiction-years)
(require 'julian-day-counts)
(require 'moon-holidays)
(require 'regnal-years)
(require 'roman-clock)
(require 'tibdate)

(require 'cl-lib)
(require 'subr-x)

(defconst ind--line-width 38
  "Width of the aligned `ind' display lines.")

(defcustom ind-show-anno-depositionis nil
  "Whether to show Anno Depositionis in the Masoretic display."
  :type 'boolean
  :group 'ind)

(defcustom ind-show-anno-ordinis nil
  "Whether to show the Anno Ordinis Knights Templar year."
  :type 'boolean
  :group 'ind)

(defconst ind--nabonassar-epoch -272787
  "Absolute date of 1 Thoth 1 in the Era of Nabonassar.

This corresponds to 26 February 747 BCE in the Julian calendar.")

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

(defconst ind--mayan-tzolkin-names
  ["Imix" "Ik" "Akbal" "Kan" "Chicchan"
   "Cimi" "Manik" "Lamat" "Muluc" "Oc"
   "Chuen" "Eb" "Ben" "Ix" "Men"
   "Cib" "Caban" "Etznab" "Cauac" "Ahau"]
  "Mayan Tzolk’in day names.")

(defconst ind--mayan-haab-month-names
  ["Pop" "Uo" "Zip" "Zotz" "Tzec" "Xul"
   "Yaxkin" "Mol" "Chen" "Yax" "Zac" "Ceh"
   "Mac" "Kankin" "Muan" "Pax" "Kayab"
   "Cumku" "Uayeb"]
  "Mayan Haab month names.")

(defcustom ind-show-lodge-dates nil
  "Whether `ind-extended' shows Lodge age lines."
  :type 'boolean
  :group 'ind)

(defcustom ind-lodge-dates
  '(("United Grand Lodge"     . 1717)
    ("Provincial Grand Lodge" . 1792)
    ("Grand Lodge of Canada"  . 1855))
  "Alist of organizations and their founding years for `ind-extended'.

Each entry has the form (NAME . YEAR), where NAME is the label
shown in the display and YEAR is the Gregorian founding year.

The default entries are Masonic Grand Lodges, but the list may
be extended or replaced with locally relevant Lodges or other
organizations."
  :type '(alist :key-type string :value-type integer)
  :group 'ind)

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

(defun ind--section-rule ()
  "Return a horizontal rule of `ind--line-width' characters."
  (make-string ind--line-width ?─))

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
    (propertize
     (format "%s %d %s %d"
             (calendar-day-name date)
             day
             (calendar-month-name month)
             year)
     'face 'bold)))

(defun ind--ce-line (date)
  "Return the Common Era and CE indiction line for DATE."
  (let ((year (calendar-extract-year date)))
    (concat
     (format "CE %d %s %s ind."
             year
             (roman-clock--int-to-roman year)
             (ind--ordinal (indiction-ce year)))
     (ind--fractional-space 0.45))))

(defun ind--day-line (date)
  "Return day-of-year, days remaining, ISO week, quarter, and time zone for DATE."
  (let* ((month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day (calendar-day-number date))
         (days-in-year (if (calendar-leap-year-p year) 366 365))
         (remaining (- days-in-year day))
         (absolute (calendar-absolute-from-gregorian date))
         (iso-date (calendar-iso-from-absolute absolute))
         (week (car iso-date))
         (quarter (1+ (/ (1- month) 3)))
         (zone (format-time-string "%Z")))
    (format "Day %d/%d Week %d Q%d %s"
            day
            remaining
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

(defun ind--roman-calendar-line (date &optional extended)
  "Return the Roman ante-diem date string for DATE.

When EXTENDED is non-nil, use the unabbreviated form."
  (roman-clock-ante-diem-string
   (unless extended '(4))
   date))

(defun ind--regnal-line (date)
  "Return the royal and papal regnal line for DATE."
  (regnal-years-date-string date))

(defun ind--julian-count-line (date)
  "Return the MJD and TJD line for Gregorian DATE."
  (format "MJD %d · TJD %d"
          (modified-julian-date-from-gregorian date)
          (truncated-julian-date-from-gregorian date)))

(defun ind--lunar-phase-time (phase)
  "Return the encoded local time of lunar PHASE data."
  (let* ((date (car phase))
         (parsed (parse-time-string (cadr phase))))
    (encode-time
     (or (decoded-time-second parsed) 0)
     (or (decoded-time-minute parsed) 0)
     (or (decoded-time-hour parsed) 0)
     (calendar-extract-day date)
     (calendar-extract-month date)
     (calendar-extract-year date)
     (decoded-time-zone parsed))))

(defun ind--lunar-age (date)
  "Return the approximate lunar age in days for Gregorian DATE."
  (let* ((moment
          (encode-time
           0 0 12
           (calendar-extract-day date)
           (calendar-extract-month date)
           (calendar-extract-year date)))
         (index (lunar-index date))
         (new-moon-index (- index (mod index 4)))
         (new-moon (lunar-phase new-moon-index))
         (new-moon-time (ind--lunar-phase-time new-moon)))
    ;; If this new moon lies ahead of DATE, use the previous lunation.
    (when (time-less-p moment new-moon-time)
      (setq new-moon
            (lunar-phase (- new-moon-index 4))
            new-moon-time
            (ind--lunar-phase-time new-moon)))
    (/ (float-time (time-subtract moment new-moon-time))
       86400.0)))

(defun ind--lunar-phase-name (age)
  "Return an eight-part lunar phase name for lunar AGE in days."
  (cond
   ((or (< age 1.85) (>= age 27.68)) "New Moon")
   ((< age 5.54)  "Waxing Crescent")
   ((< age 9.23)  "First Quarter")
   ((< age 12.92) "Waxing Gibbous")
   ((< age 16.61) "Full Moon")
   ((< age 20.30) "Waning Gibbous")
   ((< age 23.99) "Last Quarter")
   (t             "Waning Crescent")))

(defun ind--lunar-line (date)
  "Return the lunar phase and approximate age for DATE."
  (let ((age (ind--lunar-age date)))
    (format "%s %dd"
            (ind--lunar-phase-name age)
            (floor age))))

(defun ind--french-day-name (date)
  "Return the French Revolutionary feast name for Gregorian DATE."
  (pcase-let* ((absolute (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-french-from-absolute absolute)))
    (when (> year 0)
      (aref calendar-french-feasts-array
            (+ -31 (* 30 month) day)))))

(defun ind--french-feast-name (date)
  "Return the compact French Revolutionary feast name for DATE."
  (when-let* ((name (ind--french-day-name date)))
    (setq name
          (replace-regexp-in-string
           (rx string-start
               (or "du "
                   "de la "
                   "de l'"
                   "de l’"
                   "des "
                   "de "))
           ""
           name))
    (replace-regexp-in-string
     (rx " de ")
     " "
     name)))

(defun ind--french-date-label (month day)
  "Return a compact French Republican date label."
  (if (= month 13)
      (format "%d jour compl." day)
    (format "%d %s"
            day
            (aref calendar-french-month-name-array
                  (1- month)))))

(defun ind--french-line (date &optional extended)
  "Return the French Republican calendar line for DATE.

When EXTENDED is non-nil, include the day's traditional name
in parentheses."
  (pcase-let* ((absolute (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-french-from-absolute absolute))
               (label (ind--french-date-label month day))
               (name (and extended
                          (ind--french-feast-name date)))
               (left (if name
                         (format "%s (%s)" label name)
                       label))
               (right (roman-clock--int-to-roman year)))
    (when (> year 0)
      (ind--two-column-line left right))))

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

(defun ind--anno-line (date name abbreviation offset)
  "Return an anno-era line for DATE.

NAME is the display label, ABBREVIATION is the era abbreviation,
and OFFSET is added to the Gregorian year."
  (ind--era-line
   name
   "" ; abbreviation
   (+ (calendar-extract-year date) offset)))

(defun ind--anno-lucis-line (date)
  "Return the Anno Lucis year line for DATE.

Traditionally reckons years from the creation of the world,
using a Masonic epoch 4000 years before the Common Era."
  (ind--anno-line date "Anno Lucis" "AL" 4000))

(defun ind--anno-inventionis-line (date)
  "Return the Anno Inventionis year line for DATE.

Traditionally marks the completion of the Second Temple by
Zerubbabel (destroyed 70 CE)."
  (ind--anno-line date "Anno Inventionis" "AI" 530))

(defun ind--anno-depositionis-line (date)
  "Return the Anno Depositionis line for DATE.

Traditionally marks the completion of the First Temple by
Solomon (destroyed 587/586 BCE)."
  (when ind-show-anno-depositionis
    (ind--anno-line date "Anno Depositionis" "AD" 1000)))

(defun ind--anno-ordinis-line (date)
  "Return the Anno Ordinis Knights Templar line for DATE.

Traditionally marks the foundation of the Knights Templar
in 1118 CE."
  (when ind-show-anno-ordinis
    (ind--anno-line date "Anno Ordinis KT" "AO" -1117)))

(defun ind--anno-foederis-line (date)
  "Return the Anno Foederis year line for DATE.

Traditionally marks the covenant made with Abraham."
  (ind--anno-line date "Anno Foederis" "AF" 1250))

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

(defun ind--nabonassar-year (date)
  "Return the Nabonassarian era year for DATE.

The Era of Nabonassar uses Egyptian years of exactly 365 days,
without intercalation, from the epoch 26 February 747 BCE
(Julian)."
  (1+ (/ (- (calendar-absolute-from-gregorian date)
            ind--nabonassar-epoch)
         365)))

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

(defun ind--years-from-present (date)
  "Return the signed year offset from the conventional 1950 present."
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

(defun ind--nabonassar-line (date)
  "Return the Era of Nabonassar line for DATE."
  (ind--era-line
   "Nabonassar"
   "AN"
   (ind--nabonassar-year date)))

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

(defun ind--present-era-line (date)
  "Return years before or after the conventional 1950 present for DATE."
  (let ((offset (ind--years-from-present date)))
    (cond
     ((< offset 0)
      (ind--era-line "Years Before Present" "BP"
       (- offset)))
     ((> offset 0)
      (ind--era-line "Years After Present" "AP"
       offset))
     (t
      (ind--era-line "Present" "BP"
       0)))))

(defun ind--japanese-line (date)
  "Return the Japanese era and Kōki year line for DATE."
  (when-let* ((era (ind--japanese-era date))
              (name (ind--japanese-era-name date)))
    (ind--two-column-line
     (format "Japanese %s %d"
             name
             (cadr era))
     (format "Kōki %d"
             (ind--japanese-imperial-year date)))))

(defun ind--chinese-line (date)
  "Return the Republic of China and Yellow Emperor year line for DATE."
  (ind--two-column-line
   (format "Chinese  ROC %d"
           (ind--roc-year date))
   (format "YE %d"
           (ind--chinese-imperial-year date))))

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
      (ind--two-column-line
       (format "Tibetan  %d/%d" cycle year)
       (format "TE %d" tibetan-era)))))

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

;;; Persian dates
(defun ind--persian-line (date)
  "Return the Persian calendar line for Gregorian DATE."
  (pcase-let* ((absolute
                (calendar-absolute-from-gregorian date))
               (`(,month ,day ,year)
                (calendar-persian-from-absolute absolute)))
    (ind--era-line
     (format "%d %s"
             day
             (aref calendar-persian-month-name-array
                   (1- month)))
     "SH"
     year)))

;;; Mayan dates
(defun ind--mayan-line (date)
  "Return the Mayan Tzolk’in and Haab line for DATE."
  (pcase-let* ((absolute (calendar-absolute-from-gregorian date))
               (`(,tz-number . ,tz-name)
                (calendar-mayan-tzolkin-from-absolute absolute))
               (`(,haab-day . ,haab-month)
                (calendar-mayan-haab-from-absolute absolute)))
    (ind--two-column-line
     (format "Mayan    %d %s"
             tz-number
             (aref ind--mayan-tzolkin-names (1- tz-name)))
     (format "%d %s"
             haab-day
             (aref ind--mayan-haab-month-names
                   (1- haab-month))))))

;; Lodge dates
(defun ind--lodge-lines (date)
  "Return Lodge age lines for DATE."
  (let ((year (calendar-extract-year date)))
    (delq
     nil
     (mapcar
      (lambda (entry)
        (pcase-let ((`(,name . ,founded) entry))
          (when (> year founded)
            (ind--two-column-line
             name
             (format "%4d" (- year founded))))))
      ind-lodge-dates))))

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
       "Diagnostics:\n"
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
       (ind--two-column-line
	(format "Chinese (%s)"
		(ind--chinese-year-description year))
	(ind--month-day-string chinese-new-year))
       (when tibetan-losar
	 (ind--two-column-line
	  (format "Tibetan (%s)"
		  (cadr tibetan-losar))
	  (ind--month-day-string (car tibetan-losar))))
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

(defun ind--columnize (left right &optional gap)
  "Return LEFT and RIGHT line lists rendered as two columns.

GAP is the number of spaces between columns and defaults to 3."
  (let* ((gap (or gap 3))
         (width (apply #'max 0 (mapcar #'string-width left)))
         (count (max (length left) (length right))))
    (string-join
     (cl-loop for i below count
              for lhs = (or (nth i left) "")
              for rhs = (or (nth i right) "")
              collect
              (if (string-empty-p rhs)
                  lhs
                (concat lhs
                        (make-string
                         (+ gap (- width (string-width lhs)))
                         ?\s)
                        rhs)))
     "\n")))

(defun ind--primary-lines (date extended)
  "Return primary calendar lines for DATE.

When EXTENDED is non-nil, include extended primary-calendar
information."
  (delq
   nil
   (list
    (ind--gregorian-heading date)
    (ind--ce-line date)
    (ind--day-line date)
    (ind--old-style-line date)
    (ind--regnal-line date)
    (ind--julian-count-line date)
    (ind--lunar-line date)
    (ind--roman-calendar-line date extended)
    (ind--french-line date extended)
    (ind--hanke-henry-line date)
    (ind--discordian-line date)
    (ind--hebrew-line date)
    (when extended
      (ind--persian-line date)))))

(defun ind--concordance-lines (date extended)
  "Return era-year concordance lines for DATE.

When EXTENDED is non-nil, include the extended concordance and
optional Lodge dates."
  (delq
   nil
   (append
    (list
     (ind--hijri-line date)
     (ind--bahai-line date)
     (ind--byzantine-line date)
     (ind--buddhist-line date))

    (when extended
      (list
       (ind--auc-line date)
       (ind--nabonassar-line date)
       (ind--diocletian-line date)
       (ind--julian-period-line date)
       (ind--present-era-line date)
       (ind--hindu-line date)
       (ind--nanakshahi-line date)
       (ind--japanese-line date)
       (ind--chinese-line date)
       (ind--tibetan-line date)
       (ind--mayan-line date)
       (ind--section-rule)))

    (list
     (ind--anno-lucis-line date)
     (ind--anno-inventionis-line date)
     (ind--anno-depositionis-line date)
     (ind--anno-ordinis-line date)
     (ind--anno-foederis-line date))

    (when extended
       (ind--lodge-lines date)))))

(defun ind-summary-string (&optional extended date)
  "Return the daily `ind' summary as a string.

When EXTENDED is non-nil, include the extended era-year dates.
DATE is an Emacs calendar date, defaulting to today."
  (let* ((date (or date (calendar-current-date)))
         (primary (ind--primary-lines date extended))
         (concordance (ind--concordance-lines date extended)))
    (if extended
	(ind--columnize
	 (append (list (car primary) "")
		 (cdr primary))
	 (append '("" "") concordance))
      (string-join
       (append primary
               (list (ind--section-rule))
               concordance)
       "\n"))))

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
(defun ind (&optional extended date)
  "Display a multi-calendar summary.

Show civil, historical, religious, astronomical, regnal, and
idiosyncratic representations of DATE, defaulting to today.

With prefix argument EXTENDED, also show the extended era-year
dates."
  (interactive "P")
  (ind--display
   (ind-summary-string extended date)))

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

(defun ind--french-width-overflows (year)
  "Return French Republican display overflows occurring in YEAR.

Each result has the form:

  (DATE LABEL WIDTH AVAILABLE)

where DATE is the Gregorian date, LABEL is the extended French
Republican label, WIDTH is its display width, and AVAILABLE is
the width available before the Roman year."
  (let (overflows)
    (cl-loop
     for absolute
     from (calendar-absolute-from-gregorian (list 1 1 year))
     to (calendar-absolute-from-gregorian (list 12 31 year))
     for date = (calendar-gregorian-from-absolute absolute)
     do
     (pcase-let* ((`(,month ,day ,french-year)
                   (calendar-french-from-absolute absolute))
                  (label (ind--french-date-label month day))
                  (name (ind--french-feast-name date))
                  (left (if name
                            (format "%s (%s)" label name)
                          label))
                  (right (roman-clock--int-to-roman french-year))
                  (available (- ind--line-width
                                (string-width right)
                                1))
                  (width (string-width left)))
       (when (> width available)
         (push (list date left width available)
               overflows))))
    (nreverse overflows)))

(provide 'ind)

;;; ind.el ends here
