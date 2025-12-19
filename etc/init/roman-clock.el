;;; roman-clock.el --- Roman Six Hour Clock with Ante Diem
;;; Based on initial code by Sage, m/2025-12-07/cpj

;;; commentary:

;;; Roman 6-Hour Clock — Local Time, Day Begins at 18:00
;;; Example: "Sat 7 Dec • ante diem VIII Idus Decembres • Tertia • 3:51"

;;; code:

(require 'time-date)
(require 'calendar)

(defgroup roman-clock nil
"Roman-style 6-hour clock using local time.
The Roman day begins at 18:00 local, and the *date label*
advances at 18:00 (i.e., after 18:00 it shows 'tomorrow')."
  :group 'time)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun roman-clock--sec-since-midnight (sec min hour)
"Return number of seconds since midnight from SEC, MIN, HOUR.
(Kept for possible reuse; not used in the core logic now.)"
  (+ sec (* 60 min) (* 3600 hour)))

(defun roman-clock--int-to-roman (n)
"Convert positive integer N to a Roman numeral string."
  (let ((pairs '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                 (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                 (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I")))
        (res ""))
    (dolist (p pairs)
      (let ((value (car p))
            (sym   (cdr p)))
        (while (>= n value)
          (setq res (concat res sym)
                n   (- n value)))))
    res))

(defconst roman-clock--latin-month-acc
  ;; 1..12 in accusative plural, used after "ante diem" / "pridie"
  [nil
   "Ianuarias"   ; Jan
   "Februarias"  ; Feb
   "Martias"     ; Mar
   "Apriles"     ; Apr
   "Maias"       ; May
   "Iunias"      ; Jun
   "Iulias"      ; Jul
   "Augustas"    ; Aug
   "Septembres"  ; Sep
   "Octobres"    ; Oct
   "Novembres"   ; Nov
   "Decembres"]  ; Dec
  )

(defconst roman-clock--latin-month-abl
  ;; 1..12 in ablative plural, used with Kalendis/Nonis/Idibus
  [nil
   "Ianuariis"    ; Jan
   "Februariis"   ; Feb
   "Martiis"      ; Mar
   "Aprilibus"    ; Apr
   "Maiis"        ; May
   "Iuniis"       ; Jun
   "Iuliis"       ; Jul
   "Augustis"     ; Aug
   "Septembribus" ; Sep
   "Octobribus"   ; Oct
   "Novembribus"  ; Nov
   "Decembribus"] ; Dec
  )

(defconst roman-clock--latin-day
  ;; 1..7
  [nil
   "dies Lunae"	   ; Monday (The Moon)
   "dies Martis"   ; Tuesday (Mars)
   "dies Mercurii" ; Wednesday (Mercury)
   "dies Iovis"	   ; Thursday (Jupiter)
   "dies Veneris"  ; Friday (Venus)
   "dies Saturni"  ; Saturday (Saturn)
   "dies Solis"]   ; Sunday (The Sun)
  )

(defun roman-clock--roman-calendar-string (day mon year)
"Return the Roman 'ante diem' style date string for DAY/MON/YEAR.

Examples:
  1 Dec  → \"Kalendis Decembribus\"
  4 Mar  → \"ante diem IV Nonas Martias\"
  6 Mar  → \"pridie Nonas Martias\"
  14 Mar → \"ante diem II Idus Martias\"
  31 Oct → \"pridie Kalendas Novembres\""
  (let* (;; Nones/Ides rules
         (long-ides-months '(3 5 7 10)) ; Mar, May, Jul, Oct
         (nones (if (memq mon long-ides-months) 7 5))
         (ides  (+ nones 8))
         (abl-month (aref roman-clock--latin-month-abl mon))
         (acc-month (aref roman-clock--latin-month-acc mon)))
    (cond
     ;; Fixed points
     ((= day 1)
      (format "Kalendis %s" abl-month))
     ((= day nones)
      (format "Nonis %s" abl-month))
     ((= day ides)
      (format "Idibus %s" abl-month))
     (t
      ;; Non-fixed: ante diem counting to the next key date.
      (let* ((target-kind nil)   ; 'kalends, 'nones, or 'ides
             (t-day nil)
             (t-mon nil)
             (t-year nil))
        (cond
         ;; Before Nones → count to Nones of this month
         ((< day nones)
          (setq target-kind 'nones
                t-day nones
                t-mon mon
                t-year year))
         ;; Between Nones and Ides → count to Ides of this month
         ((< day ides)
          (setq target-kind 'ides
                t-day ides
                t-mon mon
                t-year year))
         ;; After Ides → count to Kalends of next month
         (t
          (setq target-kind 'kalends)
          (if (= mon 12)
              (setq t-day 1
                    t-mon 1
                    t-year (1+ year))
            (setq t-day 1
                  t-mon (1+ mon)
                  t-year year))))

        (let* ((acc-month-target
                (aref roman-clock--latin-month-acc t-mon))
               (abl-month-target
                (aref roman-clock--latin-month-abl t-mon))
               (abs-given  (calendar-absolute-from-gregorian
                            (list mon day year)))
               (abs-target (calendar-absolute-from-gregorian
                            (list t-mon t-day t-year)))
               ;; Inclusive count
               (n (1+ (- abs-target abs-given))))
          (cond
           ;; On the key date itself (should already be handled above,
           ;; but kept for safety)
           ((= n 1)
            (pcase target-kind
              ('kalends (format "Kalendis %s" abl-month-target))
              ('nones   (format "Nonis %s"    abl-month-target))
              ('ides    (format "Idibus %s"   abl-month-target))))
           ;; Day before the key date → "pridie ..."
           ((= n 2)
            (pcase target-kind
              ('kalends (format "pridie Kalendas %s" acc-month-target))
              ('nones   (format "pridie Nonas %s"    acc-month-target))
              ('ides    (format "pridie Idus %s"     acc-month-target))))
           ;; General ante diem form
           (t
            (let* ((roman-n (roman-clock--int-to-roman n))
                   (key-name
                    (pcase target-kind
                      ('kalends "Kalendas")
                      ('nones   "Nonas")
                      ('ides    "Idus"))))
              (format "ante diem %s %s %s"
                      roman-n key-name acc-month-target))))))))))

(defun roman-clock--roman-numeral (n)
"Return a Roman numeral string for positive integer N (up to ~3999)."
  (let ((pairs '((1000 . "M") (900 . "CM")
                 (500 . "D")  (400 . "CD")
                 (100 . "C")  (90 . "XC")
                 (50 . "L")   (40 . "XL")
                 (10 . "X")   (9 . "IX")
                 (5 . "V")    (4 . "IV")
                 (1 . "I")))
        (res ""))
    (dolist (p pairs res)
      (while (>= n (car p))
        (setq res (concat res (cdr p))
              n   (- n (car p)))))))

(defun roman-clock--month-abbrev (mon)
"Return a short Latin-ish month label for MON (1–12)."
  (nth (1- mon)
       '("Ian." "Feb." "Mart." "Apr." "Mai." "Iun."
         "Iul." "Aug." "Sept." "Oct." "Nov." "Dec.")))

(defun roman-clock--nones-and-ides (mon)
"Return (Nones Ides) day numbers for month MON."
  (if (memq mon '(3 5 7 10))        ; March, May, July, October
      (list 7 15)
    (list 5 13)))

(defun roman-clock--days-in-month (mon year)
"Return number of days in month MON of YEAR."
  (calendar-last-day-of-month mon year))

(defun roman-clock--ante-diem-from-dmy (day mon year)
"Return a Roman 'ante diem' date string for DAY/MON/YEAR.

Examples (not exhaustive):
  1 March  → \"Kal. Mart.\"
  2 March  → \"a.d. VI Non. Mart.\"
  6 March  → \"prid. Non. Mart.\"
  14 March → \"a.d. II Id. Mart.\" (\"prid. Id.\" but we use a.d. II)
  16 March → \"a.d. XVII Kal. Apr.\"
  31 Jan   → \"prid. Kal. Feb.\""
  (let* ((month-name (roman-clock--month-abbrev mon))
         (nones-ides (roman-clock--nones-and-ides mon))
         (nones (nth 0 nones-ides))
         (ides  (nth 1 nones-ides)))
    (cond
     ;; Kalends (first of month)
     ((= day 1)
      (format "Kal. %s" month-name))

     ;; Before Nones
     ((< day nones)
      (let* ((n (- nones day -1)) ; nones - day + 1
             (roman (roman-clock--roman-numeral n)))
        (if (= n 2)
            (format "prid. Non. %s" month-name)
          (format "a.d. %s Non. %s" roman month-name))))

     ;; Nones
     ((= day nones)
      (format "Non. %s" month-name))

     ;; Before Ides
     ((< day ides)
      (let* ((n (- ides day -1))  ; ides - day + 1
             (roman (roman-clock--roman-numeral n)))
        (if (= n 2)
            (format "prid. Id. %s" month-name)
          (format "a.d. %s Id. %s" roman month-name))))

     ;; Ides
     ((= day ides)
      (format "Id. %s" month-name))

     ;; After Ides → count to Kalends of next month
     (t
      (let* ((days-this (roman-clock--days-in-month mon year))
             (next-mon (if (= mon 12) 1 (1+ mon)))
             (next-year (if (= mon 12) (1+ year) year))
             (next-name (roman-clock--month-abbrev next-mon))
             ;; inclusive counting to next Kalends:
             ;; e.g. 31 Jan → 2 days (pridie Kalendas)
             (n (+ (- days-this day) 2))
             (roman (roman-clock--roman-numeral n)))
        (if (= n 2)
            (format "prid. Kal. %s" next-name)
          (format "a.d. %s Kal. %s" roman next-name)))))))

(defun roman-clock-ante-diem-string (&optional abbreviated)
"Return ante diem-style Roman calendar date for the *current Roman day*.

Roman day: begins at local 18:00. After 18:00, the \"day\" is
treated as tomorrow’s civil date.

If ABBREVIATED is non-nil, return the abbreviated format
(using `roman-clock--ante-diem-from-dmy`). Otherwise, return the
long form (using `roman-clock--roman-calendar-string`)."

  (let* ((now-time (current-time))
         (now      (decode-time now-time))
         (hour     (nth 2 now))
         ;; Roman date: if after 18:00, use tomorrow’s civil date.
         (roman-fields (if (>= hour 18)
                           (decode-time (time-add now-time (days-to-time 1)))
                         now))
         (day   (nth 3 roman-fields))
         (month (nth 4 roman-fields))
         (year  (nth 5 roman-fields)))
    (if abbreviated
        (roman-clock--ante-diem-from-dmy day month year)
      (roman-clock--roman-calendar-string day month year))))

;;; ------------------------------------------------------------
;;; Core function: compute Roman time and format it
;;; ------------------------------------------------------------
(defun roman-clock-current-string ()
"Return the current local Roman 6-hour clock as a string.

Roman day:
  - Begins at local 18:00.
  - Date label advances at 18:00 (after 18:00, it's 'tomorrow').

Format example:
  \"Sat 7 Dec • ante diem VIII Idus Decembres • Tertia • 3:51\"."
  (let* ((now-time (current-time))
         (now      (decode-time now-time))  ; local time
         (sec      (nth 0 now))
         (min      (nth 1 now))
         (hour     (nth 2 now))
         (day      (nth 3 now))
         (mon      (nth 4 now))
         (year     (nth 5 now))
         (dow-now  (nth 6 now))             ; 0=Sun..6=Sat

         ;; --- Roman date label ---
         ;; After 18:00, the Roman *date* is the next civil day.
         (roman-time (if (>= hour 18)
                         (time-add now-time (days-to-time 1))
                       now-time))
         (r (decode-time roman-time))
         (roman-day   (nth 3 r))
         (roman-mon   (nth 4 r))
         (roman-year  (nth 5 r))
         (roman-dow   (nth 6 r))

         seconds-since-start)

    ;; --- Compute seconds since start of Roman day (18:00) ---
    (if (>= hour 18)
        ;; After 18:00: Roman day began today at 18:00.
        (let* ((start (encode-time 0 0 18 day mon year))
               (diff  (time-subtract now-time start)))
          (setq seconds-since-start (floor (float-time diff))))
      ;; Before 18:00: Roman day began yesterday at 18:00.
      (let* ((y-time (time-subtract now-time (days-to-time 1)))
             (y      (decode-time y-time))
             (y-day  (nth 3 y))
             (y-mon  (nth 4 y))
             (y-year (nth 5 y))
             (start  (encode-time 0 0 18 y-day y-mon y-year))
             (diff   (time-subtract now-time start)))
        (setq seconds-since-start (floor (float-time diff)))))

    ;; Keep within 24 hours (0–86399)
    (setq seconds-since-start (mod seconds-since-start 86400))

    ;; --- Convert elapsed seconds to Roman-style time ---
    (let* ((roman-hour-24 (/ seconds-since-start 3600)) ; 0–23 since 'start'
           (roman-hour-6  (mod roman-hour-24 6))        ; 0–5 within 6-hour block
           (roman-min     (/ (mod seconds-since-start 3600) 60))

           ;; 24 hours = 4 periods of 6 hours:
           ;; 0–5   → Prima
           ;; 6–11  → Secunda
           ;; 12–17 → Tertia
           ;; 18–23 → Quarta
           (roman-period-num   (1+ (/ roman-hour-24 6))) ; 1–4
           (roman-period-latin (nth (1- roman-period-num)
                                    '("Prima" "Secunda" "Tertia" "Quarta")))

           (weekday-names [ "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" ])
           (month-names   [ "" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" ])
           (weekday (aref weekday-names roman-dow))
           (month   (aref month-names roman-mon))

           (roman-cal (roman-clock--roman-calendar-string
                       roman-day roman-mon roman-year)))
      (format "%s %d %s • %s • %d:%02d"
              weekday roman-day month
              roman-period-latin
              roman-hour-6 roman-min))))

;;; ------------------------------------------------------------
;;; Interactive command
;;; ------------------------------------------------------------
(defun roman-clock ()
"Display the current local Roman 6-hour clock in the echo area."
  (interactive)
  (message "%s" (roman-clock-current-string)))

(defun roman-clock- ()
"Echo ante diem-style Roman calendar date and abbreviation for current Roman day.

Roman day begins at 18:00 local time. After 18:00, the date is treated as
tomorrow’s civil date."

  (interactive)
  (message "%s (%s)" (roman-clock-ante-diem-string) (roman-clock-ante-diem-string '(4))))

;; (defun roman-clock-ante-diem (&optional prefix)
;; "Echo ante diem-style Roman calendar date for the current Roman day.

;; Roman day begins at 18:00 local time. After 18:00, the date is
;; treated as tomorrow’s civil date.

;; With PREFIX (\\[universal-argument]), use the abbreviated format."

;;   (interactive "P")
;;   (message "%s" (roman-clock-ante-diem-string prefix)))

;;; Optional keybinding:
;(global-set-key (kbd "C-c d r") #'roman-clock)
;(global-set-key (kbd "C-c d R") #'roman-clock-ante-diem)

(provide 'roman-clock)

;;; roman-clock.el ends here
; LocalWords:  diem
