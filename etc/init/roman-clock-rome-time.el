;;; roman-clock-rome-time --- Display Roman Time centred on Rome
;;; commentary:

;;; Roman 6-Hour Clock — Rome Time, Day Begins at 18:00
;;; Displays e.g. "Fri 7 Dec • Tertia • 3:51 • a.d. VIII Id. Dec."

(require 'time-date)
(require 'calendar)

(defgroup roman-clock nil
  "Roman-style 6-hour clock using local time. Day begins at 18:00."
  :group 'time)

;;; ------------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------------

(defun roman-clock--sec-since-midnight (sec min hour)
  "Return number of seconds since midnight from SEC, MIN, HOUR."
  (+ sec (* 60 min) (* 3600 hour)))

(defun roman-clock--int-to-roman (n)
  "Convert integer N to a simple Roman numeral string."
  (let ((pairs '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                 (100  . "C") (90  . "XC") (50  . "L") (40  . "XL")
                 (10   . "X") (9   . "IX") (5   . "V") (4   . "IV")
                 (1    . "I")))
        (result ""))
    (dolist (p pairs result)
      (let ((value (car p))
            (sym   (cdr p)))
        (while (>= n value)
          (setq result (concat result sym))
          (setq n (- n value)))))))

(defun roman-clock--ante-diem-helper (day month year
                                          target-day target-month target-year
                                          anchor-short anchor-month-name)
  "Internal helper to build ante diem string from DAY/MONTH/YEAR to TARGET.
ANCHOR-SHORT is \"Kal.\", \"Non.\", or \"Id.\", and ANCHOR-MONTH-NAME is an abbrev."
  (let* ((today-abs  (calendar-absolute-from-gregorian
                      (list month day year)))
         (target-abs (calendar-absolute-from-gregorian
                      (list target-month target-day target-year)))
         ;; Inclusive counting: ante diem N = (target - today) + 1
         (n          (+ 1 (- target-abs today-abs))))
    (cond
     ((= n 2)
      (format "prid. %s %s" anchor-short anchor-month-name))
     (t
      (format "a.d. %s %s %s"
              (roman-clock--int-to-roman n)
              anchor-short
              anchor-month-name)))))

(defun roman-clock--ante-diem-for-date (day month year)
  "Return a compact ante diem-style Roman calendar date for DAY/MONTH/YEAR.

Examples:
  Kal. Mart.
  Non. Ian.
  Id. Mart.
  a.d. VIII Id. Dec.
  prid. Kal. Ian."
  (let* ((nones (if (memq month '(3 5 7 10)) 7 5))
         (ides  (if (memq month '(3 5 7 10)) 15 13))
         ;; Very simple Latin-ish month abbreviations
         (month-abbrevs ["" "Ian" "Feb" "Mart" "Apr" "Mai" "Iun"
                         "Iul" "Aug" "Sept" "Oct" "Nov" "Dec"])
         (mname (aref month-abbrevs month)))
    (cond
     ;; Kalends (1st)
     ((= day 1)
      (format "Kal. %s" mname))

     ;; Before Nones
     ((< day nones)
      (roman-clock--ante-diem-helper
       day month year
       nones month year
       "Non." mname))

     ;; Nones
     ((= day nones)
      (format "Non. %s" mname))

     ;; Before Ides
     ((< day ides)
      (roman-clock--ante-diem-helper
       day month year
       ides month year
       "Id." mname))

     ;; Ides
     ((= day ides)
      (format "Id. %s" mname))

     ;; After Ides → count to next month's Kalends
     (t
      (let* ((next-month (if (= month 12) 1 (1+ month)))
             (next-year  (if (= month 12) (1+ year) year))
             (next-name  (aref month-abbrevs next-month)))
        (roman-clock--ante-diem-helper
         day month year
         1 next-month next-year
         "Kal." next-name))))))

(defun roman-clock-ante-diem-string ()
  "Return ante diem-style Roman calendar date for the *current Roman day*.

Roman day: begins at local 18:00.
After 18:00, the \"day\" is treated as tomorrow’s civil date."
  (let* ((now-time (current-time))
         (now      (decode-time now-time "Europe/Rome"))
         (hour     (nth 2 now))
         ;; Roman date: if after 18:00, use tomorrow’s civil date.
         (roman-fields (if (>= hour 18)
                           (decode-time (time-add now-time (days-to-time 1)) "Europe/Rome")
                         now))
         (day   (nth 3 roman-fields))
         (month (nth 4 roman-fields))
         (year  (nth 5 roman-fields)))
    (roman-clock--ante-diem-for-date day month year)))

;;; ------------------------------------------------------------
;;; Core clock string
;;; ------------------------------------------------------------

(defun roman-clock-current-string ()
  "Return the current local Roman 6-hour clock as a string.

Roman day begins at local 18:00. After 18:00, the *displayed date*
is the next civil day. Example:

  \"Fri 7 Dec • Tertia • 3:51 • a.d. VIII Id. Dec.\""
  (let* ((now-time (current-time))
         (now      (decode-time now-time "Europe/Rome"))  ; local time
         (sec      (nth 0 now))
         (min      (nth 1 now))
         (hour     (nth 2 now))
         (day      (nth 3 now))
         (mon      (nth 4 now))
         (year     (nth 5 now))

         ;; Roman date: flip at 18:00 → after 18:00 it’s “tomorrow”.
         (roman-fields (if (>= hour 18)
                           (decode-time (time-add now-time (days-to-time 1)) "Europe/Rome")
                         now))
         (roman-day (nth 3 roman-fields))
         (roman-mon (nth 4 roman-fields))
         (roman-year (nth 5 roman-fields))
         (roman-dow (nth 6 roman-fields))

         seconds-since-start)

    ;; Determine when the current Roman day *started* (18:00 today or yesterday).
    (if (>= hour 18)
        ;; After 18:00: Roman day began today at 18:00.
        (let* ((start (encode-time 0 0 18 day mon year))
               (diff  (time-subtract now-time start)))
          (setq seconds-since-start (floor (float-time diff))))
      ;; Before 18:00: Roman day began yesterday at 18:00.
      (let* ((y-time (time-subtract now-time (days-to-time 1)))
             (y      (decode-time y-time "Europe/Rome"))
             (y-day  (nth 3 y))
             (y-mon  (nth 4 y))
             (y-year (nth 5 y))
             (start  (encode-time 0 0 18 y-day y-mon y-year))
             (diff   (time-subtract now-time start)))
        (setq seconds-since-start (floor (float-time diff)))))

    ;; Keep within 24 hours (0–86399)
    (setq seconds-since-start (mod seconds-since-start 86400))

    ;; Convert elapsed seconds to Roman-style time and period.
    (let* ((roman-hour-24 (/ seconds-since-start 3600)) ; hours since 18:00 (0–23)
           (roman-hour-6  (mod roman-hour-24 6))        ; hour within block: 0–5
           (roman-min     (/ (mod seconds-since-start 3600) 60))

           ;; 24 hours = 4 periods of 6 hours
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
           (ante-diem (roman-clock-ante-diem-string)))
      (format "%s %d %s • %s • %d:%02d"
              weekday roman-day month
              roman-period-latin
              roman-hour-6 roman-min))))

;;; ------------------------------------------------------------
;;; Interactive commands
;;; ------------------------------------------------------------

(defun roman-clock-echo ()
  "Display the current local Roman 6-hour clock in the echo area."
  (interactive)
  (message "%s" (roman-clock-current-string)))

(defun roman-clock-echo-ante-diem ()
  "Display the ante diem Roman calendar date for the current moment."
  (interactive)
  (message "%s" (roman-clock-ante-diem-string)))

;;; Optional keybindings:
;; (global-set-key (kbd "C-c r")   #'roman-clock-echo)
;; (global-set-key (kbd "C-c R a") #'roman-clock-echo-ante-diem)

;;; roman-clock-rome-time.el ends here
