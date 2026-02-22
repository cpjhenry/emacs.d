;;; roman-clock-period-notify-mode.el -- Notify of clock period changes.

;;; commentary:

;;; ------------------------------------------------------------
;;; Roman clock period-change notifications
;;; Requires: roman-clock-current-string and roman-clock--sec-since-midnight
;;; ------------------------------------------------------------

;;; code:

(defvar roman-clock-period-notify-timer nil
  "Timer used by `roman-clock-period-notify-mode' to schedule notifications.")

(defun roman-clock--current-state ()
  "Return current Roman clock state as a plist.

Plist keys:
  :seconds-since-start   Seconds since start of the Roman day (18:00).
  :period-num            Period number 1–4.
  :period-name           Period Latin name (\"Prima\"..\"Quarta\").
  :roman-hour            Hour in current 6-hour block (0–5).
  :roman-minute          Minute (0–59).
  :day                   Today's civil day.
  :month                 Today's civil month.
  :year                  Today's civil year.
  :dow                   Today's day of week (0=Sun..6=Sat)."
  (let* ((now-time (current-time))
         (now      (decode-time now-time))  ; local time
         (sec      (nth 0 now))
         (min      (nth 1 now))
         (hour     (nth 2 now))
         (day      (nth 3 now))
         (mon      (nth 4 now))
         (year     (nth 5 now))
         (dow      (nth 6 now))
         seconds-since-start)

    ;; Same Roman-day logic as in `roman-clock-current-string`:
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

    (setq seconds-since-start (mod seconds-since-start 86400))

    (let* ((roman-hour-24 (/ seconds-since-start 3600)) ; 0–23 since 18:00
           (roman-hour    (mod roman-hour-24 6))        ; 0–5 in block
           (roman-min     (/ (mod seconds-since-start 3600) 60))
           (period-num    (1+ (/ roman-hour-24 6)))     ; 1–4
           (period-name   (nth (1- period-num)
                               '("Prima" "Secunda" "Tertia" "Quarta"))))
      (list :seconds-since-start seconds-since-start
            :period-num          period-num
            :period-name         period-name
            :roman-hour          roman-hour
            :roman-minute        roman-min
            :day                 day
            :month               mon
            :year                year
            :dow                 dow))))

(defun roman-clock--seconds-until-next-period ()
  "Return seconds until the next Roman period boundary."
  (let* ((state (roman-clock--current-state))
         (seconds-since-start (plist-get state :seconds-since-start))
         (roman-hour-24 (/ seconds-since-start 3600)) ; 0–23
         (current-period    (1+ (/ roman-hour-24 6))) ; 1–4
         (next-period-start-hours (* current-period 6)) ; 6,12,18,24
         (next-period-start-seconds (* next-period-start-hours 3600)))
    (max 1 (- next-period-start-seconds seconds-since-start))))

(defun roman-clock--notify (state)
  "Show a notification for a Roman period change using STATE plist."
  (let* ((period-name (plist-get state :period-name))
         (period-num  (plist-get state :period-num))
         (msg (format "Roma %s" period-name)))
    ;; Echo area
    (message "%s" msg)
    ;; Desktop notification if available (Linux/GTK etc.)
    (when (fboundp 'notifications-notify)
      (notifications-notify
       :title "Roman Clock"
       :body  msg))))

(defun roman-clock--period-timer-callback ()
  "Callback run at each Roman period boundary.
Reschedules itself for the next period change."
  (setq roman-clock-period-notify-timer nil)
  (let ((state (roman-clock--current-state)))
    (roman-clock--notify state))
  (roman-clock--schedule-next-period-notification))

(defun roman-clock--schedule-next-period-notification ()
  "Schedule a timer for the next Roman period boundary."
  (when roman-clock-period-notify-timer
    (cancel-timer roman-clock-period-notify-timer)
    (setq roman-clock-period-notify-timer nil))
  (let ((seconds (roman-clock--seconds-until-next-period)))
    (setq roman-clock-period-notify-timer
          (run-at-time seconds nil #'roman-clock--period-timer-callback))))

;;; ------------------------------------------------------------
;;; Minor mode: toggle period-change notifications
;;; ------------------------------------------------------------
(define-minor-mode roman-clock-period-notify-mode
  "Toggle notifications at Roman period changes.

When enabled, Emacs will notify you (echo area, and desktop
notification if supported) each time the Roman 6-hour periods
change (Prima → Secunda → Tertia → Quarta)."
  :global t
  :group 'roman-clock
  (if roman-clock-period-notify-mode
      (roman-clock--schedule-next-period-notification)
    (when roman-clock-period-notify-timer
      (cancel-timer roman-clock-period-notify-timer)
      (setq roman-clock-period-notify-timer nil))))

(provide 'roman-clock-period-notify-mode)

;;; roman-clock-period-notify-mode.el ends here
