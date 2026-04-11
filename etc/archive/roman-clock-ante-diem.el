(defun roman-clock-ante-diem (&optional prefix)
"Echo ante diem-style Roman calendar date for the current Roman day.

Roman day begins at 18:00 local time. After 18:00, the date is
treated as tomorrow’s civil date.

With PREFIX (\\[universal-argument]), use the abbreviated format."

  (interactive "P")
  (message "%s" (roman-clock-ante-diem-string prefix)))

;; Optional keybinding:
(global-set-key (kbd "C-c d r") #'roman-clock)
(global-set-key (kbd "C-c d R") #'roman-clock-ante-diem)
