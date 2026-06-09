;;; daily-info.el --- Daily briefing buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Display a compact daily information buffer, combining external
;; command output, NOAA WWV space-weather summary, day-on-earth count,
;; biorhythm, holidays, and diary entries.

;;; Code:

(require 'calendar)
(require 'diary-lib)
(require 'holidays)

(declare-function wwv-summary "wwv")
(declare-function biorhythm-string "biorhythm")
(declare-function days-on-earth "biorhythm")
(declare-function wx-alert "scripts")
(declare-function commify-number "filesandbuffers")
(declare-function turn-off-cursor "filesandbuffers")

(defvar diary-number-of-entries)

(defun daily-info--shell-string (command)
  "Return trimmed output of shell COMMAND, or nil if empty."
  (let ((s (string-trim
            (shell-command-to-string command))))
    (unless (string-empty-p s)
      s)))

(defun daily-info--ind-summary ()
  "Return `ind' daily summary."
  (daily-info--shell-string "ind -faruiq"))

(defconst daily-info-birthday-command
  (concat
   "ssh bullwinkle "
   "\"/usr/local/bin/icalBuddy -nc -df '%RD' "
   "-ic Birthdays eventsFrom:yesterday to:today+21\" "
   "2>/dev/null | sed -e "
   "\"s/'s Birthday//;"
   "s/ (age.*)//;"
   "s/, today//;"
   "s/ from now//\""))

(defun daily-info--birthday-summary ()
  "Return upcoming birthday summary from remote icalBuddy."
  (daily-info--shell-string daily-info-birthday-command))

(defun daily-info--diary-entries (date)
  "Return diary entry strings for DATE."
  (let ((diary-entries-list nil)
        (diary-display-function #'ignore))
    (diary-list-entries date 1)
    (mapcar #'cadr diary-entries-list)))

(defun daily-info--items ()
  "Return daily information items for today."
  (let* ((date (calendar-current-date))
         (holidays (calendar-check-holidays date))
         (diary-entries (daily-info--diary-entries date)))
    (delq nil
          (append
           (list
            (wwv-summary)
            (format "Day %s on Earth"
                    (commify-number (days-on-earth)))
            (biorhythm-string))
           holidays
           diary-entries))))

(defun daily-info--insert-items (items)
  "Insert ITEMS as a simple bullet list."
  (when items
    (insert "\n")
    (dolist (item items)
      (insert "- " item "\n"))))

;;;###autoload
(defun di ()
  "Display daily information."
  (interactive)
  ;; Load buffers in reverse reading order, so that *daily-info* is
  ;; the final selected buffer.
  (wx-alert)

  (let ((items (daily-info--items)))
    (switch-to-buffer "*daily-info*")

    (let ((inhibit-read-only t))
      (erase-buffer)

      (when-let ((ind (daily-info--ind-summary)))
	(insert (string-trim-right ind))
	(insert "\n"))

      (daily-info--insert-items items)

      (when-let ((birthdays (daily-info--birthday-summary)))
	(insert "\n")
	(insert (string-trim-right birthdays))
	(insert "\n"))

      (when (get-buffer "diary")
	(kill-buffer "diary"))
      (goto-char (point-min)))

    (view-mode)
    (turn-off-cursor)
    (message "'cbc' / 'xkcd' / 'elfeed'")))

(provide 'daily-info)

;;; daily-info.el ends here

; LocalWords:  faruiq usr icalBuddy nc df eventsFrom dev
; LocalWords:  filesandbuffers
