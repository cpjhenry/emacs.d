;;; daily-info.el --- Daily briefing buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Display a compact daily information buffer, combining external
;; command output, NOAA WWV space-weather summary, day-on-earth count,
;; biorhythm, holidays, and diary entries.

;;; Code:

(require 'calendar)
(require 'diary-lib)
(require 'holidays)

(require 'ind)

(declare-function wwv-summary "wwv")
(declare-function biorhythm-string "biorhythm")
(declare-function days-on-earth "biorhythm")
(declare-function wx-alert "scripts")
(declare-function commify-number "filesandbuffers")
(declare-function ordinal-number "filesandbuffers")
(declare-function turn-off-cursor "filesandbuffers")

(defvar diary-number-of-entries)

(defun daily-info--shell-string (command)
  "Return trimmed output of shell COMMAND, or nil if empty."
  (let ((s (string-trim
            (shell-command-to-string command))))
    (unless (string-empty-p s)
      s)))

(defun daily-info--ind-summary ()
  "Return the native Emacs `ind' daily summary."
  (ind-summary-string))

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

(defcustom daily-info-include-holidays t
  "Whether `daily-info' includes calendar holidays."
  :type 'boolean
  :group 'daily-info)

(defcustom daily-info-include-diary t
  "Whether `daily-info' includes diary entries."
  :type 'boolean
  :group 'daily-info)

(defun daily-info--diary-entries (date)
  "Return diary entry strings for DATE."
  (mapcar #'cadr
          (diary-list-entries date 1 t)))

(defun daily-info--items ()
  "Return daily information items for today."
  (let ((date (calendar-current-date)))
    (delq nil
          (append
           (list
            (wwv-summary)
            (format "My %s day (%s weeks, %.1f years)."
                    (ordinal-number (days-on-earth date))
                    (commify-number (weeks-on-earth date))
                    (years-on-earth date))
            (biorhythm-string))
           (when daily-info-include-holidays
             (calendar-check-holidays date))
           (when daily-info-include-diary
             (daily-info--diary-entries date))))))

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
  (org-agenda nil "a")
  (wx-alert)

  (let ((items (daily-info--items)))
    (switch-to-buffer "*daily-info*")

    (let ((inhibit-read-only t))
      (erase-buffer)

      (when-let* ((ind (daily-info--ind-summary)))
	(insert (string-trim-right ind))
	(insert "\n"))

      (daily-info--insert-items items)

      (when-let* ((birthdays (daily-info--birthday-summary)))
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
