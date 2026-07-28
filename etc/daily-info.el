;;; daily-info.el --- Daily briefing buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Display a compact daily information buffer combining `ind' calendar
;; summaries, upcoming birthdays, holidays, diary entries, NOAA WWV
;; space weather, day-on-earth count, and biorhythm.

;; Configure this package with `use-package' in `init.el'.
;;
;; Do not move the birthday `org-agenda-custom-commands' entry into this file.
;; Keep its `add-to-list' form in the `use-package daily-info' declaration so
;; that regenerating the Org-derived configuration also restores the command.
;;
;; `cpj/org-agenda-birthdays' assumes that command key "b" has been installed.

;;; Code:
(require 'calendar)
(require 'diary-lib)
(require 'holidays)

(require 'ind)
(require 'calendar-data)

(declare-function wwv-summary "wwv")
(declare-function biorhythm-string "biorhythm")
(declare-function days-on-earth "biorhythm")
(declare-function wx-alert "scripts")
(declare-function commify-number "filesandbuffers")
(declare-function ordinal-number "filesandbuffers")
(declare-function turn-off-cursor "filesandbuffers")

(defvar diary-number-of-entries)

(defun daily-info--ind-summary ()
  "Return the native Emacs `ind' daily summary."
  (ind-summary-string))

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

(defun daily-info--fontify-birthday-age (title)
  "Fontify an ordinal age immediately preceding \"Birthday\" in TITLE."
  (if (string-match
       "\\b\\([0-9]+\\)\\(?:st\\|nd\\|rd\\|th\\)\\( Birthday\\b\\)"
       title)
      (replace-match
       (concat
        (ordinal-number
         (string-to-number (match-string 1 title)))
        (match-string 2 title))
       t t title)
    title))

(defun daily-info--upcoming-birthdays (&optional days)
  "Return birthdays within DAYS as a formatted string.

DAYS defaults to 14 and includes today.  Return nil when there
are no birthdays in that period."
  (let* ((days (or days 14))
         (today (time-to-days (current-time)))
         birthdays)
    (with-current-buffer (find-file-noselect calendar-data-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward "^\\* Birthdays[ \t]*$" nil t)
         (let ((end (save-excursion
                      (org-end-of-subtree t t))))
           (while (re-search-forward "^\\*\\* \\(.+\\)$" end t)
             (let ((title (match-string-no-properties 1)))
               (forward-line 1)
               (when (looking-at org-ts-regexp-both)
                 (let* ((time
                         (org-time-string-to-time
                          (match-string-no-properties 0)))
                        (offset (- (time-to-days time) today)))
                   (when (<= 0 offset days)
                     (push (list time offset title)
                           birthdays))))))))))
    (when birthdays
      (mapconcat
       (pcase-lambda (`(,time ,offset ,title))
	 (format "%s  %s (%s)"
		 (format-time-string "%e %b" time)
		 (daily-info--fontify-birthday-age title)
          (cond
           ((zerop offset) "today")
           ((= offset 1) "1 day from now")
           (t (format "%d days from now" offset)))))
       (sort birthdays
             (lambda (a b)
               (time-less-p (car a) (car b))))
       "\n"))))

;;;###autoload
(defun di ()
  "Display daily information."
  (interactive)
  ;; Load buffers in reverse reading order, so that *daily-info* is
  ;; the final selected buffer.
  (my/org-agenda-list)
  (wx-alert)

  (switch-to-buffer "*daily-info*")

  (let ((inhibit-read-only t))
    (erase-buffer)

    (when-let* ((ind (daily-info--ind-summary)))
      (insert (string-trim-right ind))
      (insert "\n"))

    (daily-info--insert-items (daily-info--items))

    (when-let* ((birthdays (daily-info--upcoming-birthdays)))
      (insert "\n" birthdays "\n")))

  (view-mode)
  (turn-off-cursor))

;;;###autoload
(defun cpj/org-agenda-birthdays ()
  "Refresh calendar data and display the birthday agenda."
  (interactive)
  (calendar-data-refresh-if-stale)
  (org-agenda nil "b"))

(provide 'daily-info)

;;; daily-info.el ends here

; LocalWords:  faruiq usr icalBuddy nc df eventsFrom dev
; LocalWords:  filesandbuffers
