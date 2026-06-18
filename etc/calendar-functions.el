;;; calendar-functions.el --- my functions -*- lexical-binding: t; -*-

;;; Code:
(require 'calendar)
(require 'holidays)
(require 'lunar)
(require 'cal-julian)

(require 'cl-lib)
(require 'diary-lib)
(defvar displayed-year)
(defvar displayed-month)

(defun calendar-world-clock ()
  "Display a world clock buffer with times in various time zones."
  (interactive)
  (world-clock)
  (next-window-any-frame)
  (fit-window-to-buffer))

(defun display-current-date-and-time ()
  "Display current date and time."
  (interactive)
  (message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun list-holidays-this-year ()
  "Display holidays for the current or displayed year."
  (interactive)
  (list-holidays
   (or (and (boundp 'displayed-year) displayed-year)
       (nth 2 (calendar-current-date)))))

(defun list-holidays-and-diary-this-month ()
  "Display holidays and diary entries for the current or displayed month."
  (interactive)
  (let* ((month (or (and (boundp 'displayed-month) displayed-month)
                    (nth 0 (calendar-current-date))))
         (year  (or (and (boundp 'displayed-year) displayed-year)
                    (nth 2 (calendar-current-date))))
         (last-day (calendar-last-day-of-month month year))
         (buf (get-buffer-create "*Month Almanac*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s %d\n\n"
                        (calendar-month-name month) year))

        (dotimes (i last-day)
          (let* ((day (1+ i))
                 (date (list month day year))
                 (holidays (calendar-check-holidays date))
                 (diary-entries-list nil)
                 (diary-display-function #'ignore))
            (diary-list-entries date 1)
            (let ((entries (append holidays
                                   (mapcar #'cadr diary-entries-list))))
              (when entries
                (insert (format "%02d  %s\n" day (car entries)))
                (dolist (entry (cdr entries))
                  (insert (format "    %s\n" entry)))
                (insert "\n")))))

        (view-mode)))
    (switch-to-buffer buf)
    (goto-char (point-min))))

;; Harvest solar events via `solar-equinoxes-solstices' (DST-correct),
;; then filter/de-duplicate for full-year display.
(defun list-solar-events-this-year ()
  "Display equinoxes and solstices for the displayed or current year."
  (interactive)
  (let* ((year (or (and (boundp 'displayed-year) displayed-year)
                   (nth 2 (calendar-current-date))))
         (buf (get-buffer-create "*Solar Events*"))
         (entries nil))
    ;; Harvest Emacs' own formatted solar entries month by month.
    (dotimes (m 12)
      (let ((displayed-year year)
            (displayed-month (1+ m)))
        (setq entries (append entries (solar-equinoxes-solstices)))))
    ;; Keep only entries that belong to the requested year.
    (setq entries
          (cl-remove-if-not
           (lambda (entry)
             (= (nth 2 (car entry)) year))
           entries))
    ;; Remove duplicates, then sort by date.
    (setq entries (cl-delete-duplicates entries :test #'equal))
    (setq entries
          (sort entries
                (lambda (a b)
                  (< (calendar-absolute-from-gregorian (car a))
                     (calendar-absolute-from-gregorian (car b))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Solar events for %d\n\n" year))
        (dolist (entry entries)
          (insert (format "%-30s %s\n"
                          (cadr entry)
                          (calendar-date-string (car entry) nil t))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))


;; cleanup routines

(defun calendar-exit-kill ()
  "Kill Calendar when exiting."
  (interactive)
  (calendar-exit 'kill)
  (delete-other-windows)
  (let ((buffer "*wclock*"))
    (and (get-buffer buffer)
	 (kill-buffer buffer))))

;; https://emacs.stackexchange.com/questions/63533/exit-emacs-calendar-without-having-to-save-the-diary-file
(defun save-diary-before-calendar-exit (_)
  (let ((diary-buffer (get-file-buffer diary-file)))
    (or (not diary-buffer)
	(not (buffer-modified-p diary-buffer))
	(with-current-buffer diary-buffer (save-buffer)))))

;; https://www.emacswiki.org/emacs/DiaryMode
(defun alt-clean-equal-signs () "Make lines of = signs invisible."
	(goto-char (point-min))
	(let ((state buffer-read-only))
		(when state (setq buffer-read-only nil))
		(while (not (eobp)) (search-forward-regexp "^=+$" nil 'move)
		(add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
		(when state (setq buffer-read-only t))))

;; Replace the built-in `calendar-goto-info-node'.
;;
;; The stock version enlarges the Info window but leaves other windows
;; visible. We instead save the current window configuration, display
;; the Calendar/Diary manual full-frame, and restore the original
;; Calendar layout when quitting Info.

(defvar cpj/calendar--saved-window-configuration nil)

(defun calendar-goto-info-node ()
  "Go to the Info node for the calendar."
  (interactive)
  (let ((calendar-buffer (current-buffer)))
    (setq cpj/calendar--saved-window-configuration
          (current-window-configuration))
    (info "(emacs)Calendar/Diary")
    (delete-other-windows)
    (local-set-key
     (kbd "q")
     (lambda ()
       (interactive)
       (quit-window)
       (when (window-configuration-p cpj/calendar--saved-window-configuration)
         (set-window-configuration cpj/calendar--saved-window-configuration))
       (when (buffer-live-p calendar-buffer)
         (switch-to-buffer calendar-buffer)
         (calendar-redraw))))))


;;; Diary

(defconst cpj/diary-weekdays
  '((sunday    . 0)
    (monday    . 1)
    (tuesday   . 2)
    (wednesday . 3)
    (thursday  . 4)
    (friday    . 5)
    (saturday  . 6))
  "Weekday symbols used by `cpj/diary-weekday'.")

(defun cpj/diary-weekday (&rest weekdays)
  "Return non-nil if diary DATE falls on one of WEEKDAYS.

Each weekday is a symbol such as `monday', `tuesday', or `saturday'."
  (let ((today (calendar-day-of-week date)))
    (seq-some
     (lambda (weekday)
       (let ((day-number (alist-get weekday cpj/diary-weekdays)))
         (unless day-number
           (error "Unknown weekday: %S" weekday))
         (= today day-number)))
     weekdays)))

(provide 'calendar-routines)
;;; calendar-functions.el ends here.

;; Local Variables:
;; truncate-lines: -1
;; End:
