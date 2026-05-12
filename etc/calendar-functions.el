;;; calendar-functions.el --- settings / functions
;;; Commentary:

;;; Code:
(require 'calendar)
(require 'holidays)

(defun calendar-exit-kill ()
  "Kill Calendar when exiting."
  (interactive)
  (calendar-exit 'kill)
  (let ((buffer "*wclock*"))
    (and (get-buffer buffer)
	 (kill-buffer buffer))))

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

(defun calendar-world-clock ()
  "Display a world clock buffer with times in various time zones."
  (interactive)
  (world-clock)
  (next-window-any-frame)
  (fit-window-to-buffer))

;; Harvest solar events via `solar-equinoxes-solstices' (DST-correct),
;; then filter/de-duplicate for full-year display.
(require 'cl-lib)
(defvar displayed-year)
(defvar displayed-month)

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

;; Biorhythms
;; https://rosettacode.org/wiki/Biorhythms
(defun biorhythm ()
  "Show today's biorhythm. Set variable `birthdate' in format MDY."
  (interactive)
  (let* ((diff (abs (- (string-to-number (calendar-astro-date-string birthdate))
		       (string-to-number (calendar-astro-date-string)))))
         (rhyt '(23 28 33))
         (perc (mapcar (lambda (x) (round (* 100 (sin
               (* 2 pi diff (/ 1.0 x)))))) rhyt)))
    (message "age: %i  physical: %i%%  emotional: %i%%  intellectual: %i%%"
             diff (car perc) (cadr perc) (caddr perc))))

(provide 'calendar-routines)
;;; calendar-functions.el ends here.

;; Local Variables:
;; truncate-lines: -1
;; End:
