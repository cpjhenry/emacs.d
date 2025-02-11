;;; calendar --- settings / functions
;;; Commentary:
;;; Code:

(setq holiday-local-holidays '( ; National / Provincial Holidays and Commemorations
	(holiday-fixed 01 01  "New Year's Day")
	(holiday-fixed 02 02  "Groundhog Day")
	(holiday-fixed 02 14  "Valentine's Day")
	(holiday-fixed 04 01  "April Fools' Day")
	(holiday-float 05 0 2 "Mother's Day")
	(holiday-float 06 0 3 "Father's Day")
	(holiday-fixed 07 01  "Canada Day")
	(holiday-float 08 1 1 "Civic Holiday")
	(holiday-float 09 1 1 "Labour Day")
	(holiday-float 10 1 2 "Thanksgiving")
	(holiday-fixed 10 31  "Halloween")
	(holiday-fixed 11 11  "Remembrance Day")

	(holiday-fixed 01 21  "Lincoln Alexander Day")
	(holiday-float 02 1 3 "Family Day")
	(holiday-fixed 02 15  "National Flag Day")
	(holiday-float 03 1 2 "Commonwealth Day")
	(holiday-fixed 04 06  "Tartan Day")
	(holiday-fixed 04 09  "Vimy Ridge Day")
	(holiday-fixed 06 21  "Indigenous Peoples Day")
	(holiday-fixed 09 30  "Truth and Reconciliation")
	(holiday-float 11 0 2 "Remembrance Sunday")
	(holiday-fixed 12 11  "Statute of Westminster")))

(setq holiday-other-holidays '(
	(holiday-float 1 1 3 "Martin Luther King Day")
	(holiday-float 06 6 1 "Midsummer" (floor (nth 1 (solar-equinoxes/solstices 1 displayed-year))))
	(holiday-sexp '(if (zerop (% year 4)) (calendar-gregorian-from-absolute (1+
		(calendar-dayname-on-or-before 1 (+ 6 (calendar-absolute-from-gregorian
		(list 11 1 year)))))))
		"US Presidential Election")
	(holiday-float 11 4 4 "US Thanksgiving")
	(holiday-advent -11 "Prayer & Repentance")
	(holiday-fixed 12 (floor (nth 1 (solar-equinoxes/solstices 3 displayed-year))) "Midwinter")))

(setq holiday-bahai-holidays '(
	(holiday-bahai-new-year)))

(setq holiday-oriental-holidays '(
	(holiday-chinese-new-year)
	(if calendar-chinese-all-holidays-flag (append
		(holiday-chinese 1 15 "Lantern Festival")
		(holiday-chinese-qingming)
		(holiday-chinese 5 5 "Dragon Boat Festival")
		(holiday-chinese 7 7 "Double Seventh Festival")
		(holiday-chinese 7 15 "Ghost Festival")
		(holiday-chinese 8 15 "Mid-Autumn Festival")
		(holiday-chinese 9 9 "Double Ninth Festival")))))

(setq holiday-islamic-holidays '(
	(holiday-islamic-new-year)
	(holiday-islamic 9 1 "Ramadan Begins")
	(holiday-islamic 10 1 "Id-al-Fitr")))

(require 'lunar)
(setq lunar-phase-names '(
	"● New Moon"
	"☽ First Quarter Moon"
	"○ Full Moon"
	"☾ Last Quarter Moon"))

(setq zoneinfo-style-world-list '(
	("Pacific/Honolulu" "Hawai'i")
	("America/Los_Angeles" "Cupertino")
	("America/Vancouver" "Vancouver")
	("America/Edmonton" "Edmonton")
	("America/Regina" "Saskatoon")
	("America/Winnipeg" "Winnipeg")
	("America/Toronto" "Ottawa")
	("America/Halifax" "Halifax")
	("America/St_Johns" "St. John's")
	("America/Marigot" "St. Martin")
	("UTC" "UTC")
	("Europe/London" "Edinburgh")
	("Europe/Lisbon" "Lisbon")
	("Europe/Paris" "Paris")
	("Europe/Rome" "Rome")
	("Europe/Istanbul" "Ankara")
	("Asia/Calcutta" "Bangalore")
	("Asia/Shanghai" "Beijing")
	("Asia/Tokyo" "Tokyo")
	("Australia/Sydney" "Sydney")
	("NZ" "Wellington")))

;; functions
(defun calendar-exit-kill ()
	(interactive)
	(calendar-exit 'kill)
	(let ((buffer "*wclock*"))(and (get-buffer buffer) (kill-buffer buffer))))

(defun save-diary-before-calendar-exit (_)
	(let ((diary-buffer (get-file-buffer diary-file))) (or (not diary-buffer)
		(not (buffer-modified-p diary-buffer))
		(with-current-buffer diary-buffer (save-buffer)))))

(defun display-current-date-and-time () "Display current date and time."
	(interactive)
	(message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun list-holidays-this-year () "Display holidays for displayed (or current) year."
	(interactive)
	(if (boundp 'displayed-year) (list-holidays displayed-year)
	(list-holidays (string-to-number (format-time-string "%Y")))))

(defun calendar-world-clock () "Display a world clock buffer with times in various time zones."
	(interactive)
	(world-clock)
	(next-window-any-frame)
	(fit-window-to-buffer))

;; https://www.emacswiki.org/emacs/DiaryMode
(defun alt-clean-equal-signs () "Make lines of = signs invisible."
	(goto-char (point-min))
	(let ((state buffer-read-only))
		(when state (setq buffer-read-only nil))
		(while (not (eobp)) (search-forward-regexp "^=+$" nil 'move)
		(add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
		(when state (setq buffer-read-only t))))

;; Local Variables:
;; truncate-lines: -1
;; End:
