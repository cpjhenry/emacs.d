;; calendar settings

(setq calendar-christian-all-holidays-flag t)
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-general-holidays nil)
(setq holiday-bahai-holidays nil)
;(setq holiday-hebrew-holidays nil)
;(setq holiday-islamic-holidays nil)
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
	(holiday-fixed 06 24  "Midsummer Day")
	(holiday-fixed 09 30  "Truth and Reconciliation")
	(holiday-fixed 12 11  "Statute of Westminster")))

(setq lunar-phase-names '(
	"● New Moon"
	"☽ First Quarter Moon"
	"○ Full Moon"
	"☾ Last Quarter Moon"))

(setq zoneinfo-style-world-list '(
	("America/Vancouver" "Vancouver")
	("America/Edmonton" "Edmonton")
	("America/Toronto" "Ottawa")
	("America/Halifax" "Halifax")
	("America/St_Johns" "St. John's")
	("America/Marigot" "St. Martin")
	("Europe/London" "Edinburgh")
	("Europe/Lisbon" "Lisbon")
	("Europe/Paris" "Paris")
	("Europe/Istanbul" "Ankara")
	("Asia/Calcutta" "Bangalore")
	("Asia/Shanghai" "Beijing")
	("Asia/Tokyo" "Tokyo")))

(defun list-hols () (interactive) (list-holidays (string-to-number (format-time-string "%Y"))))
(defun my/save-diary-before-calendar-exit (_)
	(let ((diary-buffer (get-file-buffer diary-file)))
    	(or (not diary-buffer)
			(not (buffer-modified-p diary-buffer))
			(with-current-buffer diary-buffer (save-buffer)))))

(defun display-current-time () (interactive) (message (format-time-string "%Y-%m-%d %H:%M:%S")))
