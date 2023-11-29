;; calendar settings

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

(setq holiday-other-holidays '(
	(holiday-advent -11 "Prayer & Repentance")))

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
	("NZ" "Wellington")
	))

(defun save-diary-before-calendar-exit (_)
	(let ((diary-buffer (get-file-buffer diary-file)))
    	(or (not diary-buffer)
			(not (buffer-modified-p diary-buffer))
			(with-current-buffer diary-buffer (save-buffer)))))

(defun display-current-time () (interactive)
	(message (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun calendar-holidays () (interactive)
	(list-holidays (string-to-number (format-time-string "%Y"))))

(defun calendar-world-clock () (interactive)
	(world-clock)(next-window-any-frame)(fit-window-to-buffer))
