;;; calendar-local-holidays.el --- National / Provincial Holidays and Commemorations
;;; commentary:

;;; code:
(setq holiday-general-holidays nil
      holiday-local-holidays '(
	  (holiday-fixed 1 1    "New Year's Day")
	  (holiday-fixed 2 2    "Groundhog Day")
	  (holiday-fixed 2 14   "Valentine's Day")
	  (holiday-fixed 4 1    "April Fools' Day")
	  (holiday-float 5 0 2  "Mother's Day")
	  (holiday-float 6 0 3  "Father's Day")
	  (holiday-fixed 7 1    "Canada Day")
	  (holiday-float 8 1 1  "Civic Holiday")
	  (holiday-float 9 1 1  "Labour Day")
	  (holiday-float 10 1 2 "Thanksgiving")
	  (holiday-fixed 10 31  "Halloween")
	  (holiday-fixed 11 11  "Remembrance Day")
	  (holiday-fixed 1 21   "Lincoln Alexander Day")
	  (holiday-float 2 1 3  "Family Day")
	  (holiday-fixed 2 15   "National Flag Day")
	  (holiday-float 3 1 2  "Commonwealth Day")
	  (holiday-fixed 4 6    "Tartan Day")
	  (holiday-fixed 4 9    "Vimy Ridge Day")
	  (holiday-fixed 6 21   "Indigenous Peoples Day")
	  (holiday-fixed 9 30   "Truth and Reconciliation")
	  (holiday-float 11 0 2 "Remembrance Sunday")
	  (holiday-fixed 12 11  "Statute of Westminster")
	  (holiday-fixed 12 26  "Boxing Day"))

      	holiday-bahai-holidays '( ;; New Year is based on date of vernal equinox
	  (holiday-fixed 3 (floor (nth 1 (solar-equinoxes/solstices 1 displayed-year)))
	  (format "Bahá’í New Year (Naw-Ruz) %d" (- displayed-year (1- 1844)))))

	holiday-oriental-holidays '(
	  (holiday-chinese-new-year)
	  (if calendar-chinese-all-holidays-flag (append
	    (holiday-chinese 1 15 "Lantern Festival")
	    (holiday-chinese-qingming)
	    (holiday-chinese 5 5 "Dragon Boat Festival")
	    (holiday-chinese 7 7 "Double Seventh Festival")
	    (holiday-chinese 7 15 "Ghost Festival")
	    (holiday-chinese 8 15 "Mid-Autumn Festival")
	    (holiday-chinese 9 9 "Double Ninth Festival"))))

	holiday-islamic-holidays '(
	  (holiday-islamic-new-year)
	  (holiday-islamic 9 1 "Ramadan Begins")
	  (holiday-islamic 10 1 "Id-al-Fitr")))

(provide 'local-holidays)
;;; calendar-local-holidays.el ends here.
