;;; regnal-years.el --- British, Canadian, and papal regnal years -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar, history
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Calculate:
;;
;;   - British/Canadian regnal years from 1801-01-01
;;   - Modern papal pontifical years from 1929-02-11
;;
;; Regnal years begin on the anniversary of accession or, for popes,
;; election and acceptance.
;;
;; Dates are represented in Emacs calendar order:
;;
;;     (MONTH DAY YEAR)
;;
;; Dates beyond the current local calendar year return nil, because
;; the continuation of a living sovereign's or pope's reign cannot be
;; assumed.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup regnal-years nil
  "British, Canadian, and papal regnal years."
  :group 'calendar)

(defconst regnal-years-monarchical-epoch '(1 1 1801)
  "Earliest date for British/Canadian regnal-year display.")

(defconst regnal-years-papal-epoch '(2 11 1929)
  "Earliest date for modern papal regnal-year display.

This is the date of the Lateran Treaty.")

(defconst regnal-years-monarchs
  '(((9 8 2022)   . "Cha 3")
    ((2 6 1952)   . "Eliz 2")
    ((12 11 1936) . "Geo 6")
    ((1 20 1936)  . "Edw 8")
    ((5 6 1910)   . "Geo 5")
    ((1 22 1901)  . "Edw 7")
    ((6 20 1837)  . "Vict")
    ((6 26 1830)  . "Wm 4")
    ((1 29 1820)  . "Geo 4")
    ((10 25 1760) . "Geo 3"))
  "British and Canadian monarchs, newest accession first.")

(defconst regnal-years-popes
  '(((5 8 2025)   . "Leo XIV")
    ((3 13 2013)  . "Francis")
    ((4 19 2005)  . "Benedict XVI")
    ((10 16 1978) . "John Paul II")
    ((8 26 1978)  . "John Paul I")
    ((6 21 1963)  . "Paul VI")
    ((10 28 1958) . "John XXIII")
    ((3 2 1939)   . "Pius XII")
    ((2 6 1922)   . "Pius XI"))
  "Modern popes, newest election first.

Dates are dates of election and acceptance, not inauguration.")

(defun regnal-years--date-on-or-after-p (date reference)
  "Return non-nil when DATE is on or after REFERENCE."
  (>= (calendar-absolute-from-gregorian date)
      (calendar-absolute-from-gregorian reference)))

(defun regnal-years--future-year-p (date)
  "Return non-nil when DATE is later than the current local year."
  (> (nth 2 date)
     (nth 2 (calendar-current-date))))

(defun regnal-years--entry-for-date (date entries)
  "Return the first entry in ENTRIES governing DATE.

ENTRIES must be ordered newest first."
  (cl-find-if
   (lambda (entry)
     (regnal-years--date-on-or-after-p date (car entry)))
   entries))

(defun regnal-years-number (date accession)
  "Return the regnal year of DATE relative to ACCESSION.

Both DATE and ACCESSION use Emacs calendar order.  The first
regnal year runs from accession through the day before its first
anniversary."
  (pcase-let ((`(,month ,day ,year) date)
              (`(,acc-month ,acc-day ,acc-year) accession))
    (+ (- year acc-year)
       (if (or (> month acc-month)
               (and (= month acc-month)
                    (>= day acc-day)))
           1
         0))))

(defun regnal-years-anniversary-p (date accession)
  "Return non-nil when DATE is the anniversary of ACCESSION."
  (and (= (nth 0 date) (nth 0 accession))
       (= (nth 1 date) (nth 1 accession))))

(defun regnal-years--format (date entries epoch)
  "Format DATE using ENTRIES, beginning no earlier than EPOCH.

Return nil for dates before EPOCH or beyond the current local
calendar year."
  (when (and (regnal-years--date-on-or-after-p date epoch)
             (not (regnal-years--future-year-p date)))
    (when-let* ((entry (regnal-years--entry-for-date date entries))
                (accession (car entry))
                (name (cdr entry)))
      (format "%d %s"
              (regnal-years-number date accession)
              name))))

(defun regnal-years-monarch (&optional date)
  "Return the British/Canadian regnal year for DATE.

DATE defaults to the current local civil date.  Return nil before
1801-01-01 or for dates beyond the current local calendar year."
  (regnal-years--format
   (or date (calendar-current-date))
   regnal-years-monarchs
   regnal-years-monarchical-epoch))

(defun regnal-years-pope (&optional date)
  "Return the modern papal regnal year for DATE.

DATE defaults to the current local civil date.  Return nil before
1929-02-11 or for dates beyond the current local calendar year."
  (regnal-years--format
   (or date (calendar-current-date))
   regnal-years-popes
   regnal-years-papal-epoch))

(defun regnal-years-monarch-anniversary-p (&optional date)
  "Return non-nil when DATE is the reigning monarch's accession day."
  (let* ((date (or date (calendar-current-date)))
         (entry (regnal-years--entry-for-date
                 date regnal-years-monarchs)))
    (and entry
         (regnal-years-anniversary-p date (car entry)))))

(defun regnal-years-pope-anniversary-p (&optional date)
  "Return non-nil when DATE is the reigning pope's election day."
  (let* ((date (or date (calendar-current-date)))
         (entry (regnal-years--entry-for-date
                 date regnal-years-popes)))
    (and entry
         (regnal-years-anniversary-p date (car entry)))))

(defun regnal-years-date-string (&optional date)
  "Return monarch and papal regnal years for DATE.

DATE defaults to the current local civil date.  Omit unavailable
parts; return nil when neither system applies."
  (let* ((date (or date (calendar-current-date)))
         (monarch (regnal-years-monarch date))
         (pope (regnal-years-pope date)))
    (cond
     ((and monarch pope)
      (format "%s · %s" monarch pope))
     (monarch monarch)
     (pope pope))))

;;;###autoload
(defun regnal-years-show-date (&optional date)
  "Display monarch and papal regnal years for DATE.

DATE defaults to the current local civil date."
  (interactive)
  (if-let* ((result (regnal-years-date-string date)))
      (message "%s" result)
    (message "No regnal date available")))

;;;###autoload
(defun regnal-years-show-date-at-point ()
  "Display regnal years for the Calendar date at point."
  (interactive)
  (unless (derived-mode-p 'calendar-mode)
    (user-error "This command must be used in a Calendar buffer"))
  (regnal-years-show-date (calendar-cursor-to-date t)))

;;;###autoload
(defun regnal-years-insert-date (&optional date)
  "Insert monarch and papal regnal years for DATE.

DATE defaults to the current local civil date."
  (interactive)
  (if-let* ((result (regnal-years-date-string date)))
      (insert result)
    (user-error "No regnal date available")))

(provide 'regnal-years)

;;; regnal-years.el ends here
