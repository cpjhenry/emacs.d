;;; canadian-parliament.el --- Canadian Parliaments and sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Henry
;;
;; Author: Paul Henry
;; Keywords: calendar, history, Canada
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.1
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide a compact historical reckoning of Canadian federal
;; Parliaments and their sessions.
;;
;; For a Gregorian date, the library identifies the numbered
;; Parliament and, where applicable, the numbered session then in
;; existence. Typical results are:
;;
;;     45th Parl. 1st Sess.
;;     45th Parl. No Session
;;     No Parliament
;;
;; A Parliament and a session are distinct constitutional periods.
;; A Parliament begins on the date fixed for the return of the writs
;; following a general election and ends on dissolution. A Parliament
;; may contain one or more sessions. A session begins when Parliament
;; is summoned and ends with prorogation or dissolution.
;;
;; Consequently, a date may fall within a Parliament but outside a
;; session, as after prorogation and before the opening of a subsequent
;; session. Between the dissolution of one Parliament and the
;; commencement of the next, no Parliament exists for the purposes of
;; this reckoning. These interstitial periods are represented
;; explicitly rather than attributed to the preceding or following
;; session.
;;
;; Dates in the historical tables are inclusive. The date of
;; prorogation or dissolution therefore belongs to the session or
;; Parliament thereby terminated. The following day does not.
;;
;; The concordance begins with the opening of the First Session of the
;; 28th Parliament on 12 September 1968. This is an intentionally
;; modern and arbitrary epoch, broadly contemporary with the epoch of
;; the Truncated Julian Date used elsewhere by `ind'. It is not
;; intended to imply any constitutional significance to the year 1968.
;; The tables may be extended backward without changing the reckoning
;; or its interface.
;;
;; Historical dates are taken principally from:
;;
;;   House of Commons, Canada, House of Commons Procedure and Practice,
;;   4th ed. (2025), Appendix 13, "Parliaments Since 1867 and Number
;;   of Sitting Days".
;;
;; Parliamentary terminology follows House of Commons Procedure and
;; Practice, Chapter 8, "The Parliamentary Cycle".
;;
;; The historical tables are deliberately maintained locally rather
;; than obtained dynamically. This makes the result reproducible for
;; historical dates and avoids making calendar display dependent upon
;; network access or the current organization of parliamentary
;; websites.
;;
;; Dates beyond the current known parliamentary state are not inferred.
;; In particular, an open current Parliament or session is not assumed
;; to continue beyond the current local civil date.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup canadian-parliament nil
  "Canadian federal Parliaments and sessions."
  :group 'calendar)

(defconst canadian-parliament-epoch '(9 12 1968)
  "Earliest date for Canadian Parliament display.

This is the opening of the First Session of the 28th Parliament.")

(defconst canadian-parliament-parliaments
  '(((5 19 2025) nil         45)
    ((10 11 2021) (3 23 2025) 44)
    ((11 11 2019) (8 15 2021) 43)
    ((11 9 2015)  (9 11 2019) 42)
    ((5 23 2011)  (8 2 2015)  41)
    ((11 4 2008)  (3 26 2011) 40)
    ((2 13 2006)  (9 7 2008)  39)
    ((7 19 2004)  (11 29 2005) 38)
    ((12 18 2000) (5 23 2004) 37)
    ((6 23 1997)  (10 22 2000) 36)
    ((11 15 1993) (4 27 1997) 35)
    ((12 12 1988) (9 8 1993)  34)
    ((9 24 1984)  (10 1 1988) 33)
    ((3 10 1980)  (7 9 1984)  32)
    ((6 11 1979)  (12 14 1979) 31)
    ((7 31 1974)  (3 26 1979) 30)
    ((11 20 1972) (5 9 1974)  29)
    ((9 12 1968)  (9 1 1972)  28))
  "Canadian federal Parliaments within the supported period.

Each entry has the form:

  (START END PARLIAMENT)

START is the date fixed for return of the writs, except for the
28th Parliament, whose entry is truncated to
`canadian-parliament-epoch'. END is the inclusive date of
dissolution. A nil END denotes the current Parliament.")

(defconst canadian-parliament-sessions
  '(((5 26 2025) nil          45 1)

    ((11 22 2021) (1 6 2025) 44 1)

    ((9 23 2020)  (8 15 2021) 43 2)
    ((12 5 2019)  (8 18 2020) 43 1)

    ((12 3 2015)  (9 11 2019) 42 1)

    ((10 16 2013) (8 2 2015)  41 2)
    ((6 2 2011)   (9 13 2013) 41 1)

    ((3 3 2010)   (3 26 2011) 40 3)
    ((1 26 2009)  (12 30 2009) 40 2)
    ((11 18 2008) (12 4 2008) 40 1)

    ((10 16 2007) (9 7 2008)  39 2)
    ((4 3 2006)   (9 14 2007) 39 1)

    ((10 4 2004)  (11 29 2005) 38 1)

    ((2 2 2004)   (5 23 2004) 37 3)
    ((9 30 2002)  (11 12 2003) 37 2)
    ((1 29 2001)  (9 16 2002) 37 1)

    ((10 12 1999) (10 22 2000) 36 2)
    ((9 22 1997)  (9 18 1999) 36 1)

    ((2 27 1996)  (4 27 1997) 35 2)
    ((1 17 1994)  (2 2 1996)  35 1)

    ((5 13 1991)  (9 8 1993)  34 3)
    ((4 3 1989)   (5 12 1991) 34 2)
    ((12 12 1988) (2 28 1989) 34 1)

    ((9 30 1986)  (10 1 1988) 33 2)
    ((11 5 1984)  (8 28 1986) 33 1)

    ((12 7 1983)  (7 9 1984)  32 2)
    ((4 14 1980)  (11 30 1983) 32 1)

    ((10 9 1979)  (12 14 1979) 31 1)

    ((10 11 1978) (3 26 1979) 30 4)
    ((10 18 1977) (10 10 1978) 30 3)
    ((10 12 1976) (10 17 1977) 30 2)
    ((9 30 1974)  (10 12 1976) 30 1)

    ((2 27 1974)  (5 9 1974)  29 2)
    ((1 4 1973)   (2 26 1974) 29 1)

    ((2 17 1972)  (9 1 1972)  28 4)
    ((10 8 1970)  (2 16 1972) 28 3)
    ((10 23 1969) (10 7 1970) 28 2)
    ((9 12 1968)  (10 22 1969) 28 1))
  "Canadian federal parliamentary sessions within the supported period.

Each entry has the form:

  (START END PARLIAMENT SESSION)

START is the date of opening. END is the inclusive date of
prorogation or, when the session ends by dissolution, the date
of dissolution. A nil END denotes the current session.")

(defun canadian-parliament--absolute (date)
  "Return the absolute date corresponding to Gregorian DATE."
  (calendar-absolute-from-gregorian date))

(defun canadian-parliament--date-in-range-p (date start end)
  "Return non-nil when DATE falls within START and END, inclusive.

A nil END denotes an open range extending through the current
local civil date."
  (let ((absolute (canadian-parliament--absolute date)))
    (and (>= absolute
             (canadian-parliament--absolute start))
         (if end
             (<= absolute
                 (canadian-parliament--absolute end))
           (<= absolute
               (canadian-parliament--absolute
                (calendar-current-date)))))))

(defun canadian-parliament--entry-for-date (date entries)
  "Return the entry in ENTRIES containing DATE."
  (cl-find-if
   (lambda (entry)
     (canadian-parliament--date-in-range-p
      date (nth 0 entry) (nth 1 entry)))
   entries))

(defun canadian-parliament--ordinal (number)
  "Return NUMBER as an English ordinal string."
  (let ((n100 (% number 100)))
    (format "%d%s"
            number
            (cond
             ((memq n100 '(11 12 13)) "th")
             ((= (% number 10) 1) "st")
             ((= (% number 10) 2) "nd")
             ((= (% number 10) 3) "rd")
             (t "th")))))

(defun canadian-parliament-date (&optional date)
  "Return parliamentary information for DATE.

Return (PARLIAMENT SESSION) when DATE falls within a session,
(PARLIAMENT nil) when DATE falls within a Parliament but outside
a session, and the symbol `no-parliament' when DATE falls between
Parliaments. Return nil outside the supported period."
  (let ((date (or date (calendar-current-date))))
    (when (and
           (>= (canadian-parliament--absolute date)
               (canadian-parliament--absolute
                canadian-parliament-epoch))
           (<= (canadian-parliament--absolute date)
               (canadian-parliament--absolute
                (calendar-current-date))))
      (if-let* ((parliament
                 (canadian-parliament--entry-for-date
                  date canadian-parliament-parliaments)))
          (let ((session
                 (canadian-parliament--entry-for-date
                  date canadian-parliament-sessions)))
            (list (nth 2 parliament)
                  (and session (nth 3 session))))
        'no-parliament))))

(defun canadian-parliament-date-string (&optional date)
  "Return the Canadian Parliament and session containing DATE."
  (pcase (canadian-parliament-date date)
    ('nil nil)
    ('no-parliament "No Parliament")
    (`(,parliament nil)
     (format "%s Parl. No Session"
             (canadian-parliament--ordinal parliament)))
    (`(,parliament ,session)
     (format "%s Parl. %s Sess."
             (canadian-parliament--ordinal parliament)
             (canadian-parliament--ordinal session)))))

;;;###autoload
(defun canadian-parliament-show-date (&optional date)
  "Display the Canadian Parliament and session for DATE.

DATE defaults to the current local civil date."
  (interactive)
  (if-let* ((result (canadian-parliament-date-string date)))
      (message "%s" result)
    (message "No parliamentary date available")))

;;;###autoload
(defun canadian-parliament-show-date-at-point ()
  "Display the Canadian Parliament and session for the Calendar date at point."
  (interactive)
  (unless (derived-mode-p 'calendar-mode)
    (user-error "This command must be used in a Calendar buffer"))
  (canadian-parliament-show-date
   (calendar-cursor-to-date t)))

;;;###autoload
(defun canadian-parliament-insert-date (&optional date)
  "Insert the Canadian Parliament and session for DATE.

DATE defaults to the current local civil date."
  (interactive)
  (if-let* ((result (canadian-parliament-date-string date)))
      (insert result)
    (user-error "No parliamentary date available")))

(provide 'canadian-parliament)

;;; canadian-parliament.el ends here

; LocalWords:  canadian Parl Sess
