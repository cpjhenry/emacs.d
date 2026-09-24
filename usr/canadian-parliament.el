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

;; `canadian-parliament-check-source' is an interactive maintenance
;; aid which retrieves Appendix 13 from the House of Commons and
;; compares its entries for the current and immediately preceding
;; Parliaments with the locally maintained historical tables.  It
;; does not modify those tables; updates remain a manual operation.

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

;;; Source checking

(defconst canadian-parliament--source-url
  "https://www.ourcommons.ca/procedure/procedure-and-practice-4/app13-e.html"
  "House of Commons source for Canadian Parliament and session dates.")

(defun canadian-parliament--source-strip-tags (string)
  "Return normalized text from HTML fragment STRING."
  (setq string (replace-regexp-in-string "<[^>]*>" " " string))
  (string-trim
   (replace-regexp-in-string
    "[[:space:]\n\r]+" " " string)))

(defun canadian-parliament--source-cells (row)
  "Return textual table cells from Appendix 13 HTML ROW."
  (with-temp-buffer
    (insert row)
    (goto-char (point-min))
    (let (cells)
      (while (re-search-forward "<td\\b" nil t)
        (let ((tag-start (match-beginning 0)))
          (unless (search-forward ">" nil t)
            (error "Malformed <td> in Appendix 13"))
          (let ((tag
                 (buffer-substring-no-properties
                  tag-start (point))))
            (if (string-match-p "/[[:space:]]*>\\'" tag)
                (push "" cells)
              (let ((content-start (point)))
                (unless (search-forward "</td>" nil t)
                  (error "Unclosed <td> in Appendix 13"))
                (push
                 (canadian-parliament--source-strip-tags
                  (buffer-substring-no-properties
                   content-start
                   (- (point) 5)))
                 cells))))))
      (nreverse cells))))

(defun canadian-parliament--source-date (string)
  "Convert Appendix 13 date STRING to an Emacs Gregorian date.

Return nil when STRING contains no date."
  (when (and string
             (string-match
              "\\b\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9]\\{4\\}\\)\\b"
              string))
    (list (string-to-number (match-string 2 string))
          (string-to-number (match-string 1 string))
          (string-to-number (match-string 3 string)))))

(defun canadian-parliament--source-dates (string)
  "Return all Appendix 13 dates contained in STRING."
  (let ((pos 0)
        dates)
    (while
        (string-match
         "\\b\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9]\\{4\\}\\)\\b"
         string pos)
      (push
       (list (string-to-number (match-string 2 string))
             (string-to-number (match-string 1 string))
             (string-to-number (match-string 3 string)))
       dates)
      (setq pos (match-end 0)))
    (nreverse dates)))

(defun canadian-parliament--source-parliament-number (string)
  "Return the Parliament number found in STRING."
  (when (string-match
         "\\b\\([0-9]+\\)\\(?:st\\|nd\\|rd\\|th\\) Parliament\\b"
         string)
    (string-to-number (match-string 1 string))))

(defun canadian-parliament--source-session-count (string)
  "Return the number of sessions represented in STRING."
  (let ((pos 0)
        (count 0))
    (while
        (string-match
         "\\b[0-9]+\\(?:st\\|nd\\|rd\\|th\\) Session\\b"
         string pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

(defun canadian-parliament--source-row-data (row)
  "Parse one Appendix 13 Parliament ROW.

Return a plist describing the Parliament and its sessions.
Signal an error if ROW does not have the expected structure."
  (let ((cells (canadian-parliament--source-cells row)))
    (unless (= (length cells) 9)
      (error "Unexpected Appendix 13 row structure: %d cells"
             (length cells)))

    (let* ((heading (nth 0 cells))
           (number
            (canadian-parliament--source-parliament-number heading))
           (session-count
            (canadian-parliament--source-session-count heading))
           (election
            (canadian-parliament--source-date (nth 1 cells)))
           (writs
            (canadian-parliament--source-date (nth 2 cells)))
           (openings
            (canadian-parliament--source-dates (nth 3 cells)))
           (last-sittings
            (canadian-parliament--source-dates (nth 4 cells)))
           (prorogations
            (canadian-parliament--source-dates (nth 5 cells)))
           (dissolution
            (canadian-parliament--source-date (nth 6 cells))))

      (unless (and number writs (> session-count 0))
        (error "Incomplete Appendix 13 parliamentary row"))

      (unless (= session-count (length openings))
        (error
         "Appendix 13 session/opening mismatch for Parliament %d"
         number))

      (list :parliament number
            :session-count session-count
            :election election
            :writs writs
            :openings openings
            :last-sittings last-sittings
            :prorogations prorogations
            :dissolution dissolution))))

(defun canadian-parliament--source-session-end
    (opening next-opening prorogations dissolution)
  "Return the end of the session beginning at OPENING.

NEXT-OPENING is the opening of the following session, or nil.
PROROGATIONS contains the Parliament's prorogation dates.
DISSOLUTION is the Parliament's dissolution date."
  (or
   (cl-find-if
    (lambda (date)
      (and
       (> (calendar-absolute-from-gregorian date)
          (calendar-absolute-from-gregorian opening))
       (or
        (null next-opening)
        (< (calendar-absolute-from-gregorian date)
           (calendar-absolute-from-gregorian next-opening)))))
    prorogations)
   (and (null next-opening)
        dissolution)))

(defun canadian-parliament--source-parliament-entry (source)
  "Return the Parliament table entry represented by SOURCE."
  (list
   (plist-get source :writs)
   (plist-get source :dissolution)
   (plist-get source :parliament)))

(defun canadian-parliament--source-session-entries (source)
  "Return session table entries represented by SOURCE.

Entries are returned oldest first."
  (let* ((number (plist-get source :parliament))
         (openings (plist-get source :openings))
         (prorogations (plist-get source :prorogations))
         (dissolution (plist-get source :dissolution))
         entries)
    (dotimes (i (length openings))
      (let* ((opening (nth i openings))
             (next-opening (nth (1+ i) openings))
             (end
              (canadian-parliament--source-session-end
               opening next-opening prorogations dissolution)))
        (push
         (list opening end number (1+ i))
         entries)))
    (nreverse entries)))

(defun canadian-parliament--local-parliament (number)
  "Return the local Parliament entry numbered NUMBER."
  (cl-find number canadian-parliament-parliaments
           :key (lambda (entry) (nth 2 entry))))

(defun canadian-parliament--local-sessions (number)
  "Return local sessions for Parliament NUMBER, oldest first.

`canadian-parliament-sessions' is stored newest first."
  (reverse
   (cl-remove-if-not
    (lambda (entry)
      (= (nth 2 entry) number))
    canadian-parliament-sessions)))

(defun canadian-parliament--check-entry (source local)
  "Return comparison text for SOURCE and LOCAL table entries."
  (cond
   ((equal source local) "OK")
   ((null local) "MISSING")
   (t "DIFF")))

(defun canadian-parliament--check-one (source)
  "Insert comparison results for one SOURCE Parliament."
  (let* ((number (plist-get source :parliament))
         (source-parliament
          (canadian-parliament--source-parliament-entry source))
         (source-sessions
          (canadian-parliament--source-session-entries source))
         (local-parliament
          (canadian-parliament--local-parliament number))
         (local-sessions
          (canadian-parliament--local-sessions number)))

    (insert
     (format "%s Parliament\n"
             (canadian-parliament--ordinal number)))

    (insert
     (format "  Parliament  House: %-29S Local: %-29S %s\n"
             source-parliament
             local-parliament
             (canadian-parliament--check-entry
              source-parliament local-parliament)))

    (dotimes (i (max (length source-sessions)
                     (length local-sessions)))
      (let ((source-session (nth i source-sessions))
            (local-session (nth i local-sessions)))
        (insert
         (format "  Session %-2d  House: %-29S Local: %-29S %s\n"
                 (1+ i)
                 source-session
                 local-session
                 (canadian-parliament--check-entry
                  source-session local-session)))))

    (insert "\n")))

(defun canadian-parliament--source-row-html (number)
  "Return the Appendix 13 HTML row for Parliament NUMBER."
  (goto-char (point-min))
  (let ((heading
         (format "%s Parliament"
                 (canadian-parliament--ordinal number))))
    (unless (search-forward heading nil t)
      (error "%s not found in Appendix 13" heading))

    (let ((beg
           (save-excursion
             (unless (search-backward "<tr" nil t)
               (error "Opening <tr> not found for %s" heading))
             (point)))
          (end
           (save-excursion
             (unless (search-forward "</tr>" nil t)
               (error "Closing </tr> not found for %s" heading))
             (point))))
      (buffer-substring-no-properties beg end))))

(defun canadian-parliament--source-parliaments (numbers)
  "Retrieve source data for Parliaments in NUMBERS."
  (require 'url)
  (let ((buffer
         (url-retrieve-synchronously
          canadian-parliament--source-url
          'silent 'inhibit-cookies 15)))
    (unless buffer
      (error "Unable to retrieve Appendix 13"))
    (unwind-protect
        (with-current-buffer buffer
          (mapcar
           (lambda (number)
             (canadian-parliament--source-row-data
              (canadian-parliament--source-row-html number)))
           numbers))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;###autoload
;;;###autoload
(defun canadian-parliament-check-source (&optional parliament)
  "Check local data against House of Commons Appendix 13.

Without a prefix argument, compare the current and immediately
preceding Parliaments with the locally maintained Parliament and
session tables.

With a prefix argument, prompt for a PARLIAMENT number and check
that Parliament only.

This command is a read-only maintenance aid.  It never modifies
`canadian-parliament-parliaments' or
`canadian-parliament-sessions'."
  (interactive
   (list
    (when current-prefix-arg
      (read-number "Parliament number: "))))
  (condition-case err
      (let* ((current
              (nth 2 (car canadian-parliament-parliaments)))
             (numbers
              (if parliament
                  (list parliament)
                (list current (1- current))))
             (sources
              (canadian-parliament--source-parliaments numbers)))
        (with-current-buffer
            (get-buffer-create "*Canadian Parliament Check*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert
             "Canadian Parliament source check\n"
             "House of Commons Procedure and Practice, Appendix 13\n\n")

            (dolist (source sources)
              (canadian-parliament--check-one source))

            (goto-char (point-min))
            (special-mode))

          (pop-to-buffer (current-buffer))))

    (error
     (user-error
      "Unable to check Appendix 13: %s"
      (error-message-string err)))))

(defun canadian-parliament-show-source-row (number)
  "Retrieve and display the Appendix 13 row for Parliament NUMBER."
  (interactive "nParliament number: ")
  (require 'url)
  (let ((buffer
         (url-retrieve-synchronously
          canadian-parliament--source-url
          'silent 'inhibit-cookies 15)))
    (unless buffer
      (user-error "Unable to retrieve Appendix 13"))
    (unwind-protect
        (with-current-buffer buffer
          (let ((row
                 (canadian-parliament--source-row-html number)))
            (with-current-buffer
                (get-buffer-create "*Canadian Parliament Source Row*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert row)
                (goto-char (point-min))
                (special-mode))
              (pop-to-buffer (current-buffer)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'canadian-parliament)

;;; canadian-parliament.el ends here

; LocalWords:  canadian Parl Sess
