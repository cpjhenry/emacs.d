;;; calendar-data.el --- Generate Org agenda data from macOS Calendar -*- lexical-binding: t; -*-

;;; Commentary:

;; Generate a replaceable Org agenda file from selected macOS calendars.
;;
;; macOS Calendar remains authoritative.  The generated Org file is a
;; one-way, disposable projection and should not be edited manually.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defgroup calendar-data nil
  "Generate Org agenda data from macOS Calendar."
  :group 'calendar)

(defcustom calendar-data-file
  (expand-file-name "calendar-data.org" org-directory)
  "Org file generated from macOS Calendar."
  :type 'file
  :group 'calendar-data)

(defcustom calendar-data-calendar-names nil
  "Names of macOS calendars to include.

When nil, include all available calendars."
  :type '(repeat string)
  :group 'calendar-data)

(defcustom calendar-data-past-days 30
  "Number of past days to include in generated calendar data."
  :type 'natnum
  :group 'calendar-data)

(defcustom calendar-data-future-days 365
  "Number of future days to include in generated calendar data."
  :type 'natnum
  :group 'calendar-data)

(defcustom calendar-data-max-age 300
  "Maximum age in seconds before calendar data is refreshed."
  :type 'natnum
  :group 'calendar-data)

(defun calendar-data-stale-p ()
  "Return non-nil if calendar data is missing or too old."
  (or (not (file-exists-p calendar-data-file))
      (> (float-time
          (time-subtract
           (current-time)
           (file-attribute-modification-time
            (file-attributes calendar-data-file))))
         calendar-data-max-age)))

(defun calendar-data--load-maccalfw ()
  "Load maccalfw and its native module."
  (unless (require 'maccalfw nil t)
    (user-error "Cannot load maccalfw"))
  (maccalfw--load-module))

(defun calendar-data--event-property (property event)
  "Return the value of PROPERTY in EVENT."
  (caddr (assq property event)))

(defun calendar-data--event-parameters (property event)
  "Return the parameters associated with PROPERTY in EVENT."
  (cadr (assq property event)))

(defun calendar-data--event-parameter (property parameter event)
  "Return PARAMETER associated with PROPERTY in EVENT."
  (let ((parameters
         (calendar-data--event-parameters property event)))
    (when (eq (car parameters) parameter)
      (cadr parameters))))

(defun calendar-data--all-day-p (event)
  "Return non-nil when EVENT is an all-day event."
  (equal (calendar-data--event-parameter 'DTSTART 'VALUE event)
         "DATE"))

(defun calendar-data--parse-date (value)
  "Convert YYYYMMDD VALUE to a Gregorian date list."
  (unless (and (stringp value)
               (= (length value) 8)
               (string-match-p "\\`[[:digit:]]+\\'" value))
    (error "Invalid calendar date: %S" value))
  (list (string-to-number (substring value 4 6))
        (string-to-number (substring value 6 8))
        (string-to-number (substring value 0 4))))

(defun calendar-data--parse-datetime (value)
  "Convert YYYYMMDDTHHMMSS VALUE to a Gregorian date and time.

Return (DATE HOUR MINUTE), where DATE is in Calendar's
\(MONTH DAY YEAR) format."
  (unless (and (stringp value)
               (member (length value) '(15 16))
               (eq (aref value 8) ?T)
               (or (= (length value) 15)
                   (eq (aref value 15) ?Z))
               (string-match-p
                "\\`[[:digit:]]\\{8\\}T[[:digit:]]\\{6\\}Z?\\'"
                value))
    (error "Invalid calendar date-time: %S" value))
  (list
   (list (string-to-number (substring value 4 6))
         (string-to-number (substring value 6 8))
         (string-to-number (substring value 0 4)))
   (string-to-number (substring value 9 11))
   (string-to-number (substring value 11 13))))

(defun calendar-data--format-date (date)
  "Format Gregorian DATE for an Org timestamp."
  (format "%04d-%02d-%02d %s"
          (nth 2 date)
          (nth 0 date)
          (nth 1 date)
          (calendar-day-name date t)))

(defun calendar-data--format-time (hour minute)
  "Format HOUR and MINUTE as HH:MM."
  (format "%02d:%02d" hour minute))

(defun calendar-data--format-timestamp (event)
  "Return an Org timestamp for EVENT.

Timed values emitted by maccalfw are treated as already converted
to the Mac's local wall time.  All-day end dates are treated as
inclusive, matching maccalfw's observed output."
  (let ((start (calendar-data--event-property 'DTSTART event))
        (end (calendar-data--event-property 'DTEND event)))
    (if (calendar-data--all-day-p event)
        (let ((start-date (calendar-data--parse-date start))
              (end-date (calendar-data--parse-date end)))
          (if (equal start-date end-date)
              (format "<%s>"
                      (calendar-data--format-date start-date))
            (format "<%s>--<%s>"
                    (calendar-data--format-date start-date)
                    (calendar-data--format-date end-date))))
      (pcase-let* ((`(,start-date ,start-hour ,start-minute)
                    (calendar-data--parse-datetime start))
                   (`(,end-date ,end-hour ,end-minute)
                    (calendar-data--parse-datetime end)))
        (if (equal start-date end-date)
            (format "<%s %s-%s>"
                    (calendar-data--format-date start-date)
                    (calendar-data--format-time
                     start-hour start-minute)
                    (calendar-data--format-time
                     end-hour end-minute))
          (format "<%s %s>--<%s %s>"
                  (calendar-data--format-date start-date)
                  (calendar-data--format-time
                   start-hour start-minute)
                  (calendar-data--format-date end-date)
                  (calendar-data--format-time
                   end-hour end-minute)))))))

(defun calendar-data--clean-heading (string)
  "Return STRING suitable for use as an Org heading."
  (replace-regexp-in-string
   "[\n\r]+" " "
   (string-trim (or string "Untitled event"))))

(defun calendar-data--clean-property (string)
  "Return STRING suitable for use as an Org property value."
  (when string
    (replace-regexp-in-string
     "[\n\r]+" " "
     (string-trim string))))

(defun calendar-data--insert-property (name value)
  "Insert Org property NAME with VALUE when VALUE is non-nil."
  (when-let* ((value (calendar-data--clean-property value))
              ((not (string-empty-p value))))
    (insert ":" name ": " value "\n")))

(defun calendar-data--insert-event (record)
  "Insert wrapped calendar event RECORD as Org text."
  (let* ((calendar (plist-get record :calendar))
         (event (plist-get record :event))
         (title (calendar-data--event-property 'SUMMARY event))
         (description
          (calendar-data--event-property 'DESCRIPTION event))
         (timezone
          (calendar-data--event-parameter
           'DTSTART 'TZID event)))
    (insert "** "
            (calendar-data--clean-heading title)
            "\n")
    (insert (calendar-data--format-timestamp event) "\n")
    (insert ":PROPERTIES:\n")
    (calendar-data--insert-property
     "CALENDAR" (plist-get calendar :title))
    (calendar-data--insert-property
     "CALENDAR_ID" (plist-get calendar :id))
    (calendar-data--insert-property
     "EVENT_ID"
     (calendar-data--event-property 'UID event))
    (calendar-data--insert-property
     "LOCATION"
     (calendar-data--event-property 'LOCATION event))
    (calendar-data--insert-property
     "URL"
     (calendar-data--event-property 'URL event))
    (calendar-data--insert-property
     "SOURCE_TIME_ZONE" timezone)
    (insert ":END:\n")
    (when-let* ((description (and description
                                  (string-trim description)))
                ((not (string-empty-p description))))
      (insert "\n" description "\n"))
    (insert "\n")))

(defun calendar-data--event-sort-key (record)
  "Return chronological sort key for event RECORD."
  (or (calendar-data--event-property
       'DTSTART
       (plist-get record :event))
      ""))

(defun calendar-data--get-calendars ()
  "Return selected macOS calendars."
  (if calendar-data-calendar-names
      (maccalfw-get-calendars-by-name
       calendar-data-calendar-names)
    (maccalfw-get-calendars)))

(defun calendar-data--fetch-events (begin end)
  "Return wrapped calendar events between BEGIN and END."
  (cl-loop
   for calendar in (calendar-data--get-calendars)
   append
   (mapcar
    (lambda (event)
      (list :calendar calendar
            :event event))
    (maccalfw-fetch-events
     (plist-get calendar :id)
     (maccalfw--encode-date begin)
     (maccalfw--encode-date end t)))))

(defun calendar-data--date-range ()
  "Return the configured extraction range as (BEGIN END)."
  (let ((today
         (calendar-absolute-from-gregorian
          (calendar-current-date))))
    (list
     (calendar-gregorian-from-absolute
      (- today calendar-data-past-days))
     (calendar-gregorian-from-absolute
      (+ today calendar-data-future-days)))))

(defun calendar-data--render (records)
  "Return complete Org text for calendar RECORDS."
  (with-temp-buffer
    (insert "#+title: Calendar Data\n")
    (insert "#+startup: content\n")
    (insert "#+category: iCal\n")
    (insert "#+generated_at: "
            (format-time-string "%Y-%m-%dT%H:%M:%S%z")
            "\n\n")
    (insert "# This file is generated.  Do not edit it manually.\n\n")

    (dolist
        (calendar
         (calendar-data--get-calendars))
      (let* ((calendar-id (plist-get calendar :id))
             (events
              (cl-remove-if-not
               (lambda (record)
                 (equal
                  calendar-id
                  (plist-get
                   (plist-get record :calendar)
                   :id)))
               records)))
        (when events
          (insert "* "
                  (calendar-data--clean-heading
                   (plist-get calendar :title))
                  "\n\n")
          (dolist
              (record
               (sort events
                     (lambda (a b)
                       (string-lessp
                        (calendar-data--event-sort-key a)
                        (calendar-data--event-sort-key b)))))
            (calendar-data--insert-event record)))))

    (buffer-string)))

(defun calendar-data--write-atomically (contents)
  "Atomically replace `calendar-data-file' with CONTENTS."
  (let* ((target (expand-file-name calendar-data-file))
         (directory (file-name-directory target))
         (temporary nil))
    (make-directory directory t)
    (setq temporary
          (make-temp-file
           (expand-file-name ".calendar-data-" directory)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region contents nil temporary nil 'silent))
          (rename-file temporary target t)
          (setq temporary nil))
      (when (and temporary
                 (file-exists-p temporary))
        (delete-file temporary)))))

(defun calendar-data--revert-buffer ()
  "Revert the buffer visiting `calendar-data-file', when safe."
  (when-let* ((buffer
               (find-buffer-visiting
                (expand-file-name calendar-data-file))))
    (with-current-buffer buffer
      (if (buffer-modified-p)
          (message
           "Calendar data refreshed, but its modified buffer was not reverted")
        (revert-buffer t t)))))

;;;###autoload
(defun calendar-data-refresh ()
  "Regenerate `calendar-data-file' from macOS Calendar."
  (interactive)
  (calendar-data--load-maccalfw)
  (pcase-let* ((`(,begin ,end)
                (calendar-data--date-range))
               (records
                (calendar-data--fetch-events begin end))
               (contents
                (calendar-data--render records)))
    (calendar-data--write-atomically contents)
    (calendar-data--revert-buffer)
    (message "Wrote %d calendar events to %s"
             (length records)
             (abbreviate-file-name calendar-data-file))))

;;;###autoload
(defun calendar-data-refresh-if-stale ()
  "Refresh calendar data if its cache is missing or stale."
  (interactive)
  (when (calendar-data-stale-p)
    (calendar-data-refresh)))

(provide 'calendar-data)

;;; calendar-data.el ends here
