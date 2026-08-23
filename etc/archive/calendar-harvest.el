(defun my/calendar-harvest-year (year)
  "Return BSD `calendar` output for YEAR as a string."
  (let* ((days-in-year
          (string-to-number
           (format-time-string "%j" (encode-time 0 0 0 31 12 year))))
         (cmd (format "calendar -t0101 -W%d | sort -Mk2" (1- days-in-year))))
    (with-temp-buffer
      (let ((status (call-process-shell-command cmd nil t)))
        (unless (zerop status)
          (error "calendar command failed with status %s: %s"
                 status cmd))
        (buffer-string)))))

(defconst my/month-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defun my/calendar-parse-line (line year)
  "Parse one BSD calendar LINE into a plist."
  (when (string-match
         (rx string-start
             (* space)
             (group (repeat 1 2 digit))
             (+ space)
             (group (repeat 3 alpha))
             (optional (group "*"))
             (+ space)
             (group (+ anything))
             string-end)
         line)
    (let* ((day (string-to-number (match-string 1 line)))
           (mon-str (match-string 2 line))
           (movable (and (match-string 3 line) t))
           (label (match-string 4 line))
           (month (cdr (assoc mon-str my/month-alist))))
      (when month
        (list :year year
              :month month
              :day day
              :movable movable
              :label label)))))

(defun my/calendar-harvest-to-dataset (year)
  "Return parsed holiday dataset for YEAR."
  (let ((raw (my/calendar-harvest-year year))
        items)
    (dolist (line (split-string raw "\n" t))
      (let ((item (my/calendar-parse-line line year)))
        (when item
          (push item items))))
    (nreverse items)))

(defun my/calendar-dataset-to-holidays (dataset)
  "Convert DATASET into Emacs holiday entries."
  (mapcar
   (lambda (item)
     (list (list (plist-get item :month)
                 (plist-get item :day)
                 (plist-get item :year))
           (plist-get item :label)))
   dataset))

(defun my/show-calendar-harvest (year &optional n)
  "Pretty-print first N harvested calendar items for YEAR."
  (interactive "nYear: \nP")
  (pp-display-expression
   (seq-take (my/calendar-harvest-to-dataset year) (or n 20))
   "*calendar-harvest*"))
