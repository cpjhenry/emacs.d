;;; weather-alert.el --- Environment Canada weather -*- lexical-binding: t; -*-

;;; Commentary:
;; Retrieve weather forecasts and alerts from Environment Canada.
;;
;; Intended to replace the external `alert' shell script incrementally.
;;
;; Public commands:
;;
;;   `wx'       Local conditions and daily forecast.
;;   `wx-alert' Full weather report.  [To be implemented.]
;;
;; The implementation uses Emacs' URL and XML/HTML facilities rather
;; than external programs such as curl, xml2, sed, grep, and fold.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'xml)

(defgroup weather-alert nil
  "Environment Canada weather."
  :group 'applications)

(defcustom weather-alert-city-code "45.403_-75.687"
  "Environment Canada city code for the local forecast."
  :type 'string)

(defcustom weather-alert-region-code "onrm104"
  "Environment Canada region code for local weather alerts."
  :type 'string)

(defcustom weather-alert-city "Ottawa"
  "City used for public weather bulletins."
  :type 'string)

(defcustom weather-alert-waterway "Eastern Lake Ontario"
  "Waterway used for marine bulletins."
  :type 'string)

(defconst weather-alert--daily-url
  "https://weather.gc.ca/rss/weather/%s_e.xml")

(defconst weather-alert--summary-url
  "https://weather.gc.ca/rss/battleboard/%s_e.xml")

(defconst weather-alert--full-url
  "https://weather.gc.ca/warnings/report_e.html?%s=")

(defconst weather-alert--short-url
  "https://weather.gc.ca/forecast/public_bulletins_e.html?Bulletin=fpcn11.cwto")

(defconst weather-alert--extended-url
  "https://weather.gc.ca/forecast/public_bulletins_e.html?Bulletin=fpcn51.cwto")

(defconst weather-alert--maritime-url
  "https://www.weather.gc.ca/marine/marine_bulletins_e.html?Bulletin=fqcn13.cwto")

(define-derived-mode weather-alert-mode special-mode "Weather"
  "Major mode for Environment Canada weather reports."
  (visual-line-mode 1)
  (form-feed-st-mode 1))

;;; Retrieval

(defun weather-alert--retrieve (url)
  "Retrieve URL and return its decoded response body."
  (let ((buffer (url-retrieve-synchronously url t t 15)))
    (unless buffer
      (user-error "Unable to retrieve %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (or url-http-end-of-headers (point-min)))
          (decode-coding-string
           (buffer-substring-no-properties (point) (point-max))
           'utf-8))
      (kill-buffer buffer))))

(defun weather-alert--xml (url)
  "Retrieve URL and return its parsed XML DOM."
  (with-temp-buffer
    (insert (weather-alert--retrieve url))
    (goto-char (point-min))
    (car (xml-parse-region (point-min) (point-max)))))

(defun weather-alert--html (url)
  "Retrieve URL and return its parsed HTML DOM."
  (with-temp-buffer
    (insert (weather-alert--retrieve url))
    (goto-char (point-min))
    (libxml-parse-html-region (point-min) (point-max))))


;;; DOM utilities

(defun weather-alert--nodes (tree tag)
  "Return all TAG nodes in TREE."
  (let (nodes)
    (cl-labels
        ((walk (node)
           (when (consp node)
             (when (eq (car node) tag)
               (push node nodes))
             (mapc #'walk (cddr node)))))
      (walk tree))
    (nreverse nodes)))

(defun weather-alert--attr (node attr)
  "Return ATTR from NODE."
  (cdr (assq attr (cadr node))))

(defun weather-alert--text (node)
  "Return the textual contents of NODE."
  (cond
   ((stringp node)
    node)
   ((consp node)
    (mapconcat #'weather-alert--text (cddr node) ""))
   (t
    "")))

(defun weather-alert--child-text (node tag)
  "Return the text of the first TAG below NODE."
  (when-let* ((child (car (weather-alert--nodes node tag))))
    (string-trim (weather-alert--text child))))


;;; Daily forecast

(defun weather-alert--current-entry-p (entry)
  "Return non-nil when ENTRY contains current conditions."
  (when-let* ((title (weather-alert--child-text entry 'title)))
    (string-match-p "Current Conditions" title)))

(defun weather-alert--forecast-entry-p (entry)
  "Return non-nil when ENTRY contains a weather forecast."
  (seq-some
   (lambda (category)
     (equal (weather-alert--attr category 'term)
            "Weather Forecasts"))
   (weather-alert--nodes entry 'category)))

(defun weather-alert--daily ()
  "Return the current conditions and daily forecast."
  (let* ((url (format weather-alert--daily-url
                      weather-alert-city-code))
         (dom (weather-alert--xml url))
         (entries (weather-alert--nodes dom 'entry))
         (current
          (seq-find #'weather-alert--current-entry-p entries))
         (forecast
          (seq-find #'weather-alert--forecast-entry-p entries))
         (conditions
          (when current
            (weather-alert--child-text current 'title)))
         (summary
          (when forecast
            (weather-alert--child-text forecast 'summary))))

    (unless summary
      (user-error "No daily forecast found"))

    ;; Strip the entry label from current conditions.
    (when conditions
      (setq conditions
            (replace-regexp-in-string
             "\\`Current Conditions:? *"
             ""
             conditions))
      (unless (string-suffix-p "." conditions)
        (setq conditions (concat conditions "."))))

    ;; Strip HTML and issue time from the forecast.
    (setq summary
          (replace-regexp-in-string
           "<[^>]+>"
           ""
           summary))

    (setq summary
          (replace-regexp-in-string
           " Forecast issued.*\\'"
           ""
           summary))

    ;; Return one whitespace-normalized line.
    (string-join
     (split-string
      (string-join
       (delq nil (list conditions summary))
       " ")
      "[[:space:]\n\r]+"
      t)
     " ")))


;;; Public bulletins

(defun weather-alert--bulletin-text (url)
  "Return the bulletin body from URL as plain text."
  (let* ((dom (weather-alert--html url))
         (pre (car (weather-alert--nodes dom 'pre))))
    (unless pre
      (user-error "No bulletin text found"))
    (weather-alert--text pre)))

(defun weather-alert--extract-block (text regexp)
  "Return the first paragraph in TEXT whose first line matches REGEXP."
  (let ((lines (split-string text "\n"))
        found
        block)
    (while (and lines (not found))
      (if (string-match-p regexp (car lines))
          (setq found t)
        (setq lines (cdr lines))))

    (while (and lines
                (or (null block)
                    (not
                     (string-empty-p
                      (string-trim (car lines))))))
      (push (string-trim-right (car lines)) block)
      (setq lines (cdr lines)))

    (when block
      (string-join (nreverse block) "\n"))))

(defun weather-alert--short ()
  "Return the short-term Environment Canada forecast."
  (let* ((text
          (weather-alert--bulletin-text
           weather-alert--short-url))
         (header
          (weather-alert--extract-block
           text
           "^F.CN"))
         (forecast
          (weather-alert--extract-block
           text
           (regexp-quote weather-alert-city))))

    (when header
      (let* ((lines (split-string header "\n" t))
             (code (car lines))
             (prose (string-join (cdr lines) " ")))
	(setq prose
              (replace-regexp-in-string
               "[ \t]+" " " prose))
	(setq prose
              (replace-regexp-in-string
               "The next scheduled forecast.*\\'" "" prose))
	(setq header
              (string-join
               (delq nil
                     (list (string-trim code)
			   (string-trim prose)))
               "\n"))))

    (unless forecast
      (user-error
       "No short-term forecast found for %s"
       weather-alert-city))

    ;; Join source-wrapped continuation lines.
    (setq forecast
          (replace-regexp-in-string
           "\n[ \t]+"
           " "
           forecast))

    ;; Start each forecast period on a new line.
    (setq forecast
	  (replace-regexp-in-string
	   "\n*\\(Tonight\\|Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)"
	   "\n\\1"
	   forecast))

    (string-join
     (delq nil
	   (list (and header (string-trim header))
		 (and forecast (string-trim forecast))))
     "\n\n")))

(defun weather-alert--maritime ()
  "Return the Environment Canada marine forecast."
  (let* ((text
          (weather-alert--bulletin-text
           weather-alert--maritime-url))
         (header
          (weather-alert--extract-block
           text
           "^FQCN"))
         (forecast
          (weather-alert--extract-block
           text
           (regexp-quote weather-alert-waterway))))

    (when header
      (let* ((lines (split-string header "\n" t))
             (code (car lines))
             (prose (string-join (cdr lines) " ")))
        (setq prose
              (replace-regexp-in-string
               "[ \t]+" " " prose))
	(setq prose
	      (replace-regexp-in-string
	       "The next scheduled forecast.*\\'"
	       ""
	       prose))
        (setq header
              (string-join
               (delq nil
                     (list (string-trim code)
                           (string-trim prose)))
               "\n"))))

    (unless forecast
      (user-error
       "No marine forecast found for %s"
       weather-alert-waterway))

    ;; Join source-wrapped continuation lines.
    (setq forecast
          (replace-regexp-in-string
           "\n[ \t]+"
           " "
           forecast))

    (string-join
     (delq nil
           (list (and header (string-trim header))
                 (and forecast (string-trim forecast))))
     "\n\n")))

(defun weather-alert--by-id (tree id)
  "Return the first node in TREE whose id attribute is ID."
  (let (found)
    (cl-labels
        ((walk (node)
           (when (and (consp node) (not found))
             (when (equal (weather-alert--attr node 'id) id)
               (setq found node))
             (mapc #'walk (cddr node)))))
      (walk tree))
    found))

(defun weather-alert--alert-line-p (line)
  "Return non-nil when LINE should be kept in a full alert."
  (not
   (seq-some
    (lambda (regexp)
      (string-match-p regexp line))
    '("\\`Alerts for:"
      "\\`City of Ottawa\\'"
      "\\`Weather Alerts for:"
      "\\`In effect for:\\'"
      "\\`.*\$begin:math:text$\?\:Advisory\\\\\|Statement\\\\\|Watch\\\\\|Warning\\$end:math:text$.*in effect for:"
      "\\`.*\\(?:Advisory\\|Statement\\|Watch\\|Warning\\).*ended for:"
      "\\`Please continue to monitor"
      "\\`For more information:"
      "\\`https?://"
      "\\`Regional ATOM"
      "\\`ontario\\.ca"
      "\\`twitter\\.com"
      "\\`For road conditions"
      "tweet reports"
      "#ONStorm"
      "\\`ONstorm@ec\\.gc\\.ca"
      "\\`Ottawa North -"
      "\\`getprepared\\.gc\\.ca\\.?\\'"
      "grey icon"
      "\\`Yellow Warnings\\'"
      "yellow icon"
      "\\`Orange Warnings\\'"
      "orange icon"
      "\\`Red Warnings\\'"
      "red icon"
      "\\`For more information about the alerting program"
      "\\`Colour-coded Weather Alerts"))))

(defun weather-alert--clean-alert (text)
  "Clean full Environment Canada alert TEXT."
  (let* ((lines
          (mapcar
           (lambda (line)
             (string-trim
              (replace-regexp-in-string "[ \t]+" " " line)))
           (split-string text "\n")))
         (lines
          (seq-filter
           (lambda (line)
             (and (not (string-empty-p line))
                  (weather-alert--alert-line-p line)))
           lines))
         (text (string-join lines "\n")))

    (setq text
          (replace-regexp-in-string
           "Orléans" "Orleans" text))

    (setq text
          (replace-regexp-in-string
           "emergency plans and kits go to"
           "emergency plans and kits go to getprepared.gc.ca."
           text))

    (setq text
          (replace-regexp-in-string
           "\n\\{3,\\}" "\n\n" text))

    (setq text (string-trim text))

    (unless (or (string-empty-p text)
                (string-suffix-p "." text))
      (setq text (concat text ".")))

    text))

(defun weather-alert--full ()
  "Return the full Environment Canada weather alert, or nil."
  (let* ((url
          (format weather-alert--full-url
                  weather-alert-region-code))
         (dom (weather-alert--html url))
         (content
          (or (weather-alert--by-id dom "wb-cont")
              dom))
         (alert
          (weather-alert--clean-alert
           (weather-alert--text content))))
    (unless (string-empty-p alert)
      (concat
       alert
       "\n\nNote:\n"
       "- Watches are issued when there is the potential for severe weather.\n"
       "- Warnings are issued when severe weather is occurring or imminent."))))


;;; Commands

(defun wx ()
  "Display the local weather."
  (interactive)
  (message "%s" (weather-alert--daily)))

(defun wx-alert (&rest _ignore)
  "Display weather forecasts and alerts from Environment Canada."
  (interactive)
  (let ((buffer (get-buffer-create "*WX*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (short (weather-alert--short))
            (alert (weather-alert--full))
            (maritime (weather-alert--maritime)))
        (erase-buffer)

        (insert short)

        (when alert
          (insert "\n\n" alert))

        (insert "\n\n\f\n" maritime "\n")

        (goto-char (point-min))
	(weather-alert-mode)
        (text-scale-increase 1)))

    (switch-to-buffer buffer)))

(provide 'weather-alert)

;;; weather-alert.el ends here
