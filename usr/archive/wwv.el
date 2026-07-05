;;; wwv.el --- NOAA space weather summaries for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Retrieves and summarizes selected NOAA Space Weather Prediction
;; Center (SWPC) text products, including the daily WWV geophysical
;; alert message and related solar-terrestrial indices.
;;
;; The summary is refreshed asynchronously once an hour and cached in
;; memory.  `wwv-summary' merely returns the cached value, so callers
;; such as `daily-info' never wait for a network request.
;;
;; Example output:
;;
;;   Flux 131. A-8. K-2.67.
;;
;; Data source:
;;
;;   https://services.swpc.noaa.gov/text/
;;
;; NOAA SWPC products are public-domain publications of the United
;; States Government.
;;
;; These bulletins descend from the geophysical information
;; traditionally transmitted by radio station WWV and remain useful
;; to amateur radio operators and others interested in space weather.

;;; Code:

(require 'url)
(require 'subr-x)

(defgroup wwv nil
  "NOAA space-weather summaries."
  :group 'applications)

(defcustom wwv-base-url
  "https://services.swpc.noaa.gov/text/"
  "NOAA SWPC text product base URL."
  :type 'string)

(defcustom wwv-refresh-interval 3600
  "Number of seconds between WWV summary refreshes."
  :type 'integer)

(defvar wwv-summary-cache nil
  "Most recently cached WWV summary string.")

(defvar wwv-summary-timer nil
  "Timer used to refresh `wwv-summary-cache'.")

(defun wwv--parse-summary (text)
  "Return a compact WWV summary parsed from TEXT."
  (let (flux a k)
    (when (string-match
           (rx "Solar flux "
               (group (+ digit) (? "." (+ digit)))
               " and estimated planetary A-index "
               (group (+ digit) (? "." (+ digit))))
           text)
      (setq flux (match-string 1 text)
            a (match-string 2 text)))

    (when (string-match
           (rx "planetary K-index"
               (*? anychar)
               "was "
               (group (+ digit) (? "." (+ digit))))
           text)
      (setq k (match-string 1 text)))

    (when-let* ((parts
                 (delq nil
                       (list
                        (when flux (format "Flux %s" flux))
                        (when a (format "A-%s" a))
                        (when k (format "K-%s" k))))))
      (concat (string-join parts ". ") "."))))

(defun wwv--retrieve-callback (status)
  "Process an asynchronous WWV response described by STATUS."
  (unwind-protect
      (unless (plist-get status :error)
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (let* ((text
                  (string-replace
                   "\r" ""
                   (buffer-substring-no-properties
                    (point) (point-max))))
                 (summary (wwv--parse-summary text)))
            ;; A failed or unexpectedly changed bulletin must not
            ;; destroy the last successfully cached summary.
            (when summary
              (setq wwv-summary-cache summary)))))
    (kill-buffer (current-buffer))))

(defun wwv-refresh-summary ()
  "Asynchronously refresh `wwv-summary-cache'."
  (interactive)
  (url-retrieve
   (concat wwv-base-url "wwv.txt")
   #'wwv--retrieve-callback
   nil
   t
   t))

(defun wwv-summary ()
  "Return the cached compact NOAA WWV summary, or nil."
  wwv-summary-cache)

(defun wwv-start-summary-timer ()
  "Start the hourly WWV summary refresh timer.

Refresh immediately, then repeat every
`wwv-refresh-interval' seconds."
  (when (timerp wwv-summary-timer)
    (cancel-timer wwv-summary-timer))
  (setq wwv-summary-timer
        (run-at-time
         0
         wwv-refresh-interval
         #'wwv-refresh-summary)))

(defun wwv-stop-summary-timer ()
  "Stop the WWV summary refresh timer."
  (interactive)
  (when (timerp wwv-summary-timer)
    (cancel-timer wwv-summary-timer))
  (setq wwv-summary-timer nil))

(defun wwv ()
  "Display the cached NOAA WWV geophysical alert summary."
  (interactive)
  (if-let* ((summary (wwv-summary)))
      (message "%s" summary)
    (wwv-refresh-summary)
    (message "WWV summary is not yet available; refreshing")))

(defun wwv-alerts ()
  "Display NOAA geophysical alerts and space weather."
  (interactive)
  (eww wwv-base-url)
  (hl-line-mode))

(wwv-start-summary-timer)

(provide 'wwv)

;;; wwv.el ends here

; LocalWords: Center
