;;; wwv.el --- NOAA space weather summaries for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Retrieves and summarizes selected NOAA Space Weather Prediction
;; Center (SWPC) text products, including the daily WWV geophysical
;; alert message and related solar-terrestrial indices.
;;
;; The summary is retrieved asynchronously and cached in memory.
;; `wwv-summary' merely returns the cached value, so callers such as
;; `daily-info' never wait for a network request.
;;
;; The cache is checked hourly while the user is active.  When Emacs
;; regains focus, such as after the computer wakes, a stale cache is
;; refreshed without waiting for the next timer interval.
;;
;; Failed network requests leave the last successful cached value
;; intact.
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

(defcustom wwv-refresh-interval (* 60 60)
  "Number of seconds between WWV cache checks."
  :type 'integer)

(defcustom wwv-cache-max-age (* 2 60 60)
  "Maximum age in seconds before the WWV cache needs refreshing."
  :type 'integer)

(defcustom wwv-active-idle-limit (* 15 60)
  "Maximum user idle time for an automatic timer refresh.

The focus hook does not use this limit, since regaining focus is
itself evidence that the user is present."
  :type 'integer)

(defvar wwv-summary-cache nil
  "Most recently cached WWV summary string.")

(defvar wwv-summary-cache-time nil
  "Time of the last successful WWV summary refresh.")

(defvar wwv-summary-timer nil
  "Timer used to check whether `wwv-summary-cache' needs refreshing.")

(defvar wwv-refresh-in-progress nil
  "Non-nil while an asynchronous WWV request is in progress.")

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
                        (when flux
                          (format "Flux %s" flux))
                        (when a
                          (format "A-%s" a))
                        (when k
                          (format "K-%s" k))))))
      (concat (string-join parts ". ") "."))))

(defun wwv--retrieve-callback (status)
  "Process an asynchronous WWV response described by STATUS."
  (unwind-protect
      (unless (plist-get status :error)
        (when url-http-end-of-headers
          (goto-char url-http-end-of-headers)
          (let* ((text
                  (string-replace
                   "\r" ""
                   (buffer-substring-no-properties
                    (point) (point-max))))
                 (summary (wwv--parse-summary text)))
            ;; Never replace a good cached value with an empty or
            ;; unexpectedly unparseable response.
            (when summary
              (setq wwv-summary-cache summary
                    wwv-summary-cache-time (current-time))))))
    (setq wwv-refresh-in-progress nil)
    (kill-buffer (current-buffer))))

(defun wwv-refresh-summary ()
  "Asynchronously refresh `wwv-summary-cache'.

Keep the previous cached value if the request cannot be started,
fails, or returns an unparseable response."
  (interactive)
  (unless wwv-refresh-in-progress
    (setq wwv-refresh-in-progress t)
    (condition-case nil
        (url-retrieve
         (concat wwv-base-url "wwv.txt")
         #'wwv--retrieve-callback
         nil
         t
         t)
      (error
       (setq wwv-refresh-in-progress nil)))))

(defun wwv-summary-stale-p ()
  "Return non-nil when the cached WWV summary needs refreshing."
  (or (null wwv-summary-cache)
      (null wwv-summary-cache-time)
      (> (float-time
          (time-subtract
           (current-time)
           wwv-summary-cache-time))
         wwv-cache-max-age)))

(defun wwv-user-active-p ()
  "Return non-nil when Emacs has seen recent user activity."
  (let ((idle-time (current-idle-time)))
    (or (null idle-time)
        (< (float-time idle-time)
           wwv-active-idle-limit))))

(defun wwv-refresh-if-stale (&rest _)
  "Refresh the WWV summary when its cached value is stale."
  (when (wwv-summary-stale-p)
    (wwv-refresh-summary)))

(defun wwv-timer-refresh ()
  "Refresh a stale WWV summary when the user is active."
  (when (and (wwv-summary-stale-p)
             (wwv-user-active-p))
    (wwv-refresh-summary)))

(defun wwv-summary ()
  "Return the cached compact NOAA WWV summary, or nil."
  wwv-summary-cache)

(defun wwv-start-summary-timer ()
  "Start the hourly WWV cache-check timer.

Cancel any existing timer, check the cache immediately, and then
check every `wwv-refresh-interval' seconds while Emacs is running."
  (when (timerp wwv-summary-timer)
    (cancel-timer wwv-summary-timer))
  (setq wwv-summary-timer
        (run-at-time
         0
         wwv-refresh-interval
         #'wwv-timer-refresh)))

(defun wwv-stop-summary-timer ()
  "Stop the WWV summary timer."
  (interactive)
  (when (timerp wwv-summary-timer)
    (cancel-timer wwv-summary-timer))
  (setq wwv-summary-timer nil))

(defun wwv ()
  "Display the cached NOAA WWV geophysical alert summary."
  (interactive)
  (wwv-refresh-if-stale)
  (if-let* ((summary (wwv-summary)))
      (message "%s" summary)
    (message "WWV summary is not yet available; refreshing")))

(defun wwv-alerts ()
  "Display NOAA geophysical alerts and space weather."
  (interactive)
  (eww wwv-base-url)
  (hl-line-mode))

;; Re-evaluating this file cancels and replaces the existing timer.
;; `add-hook' will not add the same named function more than once.
(add-hook 'focus-in-hook #'wwv-refresh-if-stale)
(wwv-start-summary-timer)

(provide 'wwv)

;;; wwv.el ends here

; LocalWords: Center
