;;; wwv.el --- NOAA space weather summaries for Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Retrieves and summarizes selected NOAA Space Weather Prediction
;; Center (SWPC) text products, including the daily WWV geophysical
;; alert message and related solar-terrestrial indices.
;;
;; The intent is not to reproduce the original bulletin verbatim,
;; but to present a concise summary suitable for daily review,
;; status displays, or integration with other personal information
;; systems.
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

(defvar wwv-base-url "https://services.swpc.noaa.gov/text/"
  "NOAA SWPC text product base URL.")

(defun wwv--fetch (file)
  "Fetch NOAA SWPC text product FILE."
  (with-current-buffer
      (url-retrieve-synchronously (concat wwv-base-url file) t t 5)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (prog1
        (string-replace "\r" "" (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer))))

(defun wwv-summary ()
  "Return compact NOAA WWV geophysical alert summary."
  (let* ((text (wwv--fetch "wwv.txt"))
         (num "\\([0-9]+\\(?:\\.[0-9]+\\)?\\)")
         flux a k parts)
    (when (string-match
           (format "Solar flux %s and estimated planetary A-index %s" num num)
           text)
      (setq flux (match-string 1 text)
            a    (match-string 2 text)))
    (when (string-match
           (format "planetary K-index.*was %s" num)
           text)
      (setq k (match-string 1 text)))
    (setq parts
          (delq nil
                (list (when flux (format "Flux %s" flux))
                      (when a    (format "A-%s" a))
                      (when k    (format "K-%s" k)))))
    (when parts
      (concat (string-join parts ". ") "."))))

(defun wwv ()
  "Display compact NOAA WWV geophysical alert."
  (interactive)
  (message "%s" (wwv-summary)))

(defun wwv-alerts () "Geophysical alerts and space weather."
  (interactive)
  (eww "https://services.swpc.noaa.gov/text/"))

(provide 'wwv)

;;; wwv.el ends here

; LocalWords:  Center
