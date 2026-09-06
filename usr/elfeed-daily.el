;;; elfeed-daily.el --- Daily briefing view for Elfeed -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom search and show behaviour for the Elfeed daily briefing.

;;; Code:

(require 'elfeed)
(require 'elfeed-search)
(require 'elfeed-show)

;;; daily briefings machinery

(defvar cpj/elfeed-daily-filter
  "+unread +daily"
  "Search filter for the daily Elfeed briefing.")

(defun cpj/elfeed-daily-print-entry (entry)
  "Print ENTRY without tags."
  (pcase-let ((`(,date . ,date-width) (elfeed-search--column-date entry))
              (`(,title . ,title-width) (elfeed-search--column-title entry))
              (feed (elfeed-search--column-feed entry)))
    (insert date
            (propertize " " 'display `(space :align-to ,(1+ date-width)))
            title
            (if feed
                (propertize " " 'display
                            `(space :align-to
                                    ,(+ 2 date-width title-width)))
              "")
            (or feed ""))))

(defun cpj/elfeed-daily-title-max-width ()
  "Return the available title width for the daily Elfeed view."
  (- (window-body-width)
     (nth 1 elfeed-search-date-format)
     elfeed-search-trailing-width
     2))

(defun cpj/elfeed-daily-trailing-width ()
  "Return the width required for feed titles in the daily search results."
  (1+
   (apply #'max 0
          (mapcar
           (lambda (entry)
             (string-width
              (or (elfeed-search--column-feed entry) "")))
           elfeed-search-entries))))

(defun cpj/elfeed-daily-update-widths ()
  "Update column widths for the daily Elfeed view."
  (setq-local elfeed-search-trailing-width
              (cpj/elfeed-daily-trailing-width)
              elfeed-search-title-max-width
              (cpj/elfeed-daily-title-max-width)))

(defun cpj/elfeed-daily ()
  "Display the daily briefing feeds in Elfeed."
  (interactive)
  (elfeed-search cpj/elfeed-daily-filter)
  (setq-local elfeed-search-date-format '("%a %R" 9 :left)
              elfeed-search-print-entry-function
              #'cpj/elfeed-daily-print-entry
              mode-name "Elfeed Daily")
  (cpj/elfeed-daily-update-widths)
  (add-hook 'elfeed-search-update-hook
            #'cpj/elfeed-daily-update-widths nil t)
  (elfeed-search-update :force))

(defun cpj/elfeed-search-clear-filter ()
  "Restore the default Elfeed search view."
  (interactive)
  (dolist (variable '(elfeed-search-date-format
                      elfeed-search-print-entry-function
                      elfeed-search-title-max-width
                      elfeed-search-trailing-width))
    (kill-local-variable variable))
  (setq-local mode-name "elfeed-search")
  (elfeed-search-clear-filter)
  (elfeed-search-update :force))

(defun cpj/elfeed-show-refresh ()
  "Refresh an Elfeed entry according to daily briefing display policy."
  (setq-local elfeed-show-author
              (not (memq 'daily (elfeed-entry-tags elfeed-show-entry))))
  (elfeed-show-refresh--mail-style))

(provide 'elfeed-daily)

;;; elfeed-daily.el ends here
; LocalWords:  elfeed
