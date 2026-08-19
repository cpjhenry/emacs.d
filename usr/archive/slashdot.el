;;; slashdot.el --- Display current Slashdot headlines -*- lexical-binding: t; -*-

;; Keywords: news, rss, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package retrieves the Slashdot main RSS feed and displays the current
;; headlines in a read-only Emacs buffer.
;;
;; Headlines are inserted as buttons and may be opened with RET or a mouse
;; click.  TAB and S-TAB move between headlines.  URLs prefer
;; `browse-url-secondary-browser-function' when configured, falling back to
;; `browse-url-browser-function'.
;;
;; RSS retrieval, UTF-8 decoding, and XML parsing are handled by
;; `cpj/rss-fetch-items'.
;;
;; The main entry point is `/'.

;;; Code:
(require 'xml)
(require 'browse-url)
(require 'dom)
(require 'url)

(defconst cpj/slashdot-feed-url
  "https://rss.slashdot.org/Slashdot/slashdotMain"
  "URL of the Slashdot main RSS feed.")

(defun cpj/slashdot-browse-url (url)
  "Open URL using the secondary browser when configured."
  (let ((browse-url-browser-function
         (or browse-url-secondary-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defun cpj/slashdot-clean-url (url)
  "Remove tracking and challenge parameters from a Slashdot URL."
  (replace-regexp-in-string "[?#].*\\'" "" url))

(defun cpj/slashdot--items ()
  "Return the current Slashdot items as (TITLE . URL) pairs."
  (let ((buffer (url-retrieve-synchronously
                 cpj/slashdot-feed-url
                 'silent
                 'inhibit-cookies)))
    (unless buffer
      (user-error "Could not retrieve the Slashdot feed"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
	  (let ((dom
		 (if (fboundp 'libxml-parse-region)
		     (libxml-parse-region (point) (point-max))
		   (car (xml-parse-region (point) (point-max))))))
            (mapcar
             (lambda (item)
               (cons
                (string-trim
                 (dom-text (car (dom-by-tag item 'title))))
                (cpj/slashdot-clean-url
		 (string-trim
		  (dom-text (car (dom-by-tag item 'link)))))))
             (dom-by-tag dom 'item))))
      (kill-buffer buffer))))

(defun /. ()
  "Display the current Slashdot headlines."
  (interactive)
  (let ((buffer (get-buffer-create "*/.*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "/.\n"
                (format-time-string "%a %b %e %T %Z %Y")
                "\n")
        (dolist (item (cpj/slashdot--items))
          (insert "- ")
          (insert-text-button
           (car item)
	   'action
	   (lambda (button)
	     (cpj/slashdot-browse-url
	      (button-get button 'slashdot-url)))
	   'slashdot-url (cdr item)
           'follow-link t
           'help-echo (cdr item))
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)
	(local-set-key (kbd "TAB") #'forward-button)
	(local-set-key (kbd "<backtab>") #'backward-button)
	(local-set-key (kbd "S-TAB") #'backward-button)
	(goto-char (point-min))
	(forward-button 1)
        (when (bound-and-true-p jinx-mode)
          (jinx-mode -1))))
    (switch-to-buffer buffer)))

(provide 'slashdot)

;;; slashdot.el ends here

; LocalWords:  slashdot backtab
