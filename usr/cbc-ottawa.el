;;; cbc-ottawa.el --- Display CBC Ottawa headlines -*- lexical-binding: t; -*-

;; Keywords: news, rss, convenience
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package retrieves the CBC Ottawa RSS feed and displays the
;; current headlines in a read-only Emacs buffer.
;;
;; Headlines are inserted as buttons and may be opened with RET or
;; a mouse click. TAB and S-TAB move between headlines. URLs prefer
;; `browse-url-secondary-browser-function' when configured, falling
;; back to `browse-url-browser-function'.
;;
;; RSS retrieval and UTF-8 decoding are handled by `cpj/rss-fetch-items'.
;; The response body is treated as raw bytes, decoded explicitly as UTF-8,
;; and then parsed as XML. This avoids mojibake in headlines containing
;; accented or Indigenous-language characters.
;;
;; The main entry point is `cbc'.

;;; Code:
(require 'browse-url)
(require 'button)
(require 'dom)
(require 'url)
(require 'xml)

(defconst cpj/cbc-ottawa-feed-url
  "https://www.cbc.ca/webfeed/rss/rss-canada-ottawa"
  "URL of the CBC Ottawa RSS feed.")

(defun cpj/rss-fetch-items (url)
  "Return RSS items from URL as (TITLE . LINK) pairs."
  (let ((buffer
         (let ((coding-system-for-read 'binary))
           (url-retrieve-synchronously
            url
            'silent
            'inhibit-cookies))))
    (unless buffer
      (user-error "Could not retrieve RSS feed: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((body
                  (buffer-substring-no-properties
                   url-http-end-of-headers
                   (point-max)))
                 (decoded
                  (decode-coding-string
                   (string-as-unibyte body)
                   'utf-8))
                 dom)
            (with-temp-buffer
              (set-buffer-multibyte t)
              (insert decoded)
              (setq dom
                    (if (fboundp 'libxml-parse-region)
                        (libxml-parse-region
                         (point-min)
                         (point-max))
                      (car
                       (xml-parse-region
                        (point-min)
                        (point-max))))))
            (mapcar
             (lambda (item)
               (cons
                (string-trim
                 (dom-text
                  (car (dom-by-tag item 'title))))
                (string-trim
                 (dom-text
                  (car (dom-by-tag item 'link))))))
             (dom-by-tag dom 'item))))
      (kill-buffer buffer))))

(defun cpj/cbc-clean-url (url)
  "Remove CBC RSS tracking parameters from URL."
  (replace-regexp-in-string
   "[?&]cmp=rss\\(?:&\\|\\'\\)"
   ""
   url))

(defun cpj/cbc-browse-url (url)
  "Open URL using the secondary browser when configured."
  (let ((browse-url-browser-function
         (or browse-url-secondary-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defun cbc ()
  "Display today's headlines from CBC Ottawa."
  (interactive)
  (let ((buffer (get-buffer-create "*CBC*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
	(insert
	 (propertize "Today's headlines from CBC Ottawa"
		     'face '(:inherit bold :height 1.1))
	 "\n"
	 (propertize (format-time-string "%a %b %e %T %Z %Y")
		     'face 'shadow)
	 "\n")
        (dolist (item (cpj/rss-fetch-items
                       cpj/cbc-ottawa-feed-url))
          (insert "- ")
          (insert-text-button
           (car item)
           'action
           (lambda (button)
             (cpj/cbc-browse-url
              (button-get button 'cbc-url)))
           'cbc-url
           (cpj/cbc-clean-url (cdr item))
           'follow-link t
           'help-echo
           (cpj/cbc-clean-url (cdr item)))
          (insert "\n"))
        (special-mode)
        (local-set-key (kbd "TAB") #'forward-button)
        (local-set-key (kbd "<backtab>") #'backward-button)
        (local-set-key (kbd "S-TAB") #'backward-button)
        (when (bound-and-true-p jinx-mode)
          (jinx-mode -1))
        (goto-char (point-min))
        (forward-button 1)))
    (switch-to-buffer buffer)))

(provide 'cbc-ottawa)

;;; cbc-ottawa.el ends here

; LocalWords:  cmp backtab cbc ottawa
