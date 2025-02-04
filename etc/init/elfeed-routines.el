;; Elfeed routines
(defun elfeed-mark-all-as-read () (interactive)
	(mark-whole-buffer)
	(elfeed-search-untag-all-unread))
	(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

(defun elfeed-beginning-to-point-as-read () (interactive)
	(mark-from-beginning-of-buffer)
	(elfeed-search-untag-all-unread))
	(define-key elfeed-search-mode-map (kbd "B") 'elfeed-beginning-to-point-as-read)

;; https://noonker.github.io/posts/2020-04-22-elfeed/
(defun todo (text &optional body)
	(interactive "sTodo: ")
	(compose-mail-other-window user-mail-address text)
	(mail-text)
	(if body (insert body))
	(message-send-and-exit))

(defun elfeed-mail-todo (&optional use-generic-p)
	"Mail this to myself for later reading."
	(interactive "P")
	(let ((entries (elfeed-search-selected)))
		(cl-loop for entry in entries
			do (elfeed-untag entry 'unread)
			when (elfeed-entry-title entry)
			do (todo it (elfeed-entry-link entry)))
		(mapc #'elfeed-search-update-entry entries)
		;(unless (use-region-p) (forward-line))
		)
	(delete-other-windows))

;; https://pragmaticemacs.wordpress.com/2016/09/16/star-and-unstar-articles-in-elfeed/
(defalias 'elfeed-toggle-star
	(elfeed-expose #'elfeed-search-toggle-all 'star))
(eval-after-load 'elfeed-search
	'(define-key elfeed-search-mode-map (kbd "s") 'elfeed-toggle-star))

(defun hundred-times-better (entry)
	(let* ((original (elfeed-deref (elfeed-entry-content entry)))
	(replace (replace-regexp-in-string "#38;" "" original)))
	(setf (elfeed-entry-content entry) (elfeed-ref replace))))
;(add-hook 'elfeed-new-entry-hook #'hundred-times-better)

(advice-add 'elfeed-search-quit-window :override
	(lambda() "Save the database, kill elfeed buffers."
	(interactive)
	(elfeed-db-save)
	(kill-current-buffer)
	(let ((buffer elfeed-log-buffer-name))
	(and (get-buffer buffer) (kill-buffer buffer)))))

(defun elfeed-search-set-filter-nil () "Reset Elfeed search filter."
	(interactive)
	(elfeed-search-set-filter nil))

;; https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/
(org-link-set-parameters "elfeed"
  :follow #'elfeed-link-open
  :store #'elfeed-link-store-link
  :export #'elfeed-link-export-link)

(defun elfeed-link-export-link (link desc format _protocol)
  "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
  (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
    (if-let* ((entry
                (elfeed-db-get-entry
                  (cons (match-string 1 link)
                    (match-string 2 link))))
               (url
                 (elfeed-entry-link entry))
               (title
                 (elfeed-entry-title entry)))
      (pcase format
        ('html (format "<a href=\"%s\">%s</a>" url desc))
        ('md (format "[%s](%s)" desc url))
        ('latex (format "\\href{%s}{%s}" url desc))
        ('texinfo (format "@uref{%s,%s}" url desc))
        (_ (format "%s (%s)" desc url)))
      (format "%s (%s)" desc url))
    (format "%s (%s)" desc link)))

; LocalWords:  elfeed
