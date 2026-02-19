;;; elfeed-routines.el --- Elfeed routines
;;; commentary:
;;; code:
(require 'elfeed)
(require 'elfeed-show)

(defun elfeed-search-beginning-to-point-as-read ()
  "Mark from the beginning to point."
  (interactive)
  (mark-from-beginning-of-buffer)
  (elfeed-search-untag-all-unread))

(defun elfeed-search-mark-all-as-read ()
  "Mark all as read."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max) nil t)
  (activate-mark)
  (elfeed-search-untag-all-unread))

(advice-add 'elfeed-search-quit-window :override
  (lambda() "Save the database, kill elfeed buffers."
  (interactive)
  (elfeed-db-save)
  (kill-current-buffer)
  (let ((buffer elfeed-log-buffer-name))
  (and (get-buffer buffer) (kill-buffer buffer)))))

(defun elfeed-show-visit-secondary-browser ()
  "Visit buffer in secondary browser."
  (interactive)
  (elfeed-show-visit '(4)))

(require 'shr)
(defun elfeed-show-toggle-images ()
  "Toggle images in `elfeed-show'."
  (interactive)
  (setq shr-inhibit-images (not shr-inhibit-images))
  (elfeed-show-refresh)
  (elfeed-show-tidy-buffer)
  (message "Inhibit images: %s" shr-inhibit-images))

;;; Clean-up routines
(defun elfeed-copy-edit ()
  "Fixes spurious typesetting errors in Elfeed buffers."
  (save-excursion
    ;; remove `*' nonsense...
    (goto-char (point-min))
    (while (re-search-forward "^[[:blank:]]*[[*]]*[[:blank:]]*$" nil t nil)
      (replace-match ""))

    ;; remove successive blank lines and trailing whitespace
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\n" nil t nil)
      (replace-match "\n")))
  (delete-trailing-whitespace))

(defun elfeed-show--header-faces ()
  "Return (KEYFACE VALUEFACE) used in the current Elfeed header."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Date:[ \t]*\\(.*\\)$" nil t)
      (list
       (get-text-property (match-beginning 0) 'face)
       (get-text-property (match-beginning 1) 'face)))))

(defun elfeed-show-rewrite-date ()
  (when (and (derived-mode-p 'elfeed-show-mode)
             (bound-and-true-p elfeed-show-entry))
    (pcase-let* ((`(,keyface ,valface)
                  (or (elfeed-show--header-faces)
                      '(message-header-name message-header-other)))
                 (time (seconds-to-time
                        (elfeed-entry-date elfeed-show-entry)))
                 (new  (format-time-string "%A, %-d %B %Y %H:%M" time)))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^Date:[ \t]*\\(.*\\)$" nil t)
          (replace-match
           (concat
            (propertize "Date:" 'face keyface)
            " "
            (propertize new 'face valface))
           t t))))))

(defun elfeed-show-tidy-buffer ()
  (when (and (derived-mode-p 'elfeed-show-mode)
             (bound-and-true-p elfeed-show-entry))
    (with-silent-modifications
      (let ((inhibit-read-only t))
        (elfeed-show-rewrite-date)
        (elfeed-copy-edit)
        (replace-garbage-chars nil nil t)
        (text-scale-set 1)
	)))
  (goto-char (point-min)))

(advice-add 'elfeed-search-show-entry :after (lambda (&rest _) (elfeed-show-tidy-buffer)))

;;; others
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

(defface elfeed-search-star-title-face
  '((t :foreground "#f77"))
  "Marks a starred Elfeed entry.")

(push '(star elfeed-search-star-title-face) elfeed-search-face-alist)

;; https://nullprogram.com/blog/2013/11/26/
(defun hundred-times-better (entry)
	(let* ((original (elfeed-deref (elfeed-entry-content entry)))
	(replace (replace-regexp-in-string "#38;" "" original)))
	(setf (elfeed-entry-content entry) (elfeed-ref replace))))
;(add-hook 'elfeed-new-entry-hook #'hundred-times-better)

;; Export links to org-mode files
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

;; https://old.reddit.com/r/emacs/comments/go5d0v/using_emacs_72_customizing_elfeed/
(defun elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed."
	(interactive "^P")
	(let ((scroll-error-top-bottom nil))
	(condition-case-unless-debug nil
	(scroll-up-command arg)
	(error (elfeed-show-next)))))

(defun elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed."
	(interactive "^P")
	(let ((scroll-error-top-bottom nil))
	(condition-case-unless-debug nil
	(scroll-down-command arg)
	(error (elfeed-show-prev)))))

;;; elfeed-routines.el ends here
; LocalWords:  elfeed
