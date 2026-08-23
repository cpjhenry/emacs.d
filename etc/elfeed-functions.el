;;; elfeed-functions.el --- Elfeed routines
;;; commentary:
;;; code:
(require 'elfeed)
(require 'shr)

;;; daily briefings machinery

(defvar cpj/elfeed-daily-filter
  "+unread +daily"
  "Search filter for the daily Elfeed briefing.")

(defun cpj/elfeed-daily ()
  "Display the daily briefing feeds in Elfeed."
  (interactive)
  (elfeed-search cpj/elfeed-daily-filter)
  (setq-local elfeed-search-date-format '("%H:%M" 5 :left)
              mode-name "Elfeed Daily")
  (elfeed-search-update :force))

(defun cpj/elfeed-search-clear-filter ()
  "Restore the default Elfeed search view."
  (interactive)
  (kill-local-variable 'elfeed-search-date-format)
  (setq-local mode-name "elfeed-search")
  (elfeed-search-clear-filter))

(defun cpj/elfeed-show-refresh ()
  "Refresh an Elfeed entry using local display policy."
  (setq-local elfeed-show-author
              (not (memq 'daily (elfeed-entry-tags elfeed-show-entry))))
  (elfeed-show-refresh--mail-style))

;;; generalized elfeed helper functions

(defun cpj/elfeed-search-goto-top (&rest _)
  "Move point and window to the first Elfeed entry."
  (when elfeed--position-restore-wpoint
    (remove-hook 'pre-redisplay-functions
                 elfeed--position-restore-wpoint 'local)
    (setq elfeed--position-restore-wpoint nil))
  (goto-char (point-min))
  (set-window-point (selected-window) (point-min))
  (set-window-start (selected-window) (point-min))
  (hl-line-highlight))

(defun cpj/elfeed-search-beginning-to-point-as-read ()
  "Mark entries from the beginning through the entry at point as read."
  (interactive)
  (mark-from-beginning-of-buffer)
  (forward-line 1)
  (elfeed-search-untag-unread))

(defun cpj/elfeed-search-mark-all-as-read ()
  "Mark all as read."
  (interactive)
  (goto-char (point-min))
  (push-mark (point-max) nil t)
  (activate-mark)
  (elfeed-search-untag-unread))

(defun cpj/elfeed-show-visit-secondary-browser ()
  "Visit buffer in secondary browser."
  (interactive)
  (elfeed-show-visit '(4)))

(defun cpj/elfeed-show-toggle-images ()
  "Toggle images in `elfeed-show'."
  (interactive)
  (setq shr-inhibit-images (not shr-inhibit-images))
  (elfeed-show-refresh)
  (message "Inhibit images: %s" shr-inhibit-images))

(defun cpj/elfeed-show-scroll-up-half-or-next ()
  "Scroll half a window or go to the next entry."
  (interactive)
  (condition-case nil
      (scroll-up-command (cpj/window-half-height))
    (end-of-buffer
     (elfeed-show-next))))

(defun cpj/elfeed-show-scroll-down-half-or-prev ()
  "Scroll half a window or go to the previous entry."
  (interactive)
  (condition-case nil
      (scroll-down-command (cpj/window-half-height))
    (beginning-of-buffer
     (elfeed-show-prev)
     (goto-char (point-max))
     (recenter -1))))


;;; Tidy show buffer
(defcustom cpj/elfeed-show-hidden-fields
  '("Enclosure" "Link" "Tags")
  "Metadata fields to hide in Elfeed show buffers."
  :type '(repeat string)
  :group 'elfeed)

(defun cpj/elfeed-show--rewrite-date ()
  "Rewrite the Date field in the current Elfeed show buffer."
  (let* ((time (seconds-to-time
                (elfeed-entry-date elfeed-show-entry)))
         (new (format-time-string "%A, %-d %B %Y %H:%M" time)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^Date:[ \t]*\\(.*\\)$" nil t)
        (replace-match
         (concat
          (propertize "Date:" 'face 'elfeed-show-header-face)
          " "
          (propertize new 'face 'elfeed-show-date-face))
         t t)))))

(defun cpj/elfeed-show--copy-edit ()
  "Fix spurious typesetting errors in the current Elfeed show buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:blank:]]*[[*]]*[[:blank:]]*$" nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\n" nil t)
      (replace-match "\n")))
  (delete-trailing-whitespace))

(defun cpj/elfeed-show--hide-metadata ()
  "Hide selected metadata fields in the Elfeed show header."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((end (re-search-forward "^$" nil t)))
      (flush-lines
       (concat "^"
               (regexp-opt cpj/elfeed-show-hidden-fields)
               ":[[:space:]]")
       (point-min) end))))

(defun cpj/elfeed-show--wrap-title ()
  "Wrap the Elfeed title with a hanging indent."
  (setq-local word-wrap t)
  (remove-overlays (point-min) (point-max) 'cpj/elfeed-title-wrap t)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Title: " nil t)
      (let ((overlay
             (make-overlay
              (line-beginning-position)
              (1+ (line-end-position)))))
        (overlay-put overlay 'cpj/elfeed-title-wrap t)
        (overlay-put overlay 'wrap-prefix "       ")
        (overlay-put overlay 'evaporate t)))))

(defun cpj/elfeed-show-tidy-buffer ()
  "Tidy the current Elfeed show buffer."
  (when (and (derived-mode-p 'elfeed-show-mode)
             (bound-and-true-p elfeed-show-entry))
    (with-silent-modifications
      (let ((inhibit-read-only t))
        (cpj/elfeed-show--rewrite-date)
        (cpj/elfeed-show--copy-edit)
        (cpj/elfeed-show--hide-metadata)))
    (cpj/elfeed-show--wrap-title)
    (goto-char (point-min))))


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

(provide 'elfeed-functions)
;;; elfeed-functions.el ends here

; LocalWords:  elfeed
