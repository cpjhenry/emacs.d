;; text functions

(defun insert-tab-char ()
	"Insert a tab char. (ASCII 9, \t)"
	(interactive)
	(insert "\t"))

(defun unfill-paragraph ()
	"Takes a multi-line paragraph and makes it into a single line of text."
	(interactive)
	(let ((fill-column (point-max)))
	(fill-paragraph nil)))

(defun insert-iso-date ()
	(interactive)
	(insert (format-time-string "%Y-%m-%d")))

(defun insert-date ()
	(interactive)
	(insert (format-time-string "%d %B %Y")))

(defun todo (text &optional body)
	(interactive "sTodo: ")
	(compose-mail-other-window "cpjh" text)
	(mail-text)
	(if body
		(insert body))
	(message-send-and-exit) )

(defun elfeed-mail-todo (&optional use-generic-p)
	"Mail this to myself for later reading"
	(interactive "P")
	(let ((entries (elfeed-search-selected)))
		(cl-loop for entry in entries
			do (elfeed-untag entry 'unread)
			when (elfeed-entry-title entry)
			do (todo it (elfeed-entry-link entry)))
		(mapc #'elfeed-search-update-entry entries)
		(unless (use-region-p) (forward-line))))
;(define-key elfeed-search-mode-map (kbd "m") 'elfeed-mail-todo)

(defun markdown-preview-file ()
	"Run Marked on the current file and revert the buffer"
	(interactive)
	(shell-command (format "open -a /Applications/Marked\\ 2.app %s"
		(shell-quote-argument (buffer-file-name)) )) )

(defun flush-blank-lines (start end)
	(interactive "r")
	(flush-lines "^\\s-*$" start end nil))
