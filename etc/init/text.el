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
	"Insert ISO-formatted date."
	(interactive)
	(insert (format-time-string "%Y-%m-%d")))

(defun insert-date ()
	"Insert European-formatted date."
	(interactive)
	(insert (format-time-string "%-d %B %Y")))

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
		(shell-quote-argument (buffer-file-name)) ))
	(let ((buffer "*Shell Command Output*")) (and (get-buffer buffer) 
		(kill-buffer buffer))) )

(defun flush-blank-lines (start end)
	"Remove blank lines in a buffer."
	(interactive "r")
	(flush-lines "^\\s-*$" start end nil))

(defun replace-smart-quotes (beg end)
	"Replace 'smart quotes' in buffer or region with ascii quotes."
	(interactive "r")
	(format-replace-strings '(
		("\x201C" . "\"")
		("\x201D" . "\"")
		("\x2018" . "'")
		("\x2019" . "'") ) nil beg end))

(defun number-paragraphs (&optional takefirst)
	"Numbers resp. renumber paragraphs.

	If starting from already numbered, take that value as offset."
	(interactive "*P")
	(let ((counter 0)
		(last 0))
		(when  (looking-at "\\([0-9]+\\)\. ")
			(setq counter (car (read-from-string  (match-string-no-properties 1))))
			(forward-paragraph))
		(while (and (forward-paragraph) (< last (point)))
			(setq last (copy-marker (point)))
			(backward-paragraph)
			(skip-chars-forward " \t\r\n\f")
			(when (looking-at "[0-9]+\. ")
				(delete-region (match-beginning 0) (match-end 0)))
			(insert (format "%s. " (1+ counter)))
			(setq counter (1+ counter))
			(goto-char last))))
