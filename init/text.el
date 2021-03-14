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
