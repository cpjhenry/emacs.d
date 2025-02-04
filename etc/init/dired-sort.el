(defun mydired-sort ()
"Sort Dired listings with directories first."
	(save-excursion
		(let (buffer-read-only)
			(forward-line 2) ;; beyond dir. header
			(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
		(set-buffer-modified-p nil)))

(defadvice dired-readin
	(after dired-after-updating-hook first () activate)
	"Sort Dired listings with directories first before adding marks."
	(mydired-sort))
