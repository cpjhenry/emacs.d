(defun create-scratch-buffer ()
	"Create new *scratch* buffer."
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(text-mode))

(defun remove-scratch-buffer ()
	"Kill *scratch* buffer."	
	(interactive)
	(if (get-buffer "*scratch*")
	(kill-buffer "*scratch*")))

(defun nuke-all-buffers ()
	"Kill all buffers, leaving *scratch* only."
	(interactive)
	(mapcar (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))

(defun xah-new-empty-buffer ()
	"Create new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.txt")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(setq buffer-offer-save t)
		(olivetti-mode)))

(defun rename-file-and-buffer (new-name)
	"Renames both current buffer and file it's visiting to NEW-NAME."
	(interactive "sNew name: ")
	(let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	(progn
		(rename-file filename new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
	"Moves both current buffer and file it's visiting to DIR."
	(interactive "DNew directory: ")
	(let* ((name (buffer-name))
		(filename (buffer-file-name))
		(dir
		(if (string-match dir "\\(?:/\\|\\\\)$")
		(substring dir 0 -1) dir))
		(newname (concat dir "/" name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	(progn
		(copy-file filename newname 1)
		(delete-file filename)
		(set-visited-file-name newname)
		(set-buffer-modified-p nil) t))))
 
(defun mydired-sort ()
	"Sort dired listings with directories first."
	(save-excursion
		(let (buffer-read-only)
			(forward-line 2) ;; beyond dir. header 
			(sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
		(set-buffer-modified-p nil)))

(defadvice dired-readin
	(after dired-after-updating-hook first () activate)
	"Sort dired listings with directories first before adding marks."
	(mydired-sort))
