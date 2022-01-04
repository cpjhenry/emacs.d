;; file, buffer, dired routines

(defun create-scratch-buffer ()
	"Create new *scratch* buffer."
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(funcall (and initial-major-mode)) )

(defun new-empty-buffer ()
	"Create new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.txt")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(setq buffer-offer-save t) ))

(defun nuke-all-buffers ()
	"Kill all buffers, leaving *scratch* only."
	(interactive)
	(mapcar (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))

(defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(kill-dired-buffers))

(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer) 
		(when (eq 'dired-mode (buffer-local-value 'major-mode buffer)) 
		(kill-buffer buffer))) 
		(buffer-list)))

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

(defun recentf-open-files-compl ()
	(interactive)
	(let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
			recentf-list))
			(fname (completing-read "File name: " tocpl nil nil)))
		(when fname
		(find-file (cdr (assoc-string fname tocpl))))))

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

(defun set-window-width (n)
	"Set the selected window's width."
	(adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
	"Set the selected window to 80 columns."
	(interactive)
	(set-window-width 80))

;; Adapted following script to OSX:
;; https://genomeek.wordpress.com/2013/03/08/emarch-2-create-a-pdf-with-highlighted-code-source/
(defun print-to-pdf ()
 (interactive)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "pstopdf tmp.ps -o " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (message (concat "File printed in : "(buffer-name) ".pdf")) )

;; automatically save buffers associated with files on buffer or window switch
(defadvice switch-to-buffer (before save-buffer-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
;; automatically save buffers associated with files on frame (app) switch
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(defmacro with-system (type &rest body)
	"Evaluate BODY if `system-type' equals TYPE."
	(declare (indent defun)) `(when (eq system-type ',type),@body))
