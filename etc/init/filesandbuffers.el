;; FILE/BUFFER functions

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

(defun new-markdown-buffer ()
	"Create new empty markdown buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.md")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(markdown-mode)
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

(defun set-window-width (n)
	"Set the selected window's width."
	(adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
	"Set the selected window to 80 columns."
	(interactive)
	(set-window-width 80))

(defun toggle-fill-column ()
    "Toggle fill-column values between 32 and 55"
    (interactive)
    (setq fill-column (if (= fill-column 55) 32 55))
	(message "fill-column set to: %s" fill-column))

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


;; DIRED functions

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


;; PRINT functions

;; https://stackoverflow.com/questions/15869131/emacs-shell-command-on-buffer (adapted)
(defun spool-to-enscript ()
	"Sends current buffer to 'enscript'."
	(interactive)
	(shell-command-on-region (point-min) (point-max) "enscript -qB"))

;; https://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region (adapted)
(defun spool-to-enscript-region (&optional b e)
	"Sends current region to 'enscript'."
	(interactive "r")
	(shell-command-on-region b e "enscript -qB"))

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

