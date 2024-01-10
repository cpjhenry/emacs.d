;; FILE/BUFFER functions

(if (< emacs-major-version 29)(defun scratch-buffer ()
	"Switch to the *scratch* buffer.
	If the buffer doesn't exist, create it first."
	(interactive)
	(pop-to-buffer-same-window (get-scratch-buffer-create))))

(defun new-empty-buffer ()
	"Create new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled")))
		(switch-to-buffer buf)
		(funcall (and default-major-mode))
		(setq buffer-offer-save t) ))

(defun new-markdown-buffer ()
	"Create new empty markdown buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.md")))
		(switch-to-buffer buf)
		(markdown-mode)
		(setq buffer-offer-save t) ))

(defun new-org-buffer ()
	"Create new empty org-mode buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.org")))
		(switch-to-buffer buf)
		(org-mode)
		(setq buffer-offer-save t) ))

(defun copy-current-buffer-to-temp-buffer ()
	"Copy the current buffer, create temp buffer, paste it there."
	(interactive)
	(kill-ring-save (point-min) (point-max))
	(switch-to-buffer (make-temp-name ""))
	(yank))

(defun kill-other-buffers ()
	"Kill all other buffers."
	(interactive)
	(mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
	(kill-dired-buffers))

(defun shell-command-on-buffer ()
"Asks for a command and executes it in inferior shell with current buffer as input."
	(interactive)
	(shell-command-on-region (point-min) (point-max)
	(read-shell-command "Shell command on buffer: ")))


;; misc. functions

(defun set-window-width (n)
	"Set the selected window's width."
	(adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
	"Set the selected window to 80 columns."
	(interactive)
	(set-window-width 80))

(defun toggle-fill-column ()
    "Toggle fill-column values between 32 and 70."
    (interactive)
    (setq fill-column (if (= fill-column 70) 32 70))
	;; three values: (setq fill-column (if (= fill-column 8) 4 (if (= fill-column 4) 2 8)))
	(message "'fill-column' set to: %s" fill-column))

(defun toggle-fill-column-center ()
	"Toggles fill-column-center when in visual-fill-column-mode."
	(interactive)
	(if (bound-and-true-p visual-fill-column-mode) (progn
		(if (bound-and-true-p visual-fill-column-center-text)
			(setq visual-fill-column-center-text nil)
			(setq visual-fill-column-center-text t) )
			(visual-fill-column-adjust) )
		(message "'visual-fill-column-mode' not enabled.") ))

;; https://emacs.stackexchange.com/questions/46935/adjust-the-line-according-to-the-screen-width
(defun dynamic-fill-column-set-var (frame)
  (when dynamic-fill-column-mode
    (setq fill-column (- (window-total-width) 3))))

(defun dynamic-fill-column-buffer-list-change ()
  (when dynamic-fill-column-mode
    (setq fill-column (- (window-total-width) 3))))

(define-minor-mode dynamic-fill-column-mode
  "Sets `fill-column' when buffer's window is resized"
  :lighter " DFC"
  (if dynamic-fill-column-mode
      (progn
        (add-hook 'window-size-change-functions 'dynamic-fill-column-set-var nil t)
        (add-hook 'buffer-list-update-hook 'dynamic-fill-column-buffer-list-change nil t))
    (remove-hook 'window-size-change-functions 'dynamic-fill-column-set-var t)
    (remove-hook 'buffer-list-update-hook 'dynamic-fill-column-buffer-list-change t)))


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


;; iBuffer functions

(defun ibuffer-advance-motion (direction)
	(forward-line direction)
	(beginning-of-line)
	(if (not (get-text-property (point) 'ibuffer-filter-group-name)) t
		(ibuffer-skip-properties '(ibuffer-filter-group-name) direction)
		nil))

(defun ibuffer-previous-line (&optional arg)
	"Move backwards ARG lines, wrapping around the list if necessary."
	(interactive "P")
	(or arg (setq arg 1))
	(let (err1 err2)
		(while (> arg 0)
			(cl-decf arg)
			(setq err1 (ibuffer-advance-motion -1)
				  err2 (if (not (get-text-property (point) 'ibuffer-title)) t
			(goto-char (point-max))
			(beginning-of-line)
			(ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name) -1)
			nil)))
	(and err1 err2)))

(defun ibuffer-next-line (&optional arg)
	"Move forward ARG lines, wrapping around the list if necessary."
	(interactive "P")
	(or arg (setq arg 1))
	(let (err1 err2)
		(while (> arg 0)
			(cl-decf arg)
			(setq err1 (ibuffer-advance-motion 1)
				  err2 (if (not (get-text-property (point) 'ibuffer-summary)) t
			(goto-char (point-min))
			(beginning-of-line)
			(ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name ibuffer-title) 1)
			nil)))
	(and err1 err2)))

(defun ibuffer-next-header ()
	(interactive)
	(while (ibuffer-next-line)))

(defun ibuffer-previous-header ()
	(interactive)
	(while (ibuffer-previous-line)))

(defun ibuffer-ido-find-file (file &optional wildcards)
	"Like `ido-find-file', but default to the directory of the buffer at point."
	(interactive
	(let ((default-directory
		(let ((buf (ibuffer-current-buffer)))
		(if (buffer-live-p buf)
			(with-current-buffer buf default-directory)
			default-directory))))
		(list (ido-read-file-name "Find file: " default-directory) t)))
	(find-file file wildcards))
	(add-hook 'ibuffer-mode-hook (lambda ()
		(define-key ibuffer-mode-map (kbd "C-x C-f") 'ibuffer-ido-find-file)) )
