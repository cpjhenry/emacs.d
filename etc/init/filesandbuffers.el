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

;; misc. functions

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


;; iBuffer
;; https://www.emacswiki.org/emacs/IbufferMode

(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(setq ibuffer-saved-filter-groups (quote (("home"
   	("dired" (mode . dired-mode))
	("emacs" (or
		(name . "^\\*scratch\\*$")
		(name . "^\\*Messages\\*$")
		(name . "\\.el")))
	("planner" (or
		(name . "^\\*Calendar\\*$")
		(name . "^diary$")
		(name . "^\\*Org Agenda\\*")))
;  	("perl" (mode . cperl-mode))
;  	("erc" (mode . erc-mode))
;	("gnus" (or
;		(mode . message-mode)
;		(mode . bbdb-mode)
;		(mode . mail-mode)
;		(mode . gnus-group-mode)
;		(mode . gnus-summary-mode)
;		(mode . gnus-article-mode)
;		(name . "^\\.bbdb$")
;		(name . "^\\.newsrc-dribble")))
	))))

(setq ibuffer-hidden-filter-groups (list 
	"Helm"
	"*Internal*"
	"*Shell Command Output*"
	"from-mobile.org"
	"*tramp/" ))

(defun ibuffer-advance-motion (direction)
	(forward-line direction)
	(beginning-of-line)
	(if (not (get-text-property (point) 'ibuffer-filter-group-name))
		t
		(ibuffer-skip-properties '(ibuffer-filter-group-name)
		direction)
		nil))

(defun ibuffer-previous-line (&optional arg)
	"Move backwards ARG lines, wrapping around the list if necessary."
	(interactive "P")
	(or arg (setq arg 1))
	(let (err1 err2)
		(while (> arg 0)
			(cl-decf arg)
			(setq err1 (ibuffer-advance-motion -1)
				  err2 (if (not (get-text-property (point) 'ibuffer-title)) 
			t
			(goto-char (point-max))
			(beginning-of-line)
			(ibuffer-skip-properties '(ibuffer-summary 
			ibuffer-filter-group-name) 
			-1)
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
				  err2 (if (not (get-text-property (point) 'ibuffer-summary)) 
			t
			(goto-char (point-min))
			(beginning-of-line)
			(ibuffer-skip-properties '(ibuffer-summary 
			ibuffer-filter-group-name
			ibuffer-title)
			1)
			nil)))
	(and err1 err2)))

(defun ibuffer-next-header ()
	(interactive)
	(while (ibuffer-next-line)))

(defun ibuffer-previous-header ()
	(interactive)
	(while (ibuffer-previous-line)))


;; PRINT functions
(setq enscript "enscript -cqB")

;; https://stackoverflow.com/questions/15869131/emacs-shell-command-on-buffer (adapted)
(defun spool-to-enscript ()
	"Sends current buffer to 'enscript'."
	(interactive)
	(shell-command-on-region (point-min) (point-max) enscript))

;; https://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region (adapted)
(defun spool-to-enscript-region (&optional b e)
	"Sends current region to 'enscript'."
	(interactive "r")
	(shell-command-on-region b e enscript))

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
