; Deft Configuration
; https://jblevins.org/projects/deft/

; Deft is an Emacs mode for quickly browsing, filtering, and editing
; directories of plain text notes, inspired by Notational Velocity. It was
; designed for increased productivity when writing and taking notes by making
; it fast and simple to find the right file at the right time and by automating
; many of the usual tasks such as creating new files and saving files.

(use-package deft
	:bind (("<f7>" . deft))
	:commands (deft deft-open-file deft-new-file-named)
	:init
	(setq deft-directory "~/Documents/Notes/"
		  deft-recursive nil
		  deft-extensions '("md" "markdown" "txt" "text" "org" "tex")
		  deft-use-filter-string-for-filename nil
		  deft-use-filename-as-title t
		  deft-markdown-mode-title-level 1)

	:config
	; https://blog.xot.nl/2023/08/01/optimising-deft-for-emacs/
	(defun deft-truncate-string-to-window-width (str)
		(if str (if (> (length str) (* deft-window-width 4))
		(substring str 0 (* deft-window-width 4)) str) ""))

	(defun deft-string-width (str)
		(string-width (deft-truncate-string-to-window-width str)))

	(defun deft-truncate-string-to-width (str width)
		(truncate-string-to-width (deft-truncate-string-to-window-width str) width))

	(defun deft-file-widget (file)
	"Add a line to the file browser for the given FILE."
	(when file
		(let* ((key (file-name-nondirectory file))
		(text (deft-file-contents file))
		(title (deft-file-title file))
		(summary (deft-file-summary file))
		(mtime (when deft-time-format
			(format-time-string deft-time-format (deft-file-mtime file))))
		(mtime-width (deft-string-width mtime))
		(line-width (- deft-window-width mtime-width))
		(title-width (min line-width (deft-string-width title)))
		(summary-width (min (deft-string-width summary)
		(- line-width
		title-width
		(length deft-separator)))))
	(widget-create 'link
		:button-prefix ""
		:button-suffix ""
		:button-face 'deft-title-face
		:format "%[%v%]"
		:tag file
		:help-echo "Edit this file"
		:notify (lambda (widget &rest ignore)
		(deft-open-file (widget-get widget :tag)))
		(if title (deft-truncate-string-to-width title title-width)
		deft-empty-file-title))
	(when (> summary-width 0)
		(widget-insert (propertize deft-separator 'face 'deft-separator-face))
		(widget-insert (propertize (deft-truncate-string-to-width summary summary-width)
		'face 'deft-summary-face)))
	(when mtime
		(while (< (current-column) line-width)
		(widget-insert " "))
		(widget-insert (propertize mtime 'face 'deft-time-face)))
	(widget-insert "\n"))))
	)

	(add-hook 'deft-mode-hook (lambda()
		(local-set-key (kbd "C-c C-q") 'kill-current-buffer) ))
