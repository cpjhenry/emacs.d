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

(defun copy-current-to-temp-buffer ()
	"Copy the current buffer or region, create temp buffer, paste it there."
	(interactive)
	(let ((beg (point-min)) (end (point-max)))
		(when (region-active-p)
			(setq beg (region-beginning))
			(setq end (region-end)))

		(kill-ring-save beg end)
		(switch-to-buffer (make-temp-name ""))
		(yank)))

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

;; https://emacs.stackexchange.com/questions/3116/how-to-display-a-message-in-echo-area-only
(defun echo-and-ignore-message-buffer (message)
	(let ((prev-msg-log-max message-log-max))
	(unwind-protect (progn (setq message-log-max nil)
		(message message))
	(setq message-log-max prev-msg-log-max))))

(defun shell-other-frame ()
  "Open a `shell' in a new frame."
  (interactive)
  (let ((default-directory "~")
	(buf (shell)))
    ;; (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-frame buf)))


;; modeline functions
;; https://jiewawa.me/2024/10/useful-emacs-commands-for-reading/
(defun kill-modeline ()
	(setq-local mode-line-format nil))

(defun restore-modeline ()
	(kill-local-variable 'mode-line-format))

(defun toggle-modeline () "Toggle modeline."
	(interactive)
	(if (null mode-line-format) (restore-modeline)
	(kill-modeline)))


;; macOS frame functions
(defun ns-raise-emacs ()
"Raise Emacs."
	(ns-do-applescript "tell application \"Emacs\" to activate"))

(defun ns-raise-emacs-with-frame (frame)
"Raise Emacs and select the provided frame."
	(with-selected-frame frame
	(when (display-graphic-p)
		(ns-raise-emacs)
		(toggle-frame-maximized))))


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

(defun preview-html () "Render buffer as HTML."
	(interactive)
	(shr-render-buffer (current-buffer)))

(defun eval-r (b e) "Evaluate region."
	(interactive "r")
	(eval-region b e)
	(deactivate-mark)
	(message "Region evaluated."))

(defun my/agenda () "Load org-agenda file."
	(interactive)
	(find-file org-agenda-file))

(defun my/init () "Load init-file."
	(interactive)
	(find-file user-init-file))

(defun my/outline-previous-heading ()
	(interactive)
	(outline-previous-heading)
	(recenter-top-bottom))

(defun my/outline-next-heading ()
	(interactive)
	(outline-next-heading)
	(recenter-top-bottom))

(defun View-scroll-line-backward-top () "Scroll line backward, jump to new top of screen."
	(interactive)
	(View-scroll-line-backward)
	(move-to-window-line-top-bottom))

(defun org-edit-special-no-fill () "Call a special editor for the element at point; turn off fill."
	(interactive)
	(org-edit-special)
	(visual-fill-column-mode -1))


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

(defun dired-find-file-ow()
	(interactive)
	(dired-find-file-other-window)
	(delete-other-windows))

;; dired extension "% s"
;; https://social.tchncs.de/@stackeffect/113431684014013180
(defun my-substspaces (str)
	(subst-char-in-string ?\s ?_ str))

(defun my-dired-substspaces (&optional arg)
"Rename all marked (or next ARG) files so that spaces are replaced with underscores."
	(interactive "P")
	(dired-rename-non-directory #'my-substspaces "Rename by substituting spaces" arg))


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
	(let (err1 err2) (while (> arg 0)
		(cl-decf arg)
		(setq   err1 (ibuffer-advance-motion -1)
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
	(let (err1 err2) (while (> arg 0)
		(cl-decf arg)
		(setq   err1 (ibuffer-advance-motion 1)
			err2 (if (not (get-text-property (point) 'ibuffer-summary)) t
				(goto-char (point-min))
				(beginning-of-line)
				(ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name
					ibuffer-title) 1)
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


;; scrolling
(defun window-half-height ()	(max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-up-half ()	(interactive) (scroll-up (window-half-height)))
(defun scroll-down-half ()	(interactive) (scroll-down (window-half-height)))


;; web browsing
(defun elpher:eww-browse-url (original url &optional new-window) "Handle gemini links."
	(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url) (elpher-go url))
		(t (funcall original url new-window))))

(defun elpher-up ()	(interactive)(backward-paragraph)(recenter-top-bottom))
(defun elpher-down () 	(interactive)(forward-paragraph)(recenter-top-bottom))


;; https://vishesh.github.io/emacs/editors/2023/01/25/lean-emacs-config.html
;; (see bindings for: "C-a" "C-w" "M-w" "M-j")

(defun back-to-indentation-or-beginning-of-line ()
  "Moves cursor to beginning of line, taking indentation into account."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun kill-region-or-backward-word ()
"If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun kill-region-or-thing-at-point (beg end)
  "If a region is active kill it, or kill the thing (word/symbol) at point."
  (interactive "r")
  (unless (region-active-p)
    (save-excursion
      (setq beg (re-search-backward "\\_<" nil t))
      (setq end (re-search-forward "\\_>" nil t))))
  (kill-ring-save beg end))

(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun open-line-below ()
  "Starts a new line below the current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Starts a new line above the current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun grep-completing-read ()
"Execute grep/ripgrep search using `completing-read'."
	(interactive)
	(let* ((default-term (if (region-active-p)
			(substring-no-properties (buffer-substring (mark) (point)))
			(thing-at-point 'symbol)))
		(term (read-string "search for: " default-term))
		(execute-search (lambda (term) (if (executable-find "rg")
			(process-lines "rg" "--line-number" term)
			(process-lines "git" "grep" "-niH" "-e" term))))
		(results (funcall execute-search term))
		(line-list (split-string (completing-read "results: " results) ":"))
		(rfile (car line-list))
		(rlnum (string-to-number (car (cdr line-list)))))
	(find-file rfile)
	(goto-line rlnum)
	(recenter)))

(defun fast-file-view-mode ()
  "Makes the buffer readonly and disables fontlock and other bells and
whistles for faster viewing."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (font-lock-mode -1)
  (when (boundp 'anzu-mode) (anzu-mode -1)))

(defun large-find-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (fast-file-view-mode)))

;; https://www.emacswiki.org/emacs/DeletingWhitespace
(defun whack-whitespace (arg)
"Delete all white space from point to the next word. With prefix ARG
delete across newlines as well. The only danger in this is that you
don't have to actually be at the end of a word to make it work. It
skips over to the next whitespace and then whacks it all to the next
word."
(interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))
