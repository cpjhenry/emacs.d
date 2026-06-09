;;; filesandbuffers.el --- FILE/BUFFER functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(defun my/backward-paragraph ()
  "Backward paragraph."
  (interactive "^")
  (backward-paragraph)
  (recenter-top-bottom 0))

(defun my/forward-paragraph ()
  "Forward paragraph."
  (interactive "^")
  (forward-paragraph)
  (recenter-top-bottom 0))

(defun my/backward-page ()
  "Backward page."
  (interactive "^")
  (backward-page)
  (recenter-top-bottom 0)
  (beginning-of-line))

(defun my/forward-page ()
  "Forward page."
  (interactive "^")
  (forward-line)
  (forward-page)
  (recenter-top-bottom 0)
  (beginning-of-line))

(declare-function outline-previous-heading "outline")
(defun my/outline-previous-heading ()
  "Go to previous heading."
  (interactive "^")
  (outline-previous-heading)
  (recenter-top-bottom 0))

(declare-function outline-next-heading "outline")
(defun my/outline-next-heading ()
  "Go to next heading."
  (interactive "^")
  (outline-next-heading)
  (recenter-top-bottom 0))

(defun my/end-of-buffer ()
  "End of buffer."
  (interactive "^")
  (goto-char (point-max))
  (recenter -1))


(declare-function View-scroll-line-backward "view")
(defun my/View-scroll-line-backward ()
  "Scroll line backward, jump to new top of screen."
  (interactive)
  (View-scroll-line-backward)
  (move-to-window-line-top-bottom))

(defun my/init ()
  "Load init-file."
  (interactive)
  (find-file user-init-file))

(declare-function mistty "mistty")
(defun my/shell ()
  "Open shell at ~/ always."
  (interactive)
  (let ((default-directory "~"))
    (mistty)))


;; buffers
(defun scratch ()
  "Make a new temporary scratch file.

Useful if your *scratch* is already holding something important."
  (interactive)
  (switch-to-buffer (make-temp-name "*scratch*-")))

(defun copy-current-to-temp-buffer ()
  "Copy the current buffer or region, create temp buffer, paste it there."
  (interactive)
  (let ((beg (point-min)) (end (point-max)))
    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))

    (kill-ring-save beg end)
    (switch-to-buffer (make-temp-name ""))
    (yank))

  ;; clean up section markers, so text folds correctly.
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward "" nil t) ; group separator
	(replace-match "")))))

(defun my/delete-other-windows (&rest _)
  "Advice wrapper to discard unnecessary arguments to function."
  (delete-other-windows))

(defun quit-window-kill ()
  "Kill when quitting."
  (interactive)
  (quit-window t))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (kill-dired-buffers))

(defun kill-unmodified-file-buffer (file)
  "Kill FILE's buffer if it exists and is unmodified."
  (when-let ((buf (get-file-buffer file)))
    (unless (buffer-modified-p buf)
      (kill-buffer buf))))

(defun delete-empty-file-buffer (file)
  "Kill FILE's buffer and delete FILE if it is empty and unmodified."
  (when-let ((buf (get-file-buffer file)))
    (with-current-buffer buf
      (when (and (not (buffer-modified-p))
                 (zerop (buffer-size)))
        (kill-buffer buf)
        (when (file-exists-p file)
          (delete-file file))))))

(defun shell-command-on-buffer ()
  "Asks for a command and execute it with current buffer as input."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
			   (read-shell-command "Shell command on buffer: ")))

;; http://xahlee.info/emacs/emacs/emacs_auto_save.html
(defun save-all-unsaved ()
  "Save all unsaved files. No ask."
  (interactive)
  (save-some-buffers t))

(defun shell-other-frame ()
  "Open a `shell' in a new frame."
  (interactive)
  (let ((default-directory "~")
	(buf (shell)))
    (switch-to-buffer-other-frame buf)))

(defun buf-to-LF()
  "Set UTF coding system for current buffer."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix)
  (set-buffer-modified-p nil))

;; See: https://emacs.stackexchange.com/questions/81361/how-to-switch-to-a-buffer-from-terminal-with-a-unique-partial-name
(declare-function -find "-find")
(declare-function -compose "-compose")
(declare-function -partial "-partial")

(defun switch-to-buffer-matching (regular-expression)
  "Switch to the first buffer that matches REGULAR-EXPRESSION."
  (switch-to-buffer (-find (-compose (-partial #'string-match-p regular-expression) #'buffer-name)
			   (buffer-list))))


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
  "Raise Emacs and select the provided FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (ns-raise-emacs)
      (toggle-frame-maximized))))


;; misc. functions
(defun set-window-width (n)
  "Set the selected window's width as `N'."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(defun toggle-fill-column ()
  "Toggle `fill-column' values between 32 and 70."
  (interactive)
  (setq fill-column (if (= fill-column 70) 32 70))
  ;; three values: (setq fill-column (if (= fill-column 8) 4 (if (= fill-column 4) 2 8)))
  (message "'fill-column' set to: %s" fill-column))

(defun fill-max-column ()
  "Set `fill-column' to maximum width."
  (interactive)
  (setq fill-column 0)
  (message "Maximum width."))

(defvar visual-fill-column-mode)
(defvar visual-fill-column-center-text)
(declare-function visual-fill-column-mode "visual-fill-column")
(declare-function visual-fill-column-adjust "visual-fill-column")
(defun toggle-fill-column-center ()
  "Toggle `visual-fill-column-center-text'.

If `visual-fill-column-mode' isn't activated, activates it. Disables this
mode when toggled off."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode)
    (progn
      (if (bound-and-true-p visual-fill-column-center-text)
	  (progn (setq visual-fill-column-center-text nil)
		 (visual-fill-column-mode -1))
	  (progn (setq visual-fill-column-center-text t)))
      (visual-fill-column-adjust))
    (progn (visual-fill-column-mode)
	   (toggle-fill-column-center))))

(require 'shr)
(defun preview-html ()
  "Render buffer as HTML."
  (interactive)
  (shr-render-buffer (current-buffer)))

(defun eval-r (b e)
  "Evaluate region."
  (interactive "r")
  (eval-region b e)
  (deactivate-mark)
  (message "Region evaluated."))

(defun turn-off-cursor ()
  "Hides cursor locally."
  (interactive)
  (setq-local cursor-type nil))

(defun toggle-cursor-off/on ()
  "Toggle cursor visibility."
  (interactive)
  (if (bound-and-true-p cursor-type)
      (setq-local cursor-type nil)
    (setq-local cursor-type t)))

;; https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically
;; usage: (custom/reset-var 'somevar)
(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun commify-number (n)
  "Return N with thousands separators."
  (let ((s (number-to-string n)))
    (while (string-match "\\([0-9]+\\)\\([0-9][0-9][0-9]\\)" s)
      (setq s (replace-match "\\1,\\2" nil nil s)))
    s))


;; DIRED functions
(require 'dired)
(defun dired-home ()
  "Go to first line in Dired buffer."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun dired-end ()
  "Go to last line in Dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-previous-line 1))

(defun kill-dired-buffers ()
  "Kill all Dired buffers."
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
  "Move both current buffer and file it's visiting to DIR."
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

;; dired extension "% s"
;; https://social.tchncs.de/@stackeffect/113431684014013180
(require 'dired-aux)
(defun my-substspaces (str)
  "Replace spaces in filename with underscores."
  (subst-char-in-string ?\s ?_ str))

(defun my-dired-substspaces (&optional arg)
  "Rename all marked (or next ARG) files replacing spaces with underscores."
  (interactive "P")
  (dired-rename-non-directory #'my-substspaces "Rename by substituting spaces" arg))

;; https://old.reddit.com/r/emacs/comments/91xnv9/noob_delete_buffer_automatically_after_removing/
(defun dired-kill-before-delete (file &rest _)
  "Automatically delete the buffer of the FILE that's being deleted."
  (if-let ((buf (get-file-buffer file)))
      (kill-buffer buf)
    (dolist (dired-buf (dired-buffers-for-dir file))
      (kill-buffer dired-buf))))
(advice-add 'dired-delete-file :before 'dired-kill-before-delete)


;; iBuffer functions
(require 'ibuffer)
(defun my/ibuffer-visit-buffer ()
  "Visit the buffer on this line."
  (interactive)
  (ibuffer-visit-buffer)
  (let ((buffer "*Ibuffer*"))
    (and (get-buffer buffer)
         (kill-buffer buffer))))

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
  (let (err1 err2)
    (while (> arg 0)
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
  "Go to next header."
  (interactive)
  (while (ibuffer-next-line)))

(defun ibuffer-previous-header ()
  "Go to previous header."
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


;; scrolling
(defun window-half-height ()	(max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-up-half ()	(interactive) (scroll-up (window-half-height)))
(defun scroll-down-half ()	(interactive) (scroll-down (window-half-height)))


;; web browsing
(declare-function elpher-go "")
(defun elpher:eww-browse-url (original url &optional new-window)
  "Handle gemini links."
  (cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url) (elpher-go url))
	(t (funcall original url new-window))))

(defun eww-reddit-redirect (url)
  "Redirect reddit.com to old.reddit.com automatically."
  (replace-regexp-in-string "https://www.reddit.com" "https://old.reddit.com" url))

(defun cpj/use-system-browser ()
  "Use the macOS default browser for URL handling."
  (interactive)
  (setopt browse-url-browser-function
          #'browse-url-default-macosx-browser))

(defun cpj/use-eww-browser ()
  "Use EWW for URL handling."
  (interactive)
  (setopt browse-url-browser-function #'eww-browse-url))


;; Lookup words in browser
;; cpj / Sage
(defvar search-engine-query-url "https://duckduckgo.com/?q="
  "Base query URL for browser searches.")

(defun browser-search (&optional term)
  "Search TERM in a web browser.

When called interactively:
- use the active region, if any;
- otherwise use symbol at point;
- otherwise use word at point;
- otherwise do nothing.

Prefers `browse-url-secondary-browser-function' if available,
falling back to `browse-url-browser-function'."
  (interactive)
  (let* ((term (or term
                   (when (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning)
                      (region-end)))
                   (thing-at-point 'symbol t)
                   (thing-at-point 'word t)))
         (term (and term (string-trim term))))
    (if (string-blank-p (or term ""))
        (message "No searchable term at point.")
      (let ((browse-url-browser-function
             (or browse-url-secondary-browser-function
                 browse-url-browser-function)))
        (message "Searching for %s..." term)
        (browse-url
         (concat search-engine-query-url
                 (url-hexify-string term)))))))

;;; filesandbuffers.el ends here
