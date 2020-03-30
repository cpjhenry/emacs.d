;;; Emacs configuration / pjh

;;; Initialize terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq user-mail-address "cpjhenry@gmail.com")

(set-default-font "Inconsolata-21")

;; Mac option key is Meta (by default)
;; Mac command key is Super (by default)
(setq ns-function-modifier 'hyper) ;; Mac function key is Hyper
(setq ns-right-alternate-modifier 'alt) ;; Mac right option key is Alt
(setq mac-emulate-three-button-mouse t)

(tool-bar-mode -1) 	;; turn off tool bar
(scroll-bar-mode -1);; turn off scrollbar
(toggle-frame-maximized)

(set-default 'frame-title-format "")
(setq default-frame-alist
	'((background-color . "ivory"))) ;; use 'list-colors-display'
(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

;; Add directories to load-path
(eval-and-compile
	(mapc #'(lambda (path)(add-to-list 'load-path (expand-file-name path user-emacs-directory))) '(
		"init"
		"site-lisp"
		"site-lisp/sunrise-commander" )))
(setq default-directory (concat (getenv "HOME") "/Documents/"))
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))

;;; Initialize package manager
(eval-and-compile
	(require 'package)
	(setq package-archives '(("org" . "https://orgmode.org/elpa/")
							("melpa" . "https://melpa.org/packages/")
							("gnu" . "http://elpa.gnu.org/packages/")))
	(setq package-archive-priorities '(("org" . 3)("melpa" . 2)("gnu" . 1)))
	(package-initialize)
	;(package-refresh-contents) ;; i always fetch the archive contents on startup and during compilation
	(unless (package-installed-p 'use-package)
		(package-install 'use-package))
	(require 'use-package)
	(setf use-package-always-ensure t))

;;; settings
(setq ispell-list-command "--list") ;; correct command
(setq ispell-program-name "/usr/local/bin/aspell") ;; spell checker
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq-default tab-width 4)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;(setq shell-file-name "/usr/local/bin/bash") ;; force full subshell
;(setq shell-command-switch "-ic")

;; backups
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; remove unneeded buffers
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t) 	;; 'About Emacs'
(setq initial-scratch-message nil) 	;; Makes *scratch* empty
(setq-default message-log-max nil) 	;; Removes *messages* from the buffer
(kill-buffer "*Messages*")
(add-hook 'minibuffer-exit-hook 	;; Removes *Completions* from buffer after you've opened a file
	'(lambda () (let ((buffer "*Completions*"))
		(and (get-buffer buffer)
		(kill-buffer buffer)))))

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ;; Don't show *Buffer list*
(add-hook 'window-setup-hook 'delete-other-windows) ;; Show only one active window

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
	(global-set-key (kbd "s-K") 'nuke-all-buffers)

(defun xah-new-empty-buffer ()
	"Create new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.txt")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(setq buffer-offer-save t)
		(olivetti-mode)))
	(global-set-key (kbd "C-n") 'xah-new-empty-buffer)

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

(global-set-key (kbd "s-1") (kbd "C-x 1"))
(global-set-key (kbd "s-2") (kbd "C-x o C-x 1"))
(global-set-key (kbd "s-0") (kbd "C-x 0"))

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)

(defalias 'lcd 'list-colors-display)
(defalias 'ds 'desktop-save)
(defalias 'dt 'desktop-save)
(defalias 'dsm 'desktop-save-mode)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)
(defalias 'jsm 'js-mode)
(defalias 'fm 'fundamental-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'om 'org-mode)

(defalias 'flym 'flyspell-mode)

;;; Custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file) (load custom-file))

;;; Mode Line
(use-package smart-mode-line
	:config
	(sml/setup))
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects" ":DocProj:") t)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode)
(display-battery-mode)

;;; Emacs server
(defun server-shutdown ()
	"Save buffers, Quit, and Shutdown (kill) server."
	(interactive)
	(save-some-buffers)
	(kill-emacs))

;;; Initialize packages
(use-package elfeed)
(load (concat user-emacs-directory "elfeedrc.el")) ;; IRC
(setq elfeed-use-curl t)
(easy-menu-add-item  nil '("tools") ["Read web feeds" elfeed t])
(global-set-key (kbd "C-c w") 'elfeed)

(use-package elpher) ;; gopher
(add-hook 'elpher-mode-hook (lambda () 
	(setq left-margin-width 20)
	(set-window-buffer nil (current-buffer)) ))
(easy-menu-add-item  nil '("tools") ["Gopher" elpher t])
(global-set-key (kbd "C-c g") 'elpher)

(load (concat user-emacs-directory "ercrc.el")) ;; IRC
(easy-menu-add-item  nil '("tools")	["IRC with ERC" erc t])
(global-set-key (kbd "C-c e") 'erc)

(use-package go) ;; Game of Go
(setq gnugo-program "/usr/local/bin/gnugo")

(use-package markdown-mode
	:commands (markdown-mode gfm-mode)
	:mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:init (setq markdown-command "multimarkdown"))

(use-package nswbuff) ;; buffer switching
(global-set-key (kbd "<C-tab>") 'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "<C-S-kp-tab>") 'nswbuff-switch-to-previous-buffer)

;; Org-mode stuff
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package ssh)

;TODO Fix sunrise so it doesn't interfere with built-in 'sunrise' command
;(require 'sunrise)
;(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sunrise-virtual-mode))

(use-package vterm)

;;; Lisp & Help modes
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(setq show-paren-style 'mixed)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

(use-package form-feed) ;; ^L
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'help-mode-hook 'form-feed-mode)

;;; Emacs Text mode
(use-package olivetti
	:bind ("C-c f" . olivetti-mode)
	:config
	(progn
		(text-mode)
		(setf olivetti-body-width 80)
		(visual-line-mode))
	:mode ("\\.txt\\'" . olivetti-mode))
(add-hook 'markdown-mode-hook 'olivetti-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(eval-after-load "flyspell"
	'(progn
		(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
		(define-key flyspell-mouse-map [mouse-3] #'undefined)
		(setq flyspell-issue-message-flag nil)))

(defun unfill-paragraph ()
	"Takes a multi-line paragraph and makes it into a single line of text."
	(interactive)
	(let ((fill-column (point-max)))
	(fill-paragraph nil)))
(global-set-key (kbd "M-Q") 'unfill-paragraph)

(use-package wc-mode)
(add-hook 'text-mode-hook 'wc-mode)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'meta) ;; ‘M-left’ and ‘M-right’ to switch windows

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

;; arrow keys
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<s-prior>") 'backward-page) ;; s-H-up
(global-set-key (kbd "<s-next>") 'forward-page) ;; s-H-down

;; alternate keys
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
