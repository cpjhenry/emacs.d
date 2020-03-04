;;; Emacs configuration / pjh

;;; Initialize terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq user-mail-address "cpjhenry@gmail.com")

(set-default-font "Inconsolata-21")

(setq mac-emulate-three-button-mouse t)
(tool-bar-mode -1) 	;; turn off tool bar
(scroll-bar-mode -1);; turn off scrollbar
(toggle-frame-maximized)
;(toggle-frame-fullscreen)

(setq default-frame-alist
	'((background-color . "ivory"))) ;; list-color-display
(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

;;; Initialize package manager
(eval-and-compile
	(require 'package)
	(setq package-archives '(
							("org" . "https://orgmode.org/elpa/")
							("melpa" . "https://melpa.org/packages/")
							("gnu" . "http://elpa.gnu.org/packages/")
							))
	(setq package-archive-priorities '(
							("org" . 3)
							("melpa" . 2)
							("gnu" . 1)
							))

	(package-initialize)
	;(package-refresh-contents) ;; i always fetch the archive contents on startup and during compilation
	(unless (package-installed-p 'use-package)
		(package-install 'use-package))
	(require 'use-package)
	(setf use-package-always-ensure t))

;;; settings
(eval-and-compile ;; Add directories to load-path
	(mapc #'(lambda (path)
		(add-to-list 'load-path (expand-file-name path user-emacs-directory)))
		'("init" "site-lisp")))
(setq custom-file (concat user-emacs-directory "/custom.el"))
;(load custom-file 'noerror)
(setq default-directory (concat (getenv "HOME") "/Documents/"))
(setenv "PATH"
	(concat "/usr/local/bin" ":"
	(getenv "PATH")))
(setq exec-path (getenv "PATH"))

(setq ring-bell-function 'ignore)
(setq shell-file-name "/usr/local/bin/bash") ;; force full subshell
(setq shell-command-switch "-ic")
(setq ispell-program-name "/usr/local/bin/aspell") ;; spell checker
(setq ispell-list-command "--list") ;; correct command

;; backups
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; Remove unneeded buffers
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t) ;; 'About Emacs'
(setq initial-scratch-message nil) ;; Makes *scratch* empty
(defun remove-scratch-buffer () ;; Removes *scratch* from buffer after the mode has been set
	(if (get-buffer "*scratch*")
		(kill-buffer "*scratch*")))
	;(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
(setq-default message-log-max nil) ;; Removes *messages* from the buffer
(kill-buffer "*Messages*")
(add-hook 'minibuffer-exit-hook ;; Removes *Completions* from buffer after you've opened a file
			'(lambda ()
				(let ((buffer "*Completions*"))
				(and (get-buffer buffer)
				(kill-buffer buffer)))))

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ;; Don't show *Buffer list*
(add-hook 'window-setup-hook 'delete-other-windows) ;; Show only one active window

;; convenient
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

(defalias 'fly 'flyspell-mode)
(defalias 'go 'elpher)

;; New empty buffer
(defun xah-new-empty-buffer ()
	"Open a new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.txt")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(setq buffer-offer-save t)
		(olivetti-mode)
		))
	(global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; Ctrl+n

;;; Initialize packages
(use-package markdown-mode
	:commands (markdown-mode gfm-mode)
	:mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:init (setq markdown-command "multimarkdown"))

(use-package elpher) ;; gopher
;(require "gopher.el")

(use-package ssh)

;; Org-mode stuff
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package vterm)

;;; Emacs Text mode
(use-package visual-fill-column)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
;(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
;(add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))

;; Olivetti
(use-package olivetti
	:bind ("C-c f" . olivetti-mode)
	:config
	(progn
		(setf olivetti-body-width 80)
		(visual-line-mode))
	:mode ("\\.txt\\'" . olivetti-mode))

;; Flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)
       (setq flyspell-issue-message-flag nil)))

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
