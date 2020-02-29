;; Initialize terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq user-mail-address "cpjhenry@gmail.com")

(set-default-font "Inconsolata-18")

(setq mac-emulate-three-button-mouse t)
(tool-bar-mode -1) 	;; turn off tool bar
(scroll-bar-mode -1);; turn off scrollbar
(toggle-frame-maximized)
;(toggle-frame-fullscreen)

;; Initialize package manager
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
	;; i always fetch the archive contents on startup and during compilation, which is slow
	;(package-refresh-contents)
	(unless (package-installed-p 'use-package)
		(package-install 'use-package))
	(require 'use-package)
	(setf use-package-always-ensure t))

;; settings
;(add-to-list 'load-path "~/.emacs.d/lisp/")
;(setq site-lisp-dir (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (concat user-emacs-directory "/custom.el"))

(setq default-directory (concat (getenv "HOME") "/Documents"))
(setq make-backup-files nil)
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-default nil)
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(setq ring-bell-function 'ignore)

;; Remove unneeded buffers
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t) ;; 'About Emacs'

;; Makes *scratch* empty.
;; oles (http://unix.stackexchange.com/questions/19874/prevent-unwanted-buffers-from-opening)
(setq initial-scratch-message nil)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
	(if (get-buffer "*scratch*")
		(kill-buffer "*scratch*")))
;(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
			'(lambda ()
				 (let ((buffer "*Completions*"))
					 (and (get-buffer buffer)
								(kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; New empty buffer
(defun xah-new-empty-buffer ()
	"Open a new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled")))
		(switch-to-buffer buf)
		(funcall (and initial-major-mode))
		(setq buffer-offer-save t)))
(setq initial-major-mode (quote text-mode))
(global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; Ctrl+n

;; force full subshell
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; spell checker
(setq ispell-program-name "/usr/local/bin/aspell")

;; Initialize packages
(use-package markdown-mode
	:ensure t
	:commands (markdown-mode gfm-mode)
	:mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:init (setq markdown-command "multimarkdown"))

(use-package elpher) ;; gopher
;(require "gopher.el")

(use-package visual-fill-column)

;; Org-mode stuff
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Emacs Text mode
;(setq-default major-mode 'text-mode)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'text-mode-hook '(lambda() (set-fill-column 80)))
(setq visual-line-fringe-indicators '(nil right-curly-arrow))
