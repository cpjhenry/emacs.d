;;; init.el --- Emacs configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; brew install emacs-plus --with-modern-black-dragon-icon --with-mailutils --with-imagemagick

;; /usr/local/share/emacs/site-lisp

;;; Code:
;; Initialize terminal
(blink-cursor-mode -1)
(delete-selection-mode t)
(electric-indent-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)

;; Add directories to load-path
(add-to-list 'load-path (directory-file-name (expand-file-name "etc/" user-emacs-directory)))
(add-to-list 'load-path (directory-file-name (expand-file-name "opt/" user-emacs-directory)))
(add-to-list 'load-path (directory-file-name (expand-file-name "var/" user-emacs-directory)))

;; Environmental constants
(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst system-short-name (car (split-string (system-name) "\\.")) "Hostname of local machine.")
(defconst *bullwinkle* (string-equal system-short-name "bullwinkle"))
(defconst *natasha* (string-equal system-short-name "natasha"))

(defconst EMACS29 (>= emacs-major-version 29) "Running Emacs 29 or greater.")
(defconst EMACS30 (>= emacs-major-version 30) "Running Emacs 30 or greater.")
(defconst EMACS31 (>= emacs-major-version 31) "Running Emacs 31 or greater.")

(if (boundp 'emacs-edition) (message "Running '%s'." emacs-edition))
(load "rc/me" 'noerror)

;; Customize
(when *mac*
	(setopt	mac-function-modifier nil
		mac-control-modifier 'control	; Control
		mac-option-modifier 'meta	; Meta
		mac-command-modifier 'super	; Super
		mac-right-command-modifier 'alt	; Alt
		mac-right-option-modifier nil)	; pass-thru

	;; HACK keymap-global-set
	(global-set-key (kbd "s-c") 'ns-copy-including-secondary)	; ⌘-c = Copy
	(global-set-key (kbd "s-x") 'kill-region)			; ⌘-x = Cut
	(global-set-key (kbd "s-v") 'yank)				; ⌘-v = Paste
	(global-set-key (kbd "s-y") 'ns-paste-secondary)

	(global-set-key (kbd "s-a") 'mark-whole-buffer)
	(global-set-key (kbd "s-E") 'edit-abbrevs)
	(global-set-key (kbd "s-f") 'isearch-forward-regexp)
	(global-set-key (kbd "s-h") 'ns-do-hide-emacs)
	(global-set-key (kbd "s-k") 'kill-current-buffer)
	(global-set-key (kbd "s-l") 'goto-line)
	(global-set-key (kbd "s-o") 'find-file)
	(global-set-key (kbd "s-S") 'write-file)
	(global-set-key (kbd "s-s") 'save-buffer)
	(global-set-key (kbd "s-u") 'revert-buffer)
	(global-set-key (kbd "s-W") 'delete-frame)
	(global-set-key (kbd "s-w") 'kill-current-buffer)
	(global-set-key (kbd "s-z") 'undo)

	(global-set-key (kbd "s-1") (kbd "C-x 1"))

	(dolist (key '("s-C" "s-D" "s-d" "s-e" "s-F" "s-f" "s-g" "s-j" "s-L"
		       "s-M" "s-m" "s-n" "s-p" "s-q" "s-t" "s-^" "s-&" "s-|"))
		(global-unset-key (kbd key)))

	;; Disable suspend-frame
	(global-unset-key (kbd "C-z"))

	;; Disable toggle-frame-fullscreen
	(global-unset-key (kbd "<f11>"))
	;; FIXME - Errors with EMACS30
	(put 'toggle-frame-fullscreen 'disabled t)

	;; Line movement
	(global-set-key (kbd "<home>") nil) ; 'move-beginning-of-line
	(global-set-key (kbd "<end>" ) nil) ; 'move-end-of-line

	;; Alternates
	(global-set-key (kbd "A-<left>") (kbd "s-<left>"))
	(global-set-key (kbd "A-<right>")(kbd "s-<right>"))
	(global-set-key (kbd "A-k") (kbd "s-k"))
	(global-set-key (kbd "A-=") (kbd "s-="))

	;; Emojis
	(easy-menu-add-item global-map '(menu-bar edit) ["Emoji & Symbols"
		ns-do-show-character-palette
		:help "Show macOS Character Palette."
		:visible (eq window-system 'ns)])

	;; Font
	(add-to-list 'default-frame-alist '(font . "Inconsolata 21")))

(when *gnu*
	(add-to-list 'default-frame-alist '(font . "Monospace 17"))
	(message "Running on GNU/Linux."))

(when *w32*
	(setopt	w32-apps-modifier 'super)

	(global-set-key (kbd "<f11>") 'toggle-frame-maximized)
	(defalias 'restart-emacs 'save-buffers-kill-terminal)

	(add-to-list 'default-frame-alist '(font . "Consolas 12"))
	(menu-bar-mode 1)
	(message "Running on Windows."))

;; Initialize package manager
(require 'package)
(require 'gnutls)
(setopt	gnutls-algorithm-priority "normal:-vers-tls1.3"
	gnutls-verify-error nil
	package-archive-column-width 1)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))
(use-package use-package
	:ensure	nil
	:custom	(use-package-always-ensure t)
		(use-package-compute-statistics t)
		(use-package-verbose t))

;; settings
(set-language-environment 'utf-8)

(defvar	default-major-mode 'text-mode "Mode when creating new buffers.")
(setopt	initial-major-mode 'fundamental-mode
	standard-indent 4
	tab-width 4

	;indent-line-function 'indent-according-to-mode
	;tab-always-indent nil

	ad-redefinition-action 'accept
	async-shell-command-buffer 'new-buffer
	case-fold-search t
	confirm-kill-processes nil ; quit Emacs directly even if there are running processes
	cursor-in-non-selected-windows nil
	delete-by-moving-to-trash t
	enable-recursive-minibuffers t
	enable-remote-dir-locals t ; .dir-locals.el
	find-file-visit-truename t
	goto-address-mail-face 'default
	grep-use-headings t
	help-clean-buttons t
	help-enable-variable-value-editing t
	help-window-select t
	indicate-empty-lines t
	inhibit-default-init t
	inhibit-startup-message t ; 'About Emacs'
	inhibit-startup-buffer-menu t ; Don't show *Buffer list*
	initial-scratch-message nil ; Makes *scratch* empty
	isearch-allow-scroll t
	kill-read-only-ok t
	kill-ring-max 512
	kill-whole-line t
	large-file-warning-threshold 100000000 ; warn when opening files bigger than 100MB
	load-prefer-newer t ; Always load newest byte code
	ls-lisp-use-localized-time-format t
	mark-ring-max most-positive-fixnum
	max-lisp-eval-depth 65536
	page-delimiter "^[#; ]*"
	pop-up-windows nil
	pop-up-frames nil
	recenter-positions '(top) ; top middle bottom
	require-final-newline nil
	resize-mini-windows t
	revert-buffer-quick-short-answers t
	ring-bell-function 'ignore
	save-interprogram-paste-before-kill t
	search-default-mode 'char-fold-to-regexp ; cafe = café
	sentence-end-double-space nil
	set-mark-command-repeat-pop t ; repeating C-SPC after popping mark pops it again
	shell-kill-buffer-on-exit t
	show-paren-style 'parenthesis
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t
	trash-directory "~/.Trash"
	use-dialog-box nil
	use-file-dialog nil
	use-short-answers t
	view-read-only nil ; turn on view mode when buffer is read-only
	what-cursor-show-names t
	x-stretch-cursor t

	;; completion
	completion-auto-help 'always
	completion-auto-select 'second-tab
	completion-styles '(basic initials substring)
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t)

;; files
(setopt	custom-file			(concat user-emacs-directory "custom.el")
	dun-log-file			(concat user-emacs-directory "var/games/dunnet-scores"))
(setq	multisession-directory		(concat user-emacs-directory "var/multisession")
	nsm-settings-file		(concat user-emacs-directory "var/network-security.data")
	request-storage-directory	(concat user-emacs-directory "var/request/storage/")
	transient-history-file		(concat user-emacs-directory "var/transient/history.el")
	transient-levels-file		(concat user-emacs-directory "var/transient/levels.el")
	transient-values-file		(concat user-emacs-directory "var/transient/values.el")
	url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))

;; path
(use-package exec-path-from-shell
	:disabled
	:if	*mac*
	:custom	(shell-file-name "/usr/local/bin/bash")
		(exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
	:init	(exec-path-from-shell-initialize))

;; garbage collection
(use-package gcmh :config (gcmh-mode 1))


;; modeline
(use-package doom-modeline
  :custom (doom-modeline-column-zero-based nil)
	(doom-modeline-enable-word-count t)
	(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
	(doom-modeline-icon nil)
	(doom-modeline-time-icon nil)
  :hook (after-init . doom-modeline-mode))

(setopt	battery-mode-line-format "%p%% "
	display-time-24hr-format t
	display-time-default-load-average nil
	mode-line-compact nil
	mode-line-position (list mode-line-percent-position " " "(%l,%C)")
	mode-line-right-align-edge 'right-fringe)
(if EMACS30 (setopt project-mode-line t))
(column-number-mode)
(display-battery-mode)
;; (display-time-mode)
;; (load "rc/mm" 'noerror) ; memento-mori

;; startup time
(defun efs/display-startup-time ()
	(message "GNU Emacs %s loaded in %s with %d garbage collections." emacs-version
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)


;; buffers
(use-package dash) ; for `-find', `-compose' and `-partial'
(load "init/filesandbuffers")

(require 'abbrev)
(setopt	abbrev-file-name (concat user-emacs-directory "etc/abbrev_defs")
	abbrev-suggest t
	save-abbrevs 'silently)

(require 'bookmark)
(setopt	bookmark-save-flag 1
	bookmark-set-fringe-mark nil
	bookmark-sort-flag nil
	bookmark-default-file	(concat user-emacs-directory "etc/bookmarks"))

(require 'em-alias)
(require 'esh-mode)
(setopt	eshell-aliases-file	(concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name	(concat user-emacs-directory "var/eshell/"))

(require 'formfeed-hline)
(if (featurep 'formfeed-hline)	(formfeed-hline-mode))

(require 'man)
(setopt	Man-notify-method 'pushy)

(require 'prog-mode)
(global-prettify-symbols-mode)
(setopt	prettify-symbols-unprettify-at-point 'right-edge)

(add-hook 'before-save-hook 'time-stamp)

;; files are given +x permissions when they're saved, if they contain a valid shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; remove trailing whitespace on-save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Emacs really suffers when you open large files.
(add-hook 'find-file-hook 'large-find-file-hook)

;; Mode hooks
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'help-mode-hook (lambda()
	(setq-local font-lock-keywords-only t)
	(goto-address-mode)))
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; Modes derived from special-mode will pick-up this directive
(define-key special-mode-map (kbd "q") 'kill-current-buffer)
(define-key messages-buffer-mode-map (kbd "q")	'bury-buffer) ; 'messages-buffer-mode

;; eval-after-loads are run once, before mode hooks
;; mode-hooks execute once for every buffer in which the mode is enabled

(with-eval-after-load 'emacs-news-mode
	(define-key emacs-news-view-mode-map (kbd "[") 'my/outline-previous-heading)
	(define-key emacs-news-view-mode-map (kbd "]") 'my/outline-next-heading)
	(define-key emacs-news-view-mode-map (kbd "{") 'outline-backward-same-level)
	(define-key emacs-news-view-mode-map (kbd "}") 'outline-forward-same-level))

(with-eval-after-load 'help-mode
	(define-key help-mode-map (kbd "[")	'help-go-back)
	(define-key help-mode-map (kbd "]")	'help-go-forward)
	(define-key help-mode-map (kbd "M-RET")	'goto-address-at-point))

(with-eval-after-load 'info
	(define-key Info-mode-map (kbd "q")	'kill-current-buffer)
	(define-key Info-mode-map (kbd "[" )	'Info-history-back)
	(define-key Info-mode-map (kbd "]")	'Info-history-forward)
	(define-key Info-mode-map (kbd "{")	'Info-backward-node)
	(define-key Info-mode-map (kbd "}")	'Info-forward-node))

(with-eval-after-load 'view
	(define-key view-mode-map (kbd "j")	'View-scroll-line-forward)
	(define-key view-mode-map (kbd "k")	'my/View-scroll-line-backward)
	(define-key view-mode-map (kbd "q")	'View-kill-and-leave)

	(define-key view-mode-map (kbd "C-<up>")   'my/backward-paragraph)
	(define-key view-mode-map (kbd "C-<down>") 'my/forward-paragraph))

;; removes *Completions* buffer when done
(add-hook 'minibuffer-exit-hook (lambda()
	(let ((buffer "*Completions*"))
	(and (get-buffer buffer) (kill-buffer buffer)))))

;; opening multiple files
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

;; backups / auto-save
(setopt	auto-save-default nil
	auto-save-list-file-prefix (concat user-emacs-directory "var/auto-save/")
	auto-save-no-message nil
	auto-save-visited-interval 60
	create-lockfiles nil

	backup-by-copying t
	delete-old-versions t
	make-backup-files t
	vc-make-backup-files nil ; don't make back-ups in git-controlled dirs
	version-control nil)

;; copies every file you save in Emacs to a backup directory tree
;; (require 'backup-each-save)
;; (add-hook 'after-save-hook 'backup-each-save)

;; http://xahlee.info/emacs/emacs/emacs_auto_save.html
;; (when (>= emacs-major-version 26)
;; 	;; real auto save
;; 	(auto-save-visited-mode t))

(if (version< emacs-version "27.1")
	(add-hook 'focus-out-hook 'save-all-unsaved)
	(setq after-focus-change-function 'save-all-unsaved))
	;; to undo this, run: (setq after-focus-change-function 'ignore)

;; https://protesilaos.com/codelog/2024-12-11-emacs-diff-save-some-buffers/
(add-to-list 'save-some-buffers-action-alist
	(list "d" (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
		"show diff between the buffer and its file"))

;; automatically save buffers associated with files on buffer or window switch
(defadvice switch-to-buffer (before save-buffer-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
	(when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
	(when buffer-file-name (save-buffer)))

;; don't immediately display these buffers
(add-to-list 'display-buffer-alist '(
	"\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	(display-buffer-no-window)
	(allow-no-window . t)))


;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
	:ensure nil
	:custom	(ido-save-directory-list-file (concat user-emacs-directory "var/ido.last"))
		(ido-enable-flex-matching t)
		(ido-show-dot-for-dired nil)
	:bind (	("C-<tab>" . ido-switch-buffer)
		("C-x C-d" . ido-dired))
	:init	(ido-mode t)
	:config (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil); C-x C-w remapping
	(add-to-list 'ido-ignore-buffers "*Messages*")
	(add-to-list 'ido-ignore-buffers "*Shell Command Output*")
	(add-to-list 'ido-ignore-buffers "^*tramp/")
	(add-to-list 'ido-ignore-buffers "^*Compile-Log*")
	(add-to-list 'ido-ignore-buffers "^*.*[Nn]ative-compile-[Ll]og*")
	(add-to-list 'ido-ignore-files ".DS_Store")
	(add-to-list 'ido-ignore-files "ido.last")

	(use-package ido-sort-mtime :config (ido-sort-mtime-mode 1)))

;; M-x enhancement
(use-package smex
	:custom	(smex-save-file (concat user-emacs-directory "var/smex.history"))
	:bind (	("M-x" . smex))
	:config	(smex-initialize))


;; Ibuffer
;; https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
	:ensure	nil
	:demand	t
	:custom	(ibuffer-default-sorting-mode 'alphabetic)
		(ibuffer-expert t)
		(ibuffer-saved-filter-groups (quote (("home"
			("Dired" (mode . dired-mode) )
			("Emacs" (or (name . "^\\*scratch\\*$")
			        (name . "^\\*Messages\\*$")
				(name . "\\.el")))
			("Shell" (or (mode . sh-mode)
			        (mode . mistty-mode)))
			("Text" (or (name . "\\.txt")
				(name . "\\.text")))
			("Markdown" (name . "\\.md"))
			("Org"  (name . "\\.org"))
			("TeX"  (name . "\\.tex"))
			("ePub" (mode . nov-mode))
			("Planner" (or (mode . calendar-mode)
				(mode . diary-mode)
				(mode . diary-fancy-display-mode)
				(name . "^\\*daily-info\\*")
				(name . "^\\*Org Agenda\\*")
				(name . "^\\*Virgo\\*")
				(name . "^calendar@*")))
			;("erc" (mode . erc-mode))
			("Eww"  (mode . eww-mode))
			("gnus" (or (mode . message-mode)
				(mode . bbdb-mode)
				(mode . mail-mode)
				(mode . gnus-group-mode)
				(mode . gnus-summary-mode)
				(mode . gnus-article-mode)
				(name . "\\.bbdb$")
				(name . "^\\.newsrc-dribble"))) ))))
	:bind ( :map ibuffer-mode-map
		("C-x C-f" . ibuffer-ido-find-file)
		("<up>" . ibuffer-previous-line)
		("<down>" . ibuffer-next-line)
		("<left>" . ibuffer-previous-header)
		("<right>" . ibuffer-next-header)
		("<return>" . my/ibuffer-visit-buffer))
	:init	(defalias 'list-buffers 'ibuffer) ; always use Ibuffer
	:config	(defun my/ibuffer-visit-buffer ()
		(interactive)
		(ibuffer-visit-buffer)
		(let ((buffer "*Ibuffer*")) (and (get-buffer buffer)
			(kill-buffer buffer))))
	(add-hook 'ibuffer-mode-hook (lambda()
		(ibuffer-switch-to-saved-filter-groups "home")
		(ibuffer-update nil t)))
	(require 'ibuf-ext)
	(add-to-list 'ibuffer-never-show-predicates "^\\*Messages\\*")
	(add-to-list 'ibuffer-never-show-predicates "^\\*Shell Command Output\\*")
	(add-to-list 'ibuffer-never-show-predicates "^\\*tramp/")
	(add-to-list 'ibuffer-never-show-predicates "^\\*Latex Preview Pane Welcome\\*"))


;; Dired
(use-package dired
	:ensure nil
	:demand t)

(use-package dired-x
	:ensure	nil
	:demand	t
	:custom	(dired-dwim-target t) ; suggest other visible Dired buffer
		(dired-listing-switches "-laGhv  --group-directories-first")
		(dired-garbage-files-regexp (concat dired-garbage-files-regexp
		"\\|\\.DS_Store$\\|\\.old$\\|\\.synctex\\.gz$\\|\\.log$\\|\\.tex$"))
		(dired-omit-verbose nil)
	:bind (	:map dired-mode-map
		("q" . kill-dired-buffers))
	:hook	(dired-mode . dired-omit-mode)
	:config	(unless *w32* (setq dired-kill-when-opening-new-dired-buffer t))
		(defalias 'dired-find-file 'dired-find-alternate-file)
		(advice-add 'dired-find-file-other-window
		  :after (lambda (&rest r) (delete-other-windows)))
		(if (keymap-lookup dired-mode-map "% s")
		  (message "Error: %% s already defined in dired-mode-map")
		  (define-key dired-mode-map "%s" 'my-dired-substspaces))
		(setopt dired-omit-files (concat dired-omit-files "\\|^\\.localized$"))
		(delete "~" dired-omit-extensions)); show backup files

;; improve file sorting
(use-package ls-lisp
	:ensure	nil
	:custom	(ls-lisp-use-string-collate nil)
		(ls-lisp-ignore-case t)
	:config	(unless *w32* (setopt ls-lisp-use-insert-directory-program nil)))


;; Tramp
(require 'tramp)
(setopt	tramp-default-method "ssh"
	tramp-syntax 'simplified ; C-x C-f /remotehost:filename

	tramp-auto-save-directory	(concat user-emacs-directory "var/tramp/auto-save/")
	tramp-persistency-file-name	(concat user-emacs-directory "var/tramp/persistency"))

(defvar remote-tramp-bg "linen")
(defun checker-tramp-file-hook () "File."
	(when (file-remote-p buffer-file-name)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'find-file-hook 'checker-tramp-file-hook)
(defun checker-tramp-dired-hook () "Directory."
	(when (file-remote-p dired-directory)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'dired-after-readin-hook 'checker-tramp-dired-hook)
(defun checker-tramp-shell-hook () "Shell."
	(when (file-remote-p default-directory)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'shell-mode-hook 'checker-tramp-shell-hook)

;; Dropbox
(require 'dropbox nil t)
(setopt dropbox-config-file (concat user-emacs-directory ".dropbox"))


;; frames
(setopt	frame-inhibit-implied-resize t
	frame-resize-pixelwise t)

;; https://korewanetadesu.com/emacs-on-os-x.html
(when (featurep 'ns) (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame))

(when *mac* ;; start Emacs server
	(use-package mac-pseudo-daemon :config (mac-pseudo-daemon-mode)
	(global-set-key (kbd "C-x C-c") 'kill-daemon-save-buffers-kill-terminal)

	(defun kill-daemon-save-buffers-kill-terminal ()
	  "Kill daemon (if running), then trigger normal exit."
	  (interactive)
	  (if (boundp 'mac-pseudo-daemon-mode) (mac-pseudo-daemon-mode -1))
	  (save-buffers-kill-terminal)))

	(if (not (boundp 'server-process)) (server-start))
	(if (boundp 'server-process) (message "Server running.")))


;; calendar
(use-package calendar
	:ensure	nil
	:custom	calendar-date-style 'iso
	:bind (	:map calendar-mode-map
		("q" . calendar-exit-kill)
		("w" . calendar-world-clock)
		("y" . list-holidays-this-year))
	:init	(setq	calendar-month-header '(propertize
			(format "%s %d" (calendar-month-name month) year)
			'font-lock-face 'calendar-month-header)
		calendar-chinese-all-holidays-flag t
		calendar-christian-all-holidays-flag t
		calendar-mark-holidays-flag t
		holiday-general-holidays nil
		world-clock-time-format "%a %e %b %R %Z")
	:config
	;; don't allow marking of diary entries
	(define-key calendar-mode-map (kbd "m") nil)
	(easy-menu-remove-item calendar-mode-map '(menu-bar diary) "Mark All")

	(easy-menu-add-item calendar-mode-map '(menu-bar goto)
		["World clock" calendar-world-clock] "Beginning of Week")
	(easy-menu-add-item calendar-mode-map '(menu-bar holidays)
		["Yearly Holidays" list-holidays-this-year])

	(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)
	(advice-add 'calendar-goto-info-node
		:after (lambda (&rest r) (calendar-exit-kill) (delete-other-windows)))

	(load "init/calendar-routines"))

(use-package diary-lib
	:after	calendar
	:ensure	nil
	:custom	diary-file "~/Documents/diary"
	:init	(setq diary-list-include-blanks t)
	:config
	(add-to-list 'auto-mode-alist '("diary" . diary-mode))
	(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
	(add-hook 'diary-fancy-display-mode-hook 'alt-clean-equal-signs)
	(define-key diary-mode-map (kbd "C-c C-q") 'kill-current-buffer))


;; Initialize packages

;; use-package directives in this order:
;; :disabled
;; :ensure
;; :demand
;; :defer
;; :custom
;; :bind
;; :mode
;; :hook
;; :commands
;; :init
;; :load
;; :config

(use-package which-key
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode)
  	  (defalias 'which-key-alias 'which-key-add-key-based-replacements))

(use-package casual
  :bind (("M-o" . casual-editkit-main-tmenu)
	 :map calendar-mode-map
	 ("M-o" . casual-calendar-tmenu)
	 :map dired-mode-map
	 ("M-o" . casual-dired-tmenu)
	 :map Info-mode-map
	 ("M-o" . casual-info-tmenu)))

(use-package dictionary
  :ensure nil
  :defer  t
  :custom (dictionary-server "dict.org"))

(use-package elpher
  :bind   (	:map elpher-mode-map
	  ("[" . elpher-back)
	  ("C-<up>" . my/backward-paragraph)
	  ("C-<down>" . my/forward-paragraph))
  :hook	  (elpher-mode . (lambda()
	  (setq-local left-margin-width 10)
	  (set-window-buffer nil (current-buffer))))
  :init	  (easy-menu-add-item global-map '(menu-bar tools)
	    ["Gopher" elpher :help "Browse Gopherspace"] 'browse-web)
  :config (advice-add 'eww-browse-url :around 'elpher:eww-browse-url))

(use-package eww
  :ensure	nil
  :custom	(browse-url-browser-function 'eww-browse-url)
		(eww-auto-rename-buffer t)
		(eww-bookmarks-directory (concat user-emacs-directory "etc/"))
		(eww-readable-adds-to-history nil)
		(eww-search-prefix "https://duckduckgo.com/html?q=")
		(shr-inhibit-images t)
		(shr-use-colors nil)
		(shr-use-fonts nil)
		(shr-bullet "• ")
		(shr-folding-mode t)
		(shr-indentation 2)	; Left-side margin
		(shr-width nil)		; Fold text for comfiness
		(url-privacy-level '(email agent lastloc))
  :bind (	("C-x g" . browse-url-at-point)
		:map eww-mode-map
		("[" . eww-back-url)
		("]" . eww-forward-url)
		("Q" . eww-unfill-paragraph)
		("C-<up>" . my/backward-paragraph)
		("C-<down>" . my/forward-paragraph)
		:map eww-bookmark-mode-map
		("w" . eww))
  :config	(url-setup-privacy-info)
  		(add-hook 'eww-after-render-hook 'eww-readable) ;; default to 'readable-mode'
		(use-package ace-link :config (ace-link-setup-default))) ;; alternative to tabbing

(use-package flycheck ; on-the-fly syntax checking
  :unless *w32*
  :custom (flycheck-keymap-prefix "!")
  :hook	  (emacs-lisp-mode . flycheck-mode)
  :init	  (require 'checkdoc)
  	  (setq checkdoc-force-docstrings-flag nil)
  :config (which-key-alias "C-x !" "flycheck")
  	  (if (featurep 'ibuf-ext)
	    (add-to-list 'ibuffer-never-show-predicates "^\\*Flycheck error messages\\*"))
	  (if (featurep 'ido)
	    (add-to-list 'ido-ignore-buffers "*Flycheck error messages*")))

(use-package free-keys :defer t
  :config	(add-to-list 'free-keys-modifiers "s" t)
		(add-to-list 'free-keys-modifiers "A" t))

(use-package go-mode :defer t)

(use-package google-this
  :disabled
  :init		(which-key-alias "C-c /" "google-this")
  :config	(google-this-mode))

(use-package google-translate
  :bind (	("C-c t t" . google-translate-at-point)
		("C-c t <RET>" . google-translate-smooth-translate))
  :init	(which-key-alias "C-c t" "google-translate")
	(setq google-translate-translation-directions-alist '(
	  ("fr" . "en") ("en" . "fr"))))

(use-package hl-todo
  :custom	(hl-todo-keyword-faces `(
		("TODO"       warning bold)
		("FIXME"      error bold)
		("HACK"       font-lock-constant-face bold)
		("REVIEW"     font-lock-keyword-face bold)
		("NOTE"       success bold)
		("DEPRECATED" font-lock-doc-face bold)))
  :hook		(prog-mode . hl-todo-mode)
		(emacs-lisp-mode . hl-todo-mode))

(use-package list-projects)

(use-package lorem-ipsum
  :init	(easy-menu-add-item global-map '(menu-bar edit)
	  ["Lorem-ipsum" lorem-ipsum-insert-paragraphs :help "Insert..."])
  :config (setq-default lorem-ipsum-sentence-separator " "))

(use-package mistty
  :bind ( :map mistty-prompt-map
	("C-p" . mistty-send-key)
	("C-n" . mistty-send-key)
	("C-r" . mistty-send-key)
	("M-p" . nil)
	("M-n" . nil)
	("M-r" . nil))
  :hook	(mistty-after-process-end . mistty-kill-buffer)
	(mistty-mode . goto-address-mode))

(use-package shortcuts-mode)

(use-package simple-httpd :ensure t)

(use-package ssh)

(use-package visible-mark) ; make the mark visible


;; Configure specific machines
(when *natasha*
	(setopt	browse-url-secondary-browser-function 'browse-url-generic
		browse-url-generic-program "/Applications/Waterfox.app/Contents/MacOS/waterfox"))

;; Mail / News
(use-package rmail
	:if	*natasha*
	:ensure nil
	:defer	t
	:custom	(rmail-secondary-file-directory	(concat user-emacs-directory "var/"))
		(rmail-default-file		(concat rmail-secondary-file-directory "XMAIL"))
		(rmail-file-name	       	(concat rmail-secondary-file-directory "RMAIL"))

		(rmail-primary-inbox-list '("imaps://cn914@mail.ncf.ca"))
		(rmail-remote-password-required t)
	:hook	(rmail-show-message . goto-address-mode)
		(rmail-quit . kill-current-buffer)
	:config	(setq
		smtpmail-smtp-server "mail.ncf.ca"
		send-mail-function   'smtpmail-send-it
		smtpmail-smtp-service 587

		rmail-mime-prefer-html nil
		rmail-preserve-inbox nil
		rmail-delete-after-output t
		rmail-mail-new-frame t
		rmail-mime-prefer-html nil
		rmail-movemail-variant-in-use 'mailutils

		rmail-highlighted-headers "^Subject:"
		rmail-ignored-headers (concat rmail-ignored-headers
			"\\|^In-Reply-To:\\|^Content-Type:\\|^DKIM-Filter:")
		rmail-nonignored-headers nil))

(use-package message
	:if	*natasha*
	:ensure nil
	:custom	(message-kill-buffer-on-exit t)
	:bind ( :map  message-mode-map ("A-<return>" . message-send-and-exit)))

;; RSS
(use-package elfeed
	:if	*natasha*
	:bind (	("C-c f" . elfeed)
		:map elfeed-search-mode-map
		("/" . elfeed-search-live-filter)
		("\\" . elfeed-search-set-filter-nil)
		("[" . beginning-of-buffer) ; top
		("]" . end-of-buffer) ; bottom
		("B" . elfeed-beginning-to-point-as-read)
		("R" . elfeed-mark-all-as-read)
		("m" . elfeed-mail-todo)
		:map elfeed-show-mode-map
		("[" . beginning-of-buffer)
		("]" . end-of-buffer)
		("TAB" . shr-next-link)
		("SPC" . scroll-up-half)
		("C-<up>" . my/backward-paragraph)
		("C-<down>" . my/forward-paragraph)
		("B" . elfeed-show-visit-secondary-browser))
	:init	(easy-menu-add-item global-map '(menu-bar tools)
			["Read RSS Feeds" elfeed :help "Read RSS Feeds"] "Read Mail")
	:config	(setq
		elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/")
		elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/")
		elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el")

		elfeed-log-level 'error
		elfeed-show-truncate-long-urls nil
		elfeed-sort-order 'ascending
		elfeed-use-curl t)

	(eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
	(advice-add 'elfeed-search-update--force :after (lambda() (goto-char (point-min))))
	(advice-add 'elfeed-search-show-entry :after 'elfeed-copy-edit)

	;; HACK Figure this one out
	;; (add-to-list 'prettify-symbols-alist '("\\&\\#38\\;" . "&"))

	(load "rc/feeds" 'noerror 'nomessage)
	(load "init/elfeed-routines"))

;; Web
(use-package w3m
	:defer	t
	:bind ( :map w3m-mode-map
		("<left>" . w3m-view-previous-page)
		("&" . macosx-open-url)
		("Q" . my/w3m-quit)
		("M-o" . ace-link-w3m))
	:commands (w3m-browse-url)
	;; :init (setq browse-url-browser-function 'w3m-browse-url)
	:config (setq
		w3m-bookmark-file (concat user-emacs-directory "etc/w3m-bookmarks.html")
		w3m-confirm-leaving-secure-page nil
		w3m-default-save-directory "~/Downloads"
		w3m-use-filter nil)
	(load "init/w3m-routines"))

;; Others
(use-package chatgpt-shell
	:disabled
	:if	*natasha*
	:defer t)

(use-package chess
	:if 	*natasha*
	:defer	t
	:custom (chess-default-engine 'chess-gnuchess)
		(chess-images-default-size 80))

(use-package nov ; Read ePub files
	:if	*natasha*
	:defer	t
	:custom (nov-save-place-file (concat user-emacs-directory "var/nov-places"))
	:init	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	:config
	(if (featurep 'ibuf-ext)
	(add-to-list 'ibuffer-never-show-predicates "^\\*nov unzip\\*"))
	(if (featurep 'ido)
	(add-to-list 'ido-ignore-buffers "*nov unzip*")))

;; (use-package save-check
;; 	:vc (save-check :url "https://github.com/skx/save-check.el.git" :rev :newest)
;; 	:config (setq save-check-show-eval t)
;; 		(global-save-check-mode t))

(use-package xkcd
	:if	*natasha*
	:hook	(xkcd-mode . turn-off-cursor)
	:init	(setq	xkcd-cache-dir    (concat user-emacs-directory "var/xkcd/")
			xkcd-cache-latest (concat user-emacs-directory "var/xkcd/latest"))
	:config (defun xkcd-add-alt (&rest r)
			(interactive)
			(read-only-mode -1)
			(setq-local fill-column (window-width))
			(visual-line-mode 1)
			(insert "\n\n" xkcd-alt "\n")
			(read-only-mode t)
			(goto-char (point-min)))
	(advice-add 'xkcd-alt-text :override #'xkcd-add-alt)
	(advice-add 'xkcd-get :after #'xkcd-add-alt))

(when *gnu*
	(setq	browse-url-secondary-browser-function 'browse-url-generic
		browse-url-generic-program "firefox-esr"))


;; Text, Prog, and Markdown modes
(require 'table)
(setopt	visual-line-fringe-indicators '(nil right-curly-arrow))

(add-hook 'text-mode-hook (lambda()
	(abbrev-mode)
	(goto-address-mode)
	;; (table-recognize)
	(visual-line-mode)))

(add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)
;(define-key text-mode-map (kbd "C-M-i") nil)

(use-package visual-fill-column
	:bind (	("<f5>" . visual-fill-column-mode)
		("<f6>"	. toggle-fill-column-center))
	;; :hook	(visual-line-mode . visual-fill-column-mode)
	:config (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(if EMACS30 (global-visual-wrap-prefix-mode)
	(use-package adaptive-wrap :hook (visual-line-mode . adaptive-wrap-prefix-mode)))

(use-package hl-sentence) ; highlight current sentence

(use-package typo) ; minor mode for typographic editing

;; prog-mode
(add-hook 'prog-mode-hook (lambda()
  (setq show-trailing-whitespace t)  ; needs to be buffer local
  (abbrev-mode)
  (when (not (equal major-mode 'lisp-interaction-mode)) ; ie. *scratch*
    (display-line-numbers-mode))
  (electric-indent-local-mode)
  (goto-address-prog-mode)
  (show-paren-local-mode)
  (if (featurep 'visual-fill-column) (visual-fill-column-mode -1))))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook (lambda()
  (setq	tab-width 8
	truncate-lines -1)))

;; bash
(add-to-list 'auto-mode-alist '("\\.bash*" . sh-mode))
(define-key shell-mode-map (kbd "M-r") nil)
(define-key shell-mode-map (kbd "M-p") nil)
(add-hook 'shell-mode-hook 'goto-address-mode)

;; html
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

;; XML
(setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsl$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xhtml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.page$" . nxml-mode) auto-mode-alist))

(autoload 'xml-mode "nxml" "XML editing mode" t)
;; (add-hook 'nxml-mode-hook 'show-parens-local-mode)

;; do not mark long lines in whitespace-mode
(require 'whitespace)
(delete 'lines whitespace-style)

;; Markdown
(use-package markdown-mode
	:demand	t
	:custom	(markdown-command "multimarkdown")
		(markdown-enable-prefix-prompts nil)
		(markdown-italic-underscore t)
		(markdown-unordered-list-item-prefix "* ")
	:bind ( :map markdown-mode-map
		("M-p" . nil)
		("C-c p" . markdown-preview-file)
		("C-x x o" . markdown-convert-buffer-to-org))
	:mode	(("README\\.md\\'" . gfm-mode)
		("\\.md\\'" . markdown-mode)
		("\\.markdown\\'" . markdown-mode)
		("\\.gmi\\'" . markdown-mode))
	:commands (markdown-mode gfm-mode)
	:init 	(setopt markdown-hide-urls t)
	:config (add-to-list 'markdown-uri-types "gemini"))

(load "init/text") ; text functions


;; Org-mode
;; HACK  convert to use-package (using :ensure nil)
;; FIXME both setq below fail as setopt

(require 'org)

;; :custom
(setopt	org-directory "~/Documents/org"
	org-default-notes-file (concat org-directory "/notes.org")
	org-id-locations-file (concat user-emacs-directory "var/org-id-locations")

	org-ctrl-k-protect-subtree t
	org-ellipsis "$"
	org-fold-catch-invisible-edits 'smart
	org-footnote-auto-adjust t
	org-footnote-define-inline t
	org-hide-emphasis-markers t
	org-image-actual-width '(300)
	org-list-allow-alphabetical t
	org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
	org-log-done 'time
	org-log-repeat nil
	org-log-state-notes-into-drawer nil
	org-pretty-entities t
	org-return-follows-link t
	org-special-ctrl-a/e t
	org-startup-folded 'content ; folded children content all
	org-startup-indented nil
	org-startup-shrink-all-tables t

	org-use-speed-commands (lambda() (and (looking-at org-outline-regexp) (looking-back "^\**")))
	org-use-sub-superscripts '{}

	org-auto-align-tags nil
	org-tags-column 0)

(defvar org-agenda-file (concat org-directory "/daily.org") "Default agenda file.")
(setopt	org-agenda-files (list org-agenda-file)
	org-agenda-include-diary nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-start-on-weekday nil
	org-agenda-text-search-extra-files '(agenda-archives)
	org-agenda-todo-ignore-deadlines t
	org-agenda-todo-ignore-scheduled t)

(setq	org-export-with-author t
	org-export-with-broken-links t
	org-export-with-date t
	org-export-with-properties nil
	org-export-with-section-numbers nil
	org-export-with-smart-quotes t
	org-export-with-sub-superscripts t
	org-export-with-tables t
	org-export-with-timestamps t
	org-export-with-toc nil

	org-export-date-timestamp-format "%Y-%m-%d"
	org-export-time-stamp-file t

	org-ascii-text-width 50
	org-ascii-inner-margin 2
	org-ascii-quote-margin 4
	org-ascii-headline-spacing '(0 . 1)

	org-latex-compiler "xelatex"
	org-latex-pdf-process
	  (list (concat "latexmk -" org-latex-compiler " -recorder -synctex=1 -bibtex-cond %b"))

	org-md-headline-style 'atx)

(load "init/org-customizations") ; templates et al.

;; :bind
(define-key org-mode-map (kbd "M-[") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-]") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "C-M-[" ) 'outline-up-heading)
(define-key org-mode-map (kbd "C-M-]") (lambda()(interactive)(org-end-of-subtree)))

;; alternative mapping for 'org-support-shift-select'
(define-key org-mode-map (kbd "S-<left>") nil)
(define-key org-mode-map (kbd "S-<right>") nil)
(define-key org-mode-map (kbd "S-<up>") nil)
(define-key org-mode-map (kbd "S-<down>") nil)
(define-key org-mode-map (kbd "S-<home>") 'org-shiftleft)
(define-key org-mode-map (kbd "S-<end>") 'org-shiftright)
(define-key org-mode-map (kbd "S-<prior>") 'org-shiftup)
(define-key org-mode-map (kbd "S-<next>") 'org-shiftdown)

(define-key org-mode-map (kbd "C-c '") 'org-edit-special-no-fill)

;; fix 'Ctrl-a' binding in org-mode
(if (fboundp 'back-to-indentation-or-beginning-of-line) (org-remap org-mode-map
	'back-to-indentation-or-beginning-of-line 'org-beginning-of-line))

;; ;; primarily for cbc-mode, but also useful for other org files in view-mode
(with-eval-after-load 'view
	(define-key view-mode-map (kbd "[") 'org-previous-link)
	(define-key view-mode-map (kbd "]") 'org-next-link)
	(define-key view-mode-map (kbd "RET") nil))

;; :hook
(add-hook 'org-agenda-finalize-hook 'delete-other-windows)
(if (featurep 'visual-fill-column)
	(add-hook 'org-mode-hook 'visual-fill-column-mode--disable))

;; :config
;; Ispell should not check code blocks in org mode
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))
(add-to-list 'org-entities-user '("textnumero" "\\textnumero" nil "&numero;" "No." "No." "№"))

;; FIXME - Errors with EMACS30
;; (use-package org-appear ; automatic visibility toggling of Org elements
;; 	:disabled
;; 	:hook (org-mode . org-appear-mode))

;; FIXME - Errors with EMACS30
;; (use-package org-autolist ; pressing "Return" will insert a new list item automatically
;; 	:hook (org-mode . org-autolist-mode))

(use-package org-autoexport
  :defer t
  :hook (org-mode . org-autoexport-mode))

(use-package org-chef
	:disabled
	:if *natasha* :defer t)

(use-package org-cliplink) ; insert org-mode links from the clipboard

(use-package org-contrib ; use ':ignore:' tag to exclude heading (but not content) from export
	:config	(require 'ox-extra)
		(ox-extras-activate '(ignore-headlines)))

(use-package org-download ; org-download-yank
	:if	*natasha*
	:custom	(org-download-heading-lvl nil)
		(org-download-image-org-width 925))

(require 'org-pretty-table)
(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

(require 'ox-latex)
(add-to-list 'org-latex-classes '("letter" "\\documentclass{letter}") t)
(add-to-list 'org-latex-classes '("memoir" "\\documentclass{memoir}"
	("\\chapter{%s}" . "\\chapter*{%s}")
	("\\section{%s}" . "\\section*{%s}")
	("\\subsection{%s}" . "\\subsection*{%s}")
	("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	t)

(require 'ox-md)

;; FIXME - cleanup
(load "init/org-functions")

;; fix table.el error
;; https://github.com/doomemacs/doomemacs/issues/6980
(defun myfunc/check_table_p (oldfunc) (funcall oldfunc t))
(advice-add 'org-at-table-p :around 'myfunc/check_table_p)


;; TeX
(use-package tex
	:unless *w32*
	:ensure auctex
	:hook	(LaTeX-mode . prettify-symbols-mode)
		(LaTeX-mode . (lambda () (push '("\\&" . ?＆) prettify-symbols-alist)))
	:config (setq
		font-latex-fontify-sectioning 'color
		ispell-parser 'tex
		LaTeX-babel-hyphen-after-hyphen nil
		latex-run-command "xelatex"
		preview-locating-previews-message nil
		preview-protect-point t
		preview-leave-open-previews-visible t
		TeX-auto-save t
		TeX-parse-self t)

	(use-package latex-extra
		:hook (LaTeX-mode . latex-extra-mode))

	(use-package latex-pretty-symbols)

	(use-package latex-preview-pane
		:bind (	:map latex-preview-pane-mode-map ("M-p" . nil) ("M-P" . nil))
		:config	(setq message-latex-preview-pane-welcome ""))

	(use-package reftex
		:ensure nil
		:hook (LaTeX-mode . turn-on-reftex)))


;; spell checking
(bind-key "<f7>" 'ispell-buffer)

(use-package jinx
	:demand	t
	:pin gnu ; source from 'gnu' package archives only
	:if (executable-find "aspell")
	:bind ( ;([remap ispell-word] . jinx-correct)
	        ("M-$" . jinx-correct)
		("C-M-$" . jinx-languages)
		("<f7>" . jinx-correct-all))
	:hook	(emacs-startup . global-jinx-mode)
	:config	(load "init/jinx-routines")
		(add-hook 'jinx-mode-hook #'my/jinx-add-ispell-localwords)
		(setf (alist-get ?* jinx--save-keys) #'my/jinx-save-as-ispell-localword))


;; print functions
(load "init/print")

(setq lpr-page-header-switches '("-t"))
(define-key global-map [menu-bar file print] nil)

(bind-key "M-p e" 'enscript)
(bind-key "M-p E" (lambda()(interactive) (enscript '(4)) (kill-buffer))) (which-key-alias "M-p E" "folded")
(bind-key "M-p f" 'fill-to-printer) (which-key-alias "M-p f" "fill buffer")
(bind-key "M-p r" 'print-buffer-or-region)
(bind-key "M-p s" 'ps-print-buffer-or-region)


;; sundry
(load "init/misc")
(load "init/kfhelp")
(load "init/cpjhelp")
(load "init/scripts" 'noerror)

;; pdf-export
(load "init/pdfexport")
(eval-after-load 'latex-mode '(define-key latex-mode-map (kbd "C-c r") 'latex-compile-and-update-other-buffer))
(eval-after-load 'markdown-mode '(define-key markdown-mode-map (kbd "C-c r") 'md-compile-and-update-other-buffer))
(eval-after-load 'org-mode '(define-key org-mode-map (kbd "C-c o r") 'org-compile-latex-and-update-other-buffer))

;; https://jonathanabennett.github.io/blog/2019/05/29/writing-academic-papers-with-org-mode/
(use-package pdf-tools
	:if	*mac*
	:custom	(pdf-annot-activate-created-annotations t)
		(pdf-view-display-size 'fit-width)
	:bind (	:map pdf-view-mode-map
		("C-s" . isearch-forward)
		("h" . pdf-annot-activate-created-annotations)
		("t" . pdf-annot-add-text-annotation)
		("D" . pdf-annot-delete))
	:magic	("%PDF" . pdf-view-mode)
	:config	(pdf-tools-install :no-query))


;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up	<next> is fn-down

;; <home> key / Ctrl-a
(if (key-binding (kbd "<home>"))
  (global-set-key (kbd "<home>") 'back-to-indentation-or-beginning-of-line))
(if (key-binding (kbd "s-<left>"))
  (global-set-key (kbd "s-<left>") 'back-to-indentation-or-beginning-of-line))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning-of-line)

(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) (lambda()(interactive)(end-of-buffer)(recenter -1)))
(global-set-key (kbd "C-<prior>") 'scroll-down-line)
(global-set-key (kbd "C-<next>" ) 'scroll-up-line)

;; M-<home>		'beginning-of-buffer-other-window
;; M-<end>		'end-of-buffer-other-window
;; M-<prior>		'scroll-other-window-down
;; M-<next>		'scroll-other-window

(global-unset-key (kbd "M-<left>"))
(global-unset-key (kbd "M-<right>"))
(global-set-key (kbd "M-[") 'my/backward-page)
(global-set-key (kbd "M-]") 'my/forward-page)
(global-set-key (kbd "M-{") 'my/backward-paragraph)
(global-set-key (kbd "M-}") 'my/forward-paragraph)


;; scroll settings
(setq	auto-window-vscroll nil
	next-screen-context-lines 0
	scroll-conservatively 10000
	scroll-margin 0
	scroll-preserve-screen-position t
	scroll-step 0)
(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

;; half-scroll
(global-set-key [prior] 'scroll-down-half)
(global-set-key [next] 'scroll-up-half)
(global-set-key (kbd "A-<up>") [prior])
(global-set-key (kbd "A-<down>") [next])
(when *mac*
	(global-set-key (kbd "s-<up>") [prior])
	(global-set-key (kbd "s-<down>") [next]))

;; (unless (package-installed-p 'ultra-scroll)
;; 	(package-vc-install '(ultra-scroll
;; 		:vc-backend Git
;; 		:url "https://github.com/jdtsmith/ultra-scroll")))
;; (use-package ultra-scroll
;; 	:init (setq scroll-conservatively 101) ; important!
;; 	:config (ultra-scroll-mode 1))


;; mouse
;; https://github.com/purcell/disable-mouse
(setopt	mouse-yank-at-point t
	mouse-wheel-progressive-speed nil
	mouse-wheel-scroll-amount '(1))

;(use-package disable-mouse)
;(global-disable-mouse-mode)
(unless *w32* (mouse-avoidance-mode 'banish))

(when *mac*
  ;; https://lmno.lol/alvaro/hey-mouse-dont-mess-with-my-emacs-font-size
  (global-set-key (kbd "<pinch>") 'ignore)
  (global-set-key (kbd "<C-wheel-up>") 'ignore)
  (global-set-key (kbd "<C-wheel-down>") 'ignore))


;; window navigation
(use-package windmove
  :ensure nil
  :bind
  (("C-M-<left>". windmove-left)
   ("C-M-<right>". windmove-right)
   ("C-M-<up>". windmove-up)
   ("C-M-<down>". windmove-down)))

;; (when (fboundp 'windmove-default-keybindings)
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down))

;; Winner mode is a global minor mode that records the changes in the window configuration.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;(winner-mode t)


;; alternate keys
(bind-key "C-w" 'kill-region-or-backward-word) ; kill-region
(bind-key "M-w" 'kill-region-or-thing-at-point) ; kill-ring-save
(bind-key "M-j" 'join-line) ; default-indent-new-line (see 'C-M-j')

(global-set-key (kbd "C-s")	'isearch-forward-regexp)
(global-set-key (kbd "C-r")	'isearch-backward-regexp)
(global-set-key (kbd "M-s s")	'isearch-forward)
(global-set-key (kbd "M-s r")	'isearch-backward)

(global-set-key (kbd "<f12>")	'list-buffers)
(global-set-key (kbd "M-<f11>")	'toggle-modeline)

(global-set-key (kbd "A-<return>") (kbd "M-<return>"))

;; https://www.matem.unam.mx/~omar/apropos-emacs.html#writing-experience
(bind-key "C-d" 'delete-forward-char) ; better replacement for delete-char
(bind-key "M-c" 'capitalize-dwim) ; capitalize-word
(bind-key "M-K" 'kill-paragraph) ; M-k capitalizes sentence
(bind-key "C-x M-t" 'transpose-paragraphs) ; C-x C-t transpose-lines
(global-set-key [remap mark-word] 'mark-whole-word)
(global-set-key [remap forward-word] 'forward-to-word) ; leaves point in better spot

;; quit cleanly
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "C-c C-g") 'keyboard-quit)

;; Disable alternate suspend-frame
(global-unset-key (kbd "C-x C-z"))

;; Disable the "numeric argument". Prefer universal argument (C-u) prefix.
(dolist (prefix '("C-" "M-" "C-M-"))
  ;(keymap-global-unset (concat prefix "-")) ; negative-argument
  (dotimes (i 10) (keymap-global-unset (concat prefix (number-to-string i)))))

;; <f10>	menu-bar-open
;; S-<f10>	context-menu-open
;; C-<f10>	buffer-menu-open
;; M-<f10>	toggle-frame-maximized

(dolist (key '("C-<f10>"))
  (global-unset-key (kbd key)))

;; Cleanup abbrev menu
(dolist (key '("C-a" "+" "-" "'"))
  (keymap-global-unset (concat "C-x a " key)))


;; Disabled functions
;(setq disabled-command-function 'enable-me)

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'suspend-frame 'disabled nil) ; C-z
(put 'upcase-region 'disabled nil) ; C-x C-u

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-02/msg01410.html
(with-eval-after-load 'help-fns (put 'help-fns-edit-variable 'disabled nil))

;; Safe local variables
(add-to-list 'safe-local-variable-values '(org-log-done))
(add-to-list 'safe-local-variable-values '(truncate-lines . -1))


;; Shortcuts
(bind-key "<f8>"	'list-bookmarks)
(bind-key "<f9>"	'shortcuts-mode)

(bind-key "C-`"		'scratch-buffer)
(bind-key "C-<escape>"	'my/shell)
(defun my/shell () (interactive) (let ((default-directory "~")) (mistty)))

(bind-key "M-<f1>"	'my/emacs-help)
(bind-key "M-<f2>"	'describe-personal-keybindings)
(bind-key "M-<f3>"	'shortdoc)
(bind-key "M-<f4>"	'org-speed-command-help)

(bind-key "M-q"		'my/fill-paragraph)
(defun my/fill-paragraph () (interactive) (fill-paragraph nil t))
(bind-key "M-Q"		'unfill-paragraph)

(bind-key "C-M-;"	'eval-r)
(bind-key "C-M-y"	'undo-yank)


;; Ctrl-c (personal keybindings)
(bind-key "C-c a"	'org-agenda)

(bind-key "C-c b m"	'new-markdown-buffer)
(bind-key "C-c b n"	'new-empty-buffer)
(bind-key "C-c b o"	'new-org-buffer)
(which-key-alias "C-c b" "buffers")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC"	'display-current-date-and-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-alias "C-c d" "dates")

(bind-key "C-c e"	'elpher) ; gopher / gemini
(bind-key "C-c g"	'grep-completing-read)
(bind-key "C-c i"	'my/init)

(bind-key "C-c m"	'menu-bar-read-mail)
(which-key-alias "C-c m" "read-mail")
;(bind-key "C-c n"	'newsticker-show-news)

(bind-key "C-c o c"	'org-capture)
(bind-key "C-c o k"	'org-cliplink)
(bind-key "C-c o l"	'org-store-link)
(bind-key "C-c o r"	'org-mode-restart)
(bind-key "C-c o t"	'org-toggle-link-display)
(which-key-alias "C-c o" "org")

(bind-key "C-c q"	'dictionary-search)
(bind-key "C-c w"	'eww-list-bookmarks) ; WWW
(which-key-alias "C-c w" "eww")

(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x d"	'delete-duplicate-lines)
(bind-key "C-c x g"	'replace-garbage-chars)
(bind-key "C-c x l"	'lorem-ipsum-insert-paragraphs)
(bind-key "C-c x n"	'number-paragraphs)

(bind-key "C-c x x"	'delete-whitespace-rectangle)
(bind-key "C-c x y"	'whitespace-cleanup)
(which-key-alias "C-c x" "text")

(bind-key "C-c z"	'my/agenda)

(global-set-key (kbd "C-c 8 c") (kbd "✓"))
(global-set-key (kbd "C-c 8 n") (kbd "№"))
(global-set-key (kbd "C-c 8 p") (kbd "¶"))
(which-key-alias "C-c 8" "key translations")


;; Ctrl-x (buffer functions)
(bind-key "C-x c"	'kill-current-buffer)
(bind-key "C-x n f"	'narrow-to-focus)

(bind-key "C-x x SPC"	'toggle-cursor-off/on)
(bind-key "C-x x L"	'buf-to-LF)
(bind-key "C-x x V"	'view-text-file-as-info-manual)
(bind-key "C-x x c"	'toggle-fill-column)
(bind-key "C-x x k"	'kill-other-buffers)
(bind-key "C-x x l"	'add-file-local-variable)
(bind-key "C-x x m"	'move-buffer-file)
(bind-key "C-x x r"	'rename-file-and-buffer)
(bind-key "C-x x v"	'view-mode)
(bind-key "C-x x w"	'preview-html)
(which-key-alias "C-x x" "buffers")

;; Additional which-key aliases
(which-key-alias "C-x 8" "key translations")
(which-key-alias "C-x 8 e" "emojis")
(which-key-alias "C-x a" "abbrev")
(which-key-alias "C-x a i" "inverse")
(which-key-alias "C-x n" "narrow")
(which-key-alias "C-x p" "project")
(which-key-alias "C-x r" "registers")
(which-key-alias "C-x t" "tabs")
(which-key-alias "C-x w" "windows")


;; Aliases
(defalias 'doe 'toggle-debug-on-error)
(defalias 'cr 'customize-rogue)
(defalias 'la 'list-abbrevs)
(defalias 'lp 'list-packages)
(defalias 'recs 'recover-session)
(defalias 'undefun 'fmakunbound)
(defalias 'unset 'makunbound)

(defalias 'arm 'auto-revert-mode)
(defalias 'art 'auto-revert-tail-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'fly 'flyspell-mode)
(defalias 'fci 'display-fill-column-indicator-mode)
(defalias 'fm 'fundamental-mode)
(defalias 'hlm 'hl-line-mode)
(defalias 'hm 'html-mode)
(defalias 'lim 'lisp-interaction-mode)
(defalias 'jsm 'js-mode)
(defalias 'mm 'markdown-mode)
(defalias 'om 'org-mode)
(defalias 'tm 'text-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'vfc 'visual-fill-column-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wm 'whitespace-mode)

(defalias 'dr 'desktop-read)
(defalias 'ds 'desktop-save)

;; Work-specific
(when *w32* (load (expand-file-name ".work" user-emacs-directory) 'noerror))
(when *mac* (bind-key "C-c Z" (lambda()(interactive) (find-file "/db:/!.org")))
      (which-key-alias "C-c Z" "work-agenda"))

;;; init.el ends here
; LocalWords:  canadian sug aspell memq eval RET kfhelppanels init FN
; LocalWords:  pdfexport melpa vers tls dg defs eshell multisession
; LocalWords:  persistency ido Ibuffer elfeed rc rmh elfeedroutines
; LocalWords:  esr md noindent nEntered shoppinglist Cliplink el kbd
; LocalWords:  INPROGRESS kfhelp setq xm readabilizing JS dev Lorem
; LocalWords:  Gopherspace filesandbuffers ipsum ePub epub xelatex
; LocalWords:  vcusepackage latexmk synctex bibtex cond xah dirs Ctrl
; LocalWords:  remotehost flycheck modeline mori featurep cbc smex
; LocalWords:  setq's setopt mailutils imagemagick usr dunnet Async
; LocalWords:  dir fullscreen dropbox keymap
