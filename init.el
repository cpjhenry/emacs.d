;;; Emacs configuration / cpjh -*- no-byte-compile: t; lexical-binding: t; -*-

;; Initialize terminal
(blink-cursor-mode -1)
(delete-selection-mode t)
(electric-indent-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)

(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst system-short-name (car (split-string (system-name) "\\.")) "Hostname of local machine.")
(defconst *bullwinkle* (string-equal system-short-name "bullwinkle"))
(defconst *natasha* (string-equal system-short-name "natasha"))

(defconst EMACS29 (>= emacs-major-version 29) "Running Emacs 29 or greater.")
(defconst EMACS30 (>= emacs-major-version 30) "Running Emacs 30 or greater.")

;; Add directories to load-path
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "opt" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "var" user-emacs-directory))

;; what and who am I
(if (boundp 'emacs-edition) (message "Running '%s'." emacs-edition))
(load "rc/me" 'noerror)

(unless EMACS29
	(defalias 'keymap-set 'define-key)
	(defalias 'keymap-global-set 'global-set-key))

(when *mac* (add-to-list 'default-frame-alist '(font . "Inconsolata 21"))
	(setq
	mac-function-modifier nil
	mac-control-modifier 'control	; Control
	mac-option-modifier 'meta	; Meta
	mac-command-modifier 'super	; Super
	mac-right-command-modifier 'alt	; Alt
	mac-right-option-modifier nil	; pass-thru
	ns-use-native-fullscreen t)

	(global-set-key (kbd "s-c") 'ns-copy-including-secondary)	; ⌘-c = Copy
	(global-set-key (kbd "s-x") 'kill-region)			; ⌘-x = Cut
	(global-set-key (kbd "s-v") 'yank)				; ⌘-v = Paste
	(global-set-key (kbd "s-y") 'ns-paste-secondary)

	(global-set-key (kbd "s-a") 'mark-whole-buffer)
	(global-set-key (kbd "s-E") 'edit-abbrevs)
	(global-set-key (kbd "s-h") 'ns-do-hide-emacs)
	(global-set-key (kbd "s-k") 'kill-current-buffer)
	(global-set-key (kbd "s-l") 'goto-line)
	(global-set-key (kbd "s-o") 'find-file)
	(global-set-key (kbd "s-S") 'write-file)
	(global-set-key (kbd "s-s") 'save-buffer)
	(global-set-key (kbd "s-u") 'revert-buffer)
	(global-set-key (kbd "s-w") 'delete-frame)
	(global-set-key (kbd "s-z") 'undo)

	(global-set-key (kbd "s-1")(kbd "C-x 1"))

	(dolist (key '("s-C" "s-D" "s-d" "s-e" "s-F" "s-f" "s-g" "s-j"
		"s-L" "s-M" "s-m" "s-n" "s-p" "s-q" "s-t"))
		(global-unset-key (kbd key))))

(when *gnu* (add-to-list 'default-frame-alist '(font . "Monospace 17"))
	(message "Running on GNU/Linux."))

(when *w32* (add-to-list 'default-frame-alist '(font . "Consolas 12"))
	(setq
	w32-lwindow-modifier 'super
	w32-pass-lwindow-to-system nil
	w32-apps-modifier 'hyper)
	(menu-bar-mode 1)
	(message "Running on Windows."))

;; Initialize package manager
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents (package-refresh-contents))

(unless EMACS29
	(unless (package-installed-p 'use-package)
	(package-install 'use-package)))
(require 'use-package)
(setf	use-package-always-ensure t
	use-package-verbose nil)

;; init/vcusepackage goes here, if needed.
(unless EMACS30 (load "init/vcusepackage"))

(add-to-list 'display-buffer-alist '(
	"\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	(display-buffer-no-window)
	(allow-no-window . t)))

;; settings
(set-language-environment 'utf-8)

(setq-default
	initial-major-mode 'fundamental-mode

	tab-width 4
	standard-indent 4
	help-window-select t
	indicate-empty-lines t
	x-stretch-cursor t)

(setq	default-major-mode 'text-mode
	default-input-method nil

	ad-redefinition-action 'accept
	async-shell-command-buffer 'new-buffer
	bookmark-save-flag 1
	bookmark-set-fringe-mark nil
	bookmark-sort-flag nil
	comp-async-report-warnings-errors 'silent
	completion-auto-help 'always
	completion-auto-select 'second-tab
	cursor-in-non-selected-windows nil
	delete-by-moving-to-trash t
	dictionary-server "dict.org"
	enable-recursive-minibuffers t
	enable-remote-dir-locals t ; .dir-locals.el
	find-file-visit-truename t
	frame-inhibit-implied-resize t
	frame-resize-pixelwise t
	frame-title-format nil
	gnutls-verify-error nil
	global-auto-revert-non-file-buffers t ; Revert buffer when the underlying file has changed
	goto-address-mail-face 'default
	help-clean-buttons t
	help-enable-variable-value-editing t
	ibuffer-expert t
	inhibit-compacting-font-caches t
	inhibit-default-init t
	inhibit-startup-message t ; 'About Emacs'
	inhibit-startup-buffer-menu t ; Don't show *Buffer list*
	initial-scratch-message nil ; Makes *scratch* empty
	isearch-allow-scroll t
	kill-read-only-ok t
	kill-ring-max 512
	kill-whole-line t
	ls-lisp-use-localized-time-format t
	Man-notify-method 'pushy
	mark-ring-max most-positive-fixnum
	max-lisp-eval-depth 65536
	message-kill-buffer-on-exit t
	package-archive-column-width 1
	page-delimiter "^[#; ]*"
	pop-up-windows nil
	pop-up-frames nil
	prettify-symbols-unprettify-at-point 'right-edge
	recenter-positions '(top) ; top middle bottom
	require-final-newline nil
	resize-mini-windows t
	revert-buffer-quick-short-answers t
	ring-bell-function 'ignore
	save-abbrevs 'silent
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
	visual-line-fringe-indicators '(nil right-curly-arrow)
	what-cursor-show-names t

	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case  t

	case-fold-search t)

;; https://lambdaland.org/posts/2024-12-14_emacs_catchup/
(setopt tab-always-indent 'complete)
(setopt completion-styles '(basic initials substring))

;; files
(setq	abbrev-file-name		(concat user-emacs-directory "etc/abbrev_defs")
	auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/")
	bookmark-default-file		(concat user-emacs-directory "etc/bookmarks")
	eshell-aliases-file		(concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name		(concat user-emacs-directory "var/eshell/")
	multisession-directory		(concat user-emacs-directory "var/multisession")
	nsm-settings-file		(concat user-emacs-directory "var/network-security.data")
	request-storage-directory	(concat user-emacs-directory "var/request/storage/")

	transient-history-file		(concat user-emacs-directory "var/transient/history.el")
	transient-levels-file		(concat user-emacs-directory "var/transient/levels.el")
	transient-values-file		(concat user-emacs-directory "var/transient/values.el")

	url-cache-directory		(concat user-emacs-directory "var/url/cache/")
	url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))

;; custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))

;; backups
(setq	auto-save-default nil
	backup-by-copying t
	make-backup-files nil)

(unless *w32* (require 'backup-each-save) (add-hook 'after-save-hook 'backup-each-save))

;; path
(if *mac* (use-package exec-path-from-shell
	:custom	(shell-file-name "/usr/local/bin/bash"
		"This is necessary because some Emacs installs overwrite this variable.")
		(exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH")
		"This adds PKG_CONFIG_PATH to the list of variables to grab.")
	:init	(exec-path-from-shell-initialize)))

;; garbage collection
(use-package gcmh :config (gcmh-mode 1))


;; buffers
(load "init/filesandbuffers")
(require 'formfeed-hline)
(if (featurep 'formfeed-hline) (formfeed-hline-mode))

(require 'prog-mode)
;; HACK (if (featurep 'prog-mode) (global-prettify-symbols-mode))

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
(add-hook 'shell-mode-hook 'goto-address-mode)
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
	(define-key view-mode-map (kbd "k")	'View-scroll-line-backward-top)
	(define-key view-mode-map (kbd "q")	'View-kill-and-leave))

;; Removes *Completions* buffer when done
(add-hook 'minibuffer-exit-hook
	(lambda() (let ((buffer "*Completions*")) (and (get-buffer buffer) (kill-buffer buffer)))))

;; opening multiple files
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

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
(add-hook 'focus-out-hook (lambda() (save-some-buffers t)))

;; https://protesilaos.com/codelog/2024-12-11-emacs-diff-save-some-buffers/
(add-to-list 'save-some-buffers-action-alist
	(list "d" (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
		"show diff between the buffer and its file"))

;; Tramp
(setq	tramp-default-method "ssh"
	tramp-syntax 'simplified ; C-x C-f /remotehost:filename

	tramp-auto-save-directory	(concat user-emacs-directory "var/tramp/auto-save/")
	tramp-persistency-file-name	(concat user-emacs-directory "var/tramp/persistency"))

(defvar remote-tramp-bg "linen")
(defun checker-tramp-file-hook ()
	(when (file-remote-p buffer-file-name)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'find-file-hook 'checker-tramp-file-hook)
(defun checker-tramp-dired-hook ()
	(when (file-remote-p dired-directory)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'dired-after-readin-hook 'checker-tramp-dired-hook)
(defun checker-tramp-shell-hook ()
	(when (file-remote-p default-directory)
	(face-remap-add-relative 'default :background remote-tramp-bg)))
(add-hook 'shell-mode-hook 'checker-tramp-shell-hook)


;; frames
;; https://korewanetadesu.com/emacs-on-os-x.html
(when (featurep 'ns) (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame))

(when *mac*
	;; start Emacs server
	(use-package mac-pseudo-daemon :config (mac-pseudo-daemon-mode))
	(if (not (boundp 'server-process)) (server-start))
	(if (boundp 'server-process) (message "Server running.")))


;; modeline
(use-package doom-modeline
	:custom	(doom-modeline-column-zero-based nil)
		(doom-modeline-enable-word-count t)
		(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
		(doom-modeline-icon nil)
	:hook	(after-init . doom-modeline-mode)
	:config	(use-package nerd-icons))

(setq	battery-mode-line-format "%p%% "
	display-time-24hr-format t
	display-time-default-load-average nil
	mode-line-compact nil
	mode-line-position (list mode-line-percent-position " " "(%l,%C)")
	mode-line-right-align-edge 'right-fringe)
(column-number-mode)
(display-battery-mode)
(display-time-mode -1)
(load "rc/mm" 'noerror) ; memento-mori

;; Startup time
(defun efs/display-startup-time ()
	(message "GNU Emacs %s loaded in %s with %d garbage collections." emacs-version
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)


;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
	:ensure nil
	:custom	(ido-save-directory-list-file (concat user-emacs-directory "var/ido.last"))
		(ido-enable-flex-matching t)
		(ido-show-dot-for-dired nil)
	:config (ido-mode t)

	(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil); C-x C-w remapping
	(add-to-list 'ido-ignore-buffers "*Messages*")
	(add-to-list 'ido-ignore-buffers "*Shell Command Output*")
	(add-to-list 'ido-ignore-buffers "^*tramp/")
	(add-to-list 'ido-ignore-files ".DS_Store")
	(add-to-list 'ido-ignore-files "ido.last")

	(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)
	(global-set-key (kbd "C-x C-d")	'ido-dired)

	(use-package ido-sort-mtime :config (ido-sort-mtime-mode 1)))

;; M-x enhancement
(use-package smex
	:bind	(("M-x" . smex))
	:custom	(smex-save-file (concat user-emacs-directory "var/smex.history"))
	:config	(smex-initialize))


;; Dired
(with-eval-after-load 'dired
	(require 'dired-x)
	(unless *w32* (setq dired-kill-when-opening-new-dired-buffer t))
	(setq	dired-dwim-target t ; suggest other visible Dired buffer
		dired-listing-switches "-aBhl  --group-directories-first"
		dired-omit-files (concat dired-omit-files
		"\\|^INDEX$\\|-t\\.tex$\\|\\.DS_Store$\\|\\.localized$")
		dired-omit-verbose nil)
	(require 'ls-lisp)
	(setq	ls-lisp-use-string-collate nil
		ls-lisp-use-insert-directory-program nil
		ls-lisp-ignore-case 't)

	(define-key dired-mode-map (kbd "q") 'kill-dired-buffers)
	(define-key dired-mode-map (kbd "o") 'dired-find-file-ow)
	(defalias 'dired-find-file 'dired-find-alternate-file)
	(if (keymap-lookup dired-mode-map "% s")
		(message "Error: %% s already defined in dired-mode-map")
		(define-key dired-mode-map "%s" 'my-dired-substspaces))
	(add-hook 'dired-mode-hook (lambda()(dired-omit-mode 1))))


;; Ibuffer
;; https://www.emacswiki.org/emacs/IbufferMode
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ; always use Ibuffer

(setq	ibuffer-default-sorting-mode 'alphabetic
	ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
	ibuffer-saved-filter-groups (quote (("home"
		("Dired" (mode . dired-mode) )
		("Emacs" (or (name . "^\\*scratch\\*$")
			(name . "^\\*Messages\\*$")
			(name . "\\.el")))
		("Text" (or (name . "\\.txt")
			(name . "\\.text")))
		("Markdown" (name . "\\.md"))
		("Org" (name . "\\.org"))
		("TeX" (name . "\\.tex"))
		("Planner" (or (mode . calendar-mode)
			(mode . diary-mode)
			(mode . diary-fancy-display-mode)
			(name . "^\\*daily-info\\*")
			(name . "^\\*Org Agenda\\*")
			(name . "^\\*Virgo\\*")
			(name . "^calendar@*")))
		;("erc" (mode . erc-mode))
		("Eww"   (mode . eww-mode))
		("gnus" (or (mode . message-mode)
			(mode . bbdb-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(name . "^\\.bbdb$")
			(name . "^\\.newsrc-dribble"))) ))))

(define-key ibuffer-mode-map (kbd "<up>")   'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>") 'ibuffer-next-line)
(define-key ibuffer-mode-map (kbd "<left>") 'ibuffer-previous-header)
(define-key ibuffer-mode-map (kbd "<right>")'ibuffer-next-header)
(define-key ibuffer-mode-map (kbd "<return>")(lambda()(interactive)(ibuffer-visit-buffer)
	(let ((buffer "*Ibuffer*")) (and (get-buffer buffer) (kill-buffer buffer)))))
(add-hook 'ibuffer-mode-hook (lambda()
	(ibuffer-switch-to-saved-filter-groups "home")
	(ibuffer-update nil t)))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*Shell Command Output\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp/")
(add-to-list 'ibuffer-never-show-predicates "^\\*Latex Preview Pane Welcome\\*")


;; calendar
(require 'calendar)
(require 'diary-lib)
(load "init/calendar")
(setq	diary-file "~/Documents/diary"

	diary-display-function 'diary-fancy-display
	diary-list-include-blanks t
	diary-show-holidays-flag t

	calendar-date-style 'iso
	calendar-mark-diary-entries-flag nil
	calendar-mark-holidays-flag t
	calendar-setup nil; one-frame
	calendar-view-diary-initially-flag nil
	calendar-view-holidays-initially-flag nil

	calendar-month-header '(propertize
		(format "%s %d" (calendar-month-name month) year)
		'font-lock-face 'calendar-month-header)
	world-clock-time-format "%a %e %b %R %Z"

	calendar-christian-all-holidays-flag t
	calendar-chinese-all-holidays-flag t
	holiday-general-holidays nil)

(define-key calendar-mode-map (kbd "m") nil) ; don't allow marking of diary entries
(easy-menu-remove-item calendar-mode-map '(menu-bar diary) "Mark All")

(define-key calendar-mode-map (kbd "q") 'calendar-exit-kill)
(define-key calendar-mode-map (kbd "w") 'calendar-world-clock)
(define-key calendar-mode-map (kbd "y") 'list-holidays-this-year)
(define-key diary-mode-map (kbd "C-c C-q") 'kill-current-buffer)

(easy-menu-add-item calendar-mode-map '(menu-bar goto)
	["World clock" calendar-world-clock] "Beginning of Week")
(easy-menu-add-item calendar-mode-map '(menu-bar holidays)
	["Yearly Holidays" list-holidays-this-year])

(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)
(advice-add 'calendar-goto-info-node :after (lambda (&rest r) (calendar-exit-kill) (delete-other-windows)))

(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-fancy-display-mode-hook 'alt-clean-equal-signs)
(add-to-list 'auto-mode-alist '("diary" . diary-mode))


;; Initialize packages

;; use-package directives in this order:
;; :ensure
;; :defer
;; :custom
;; :bind
;; :mode
;; :hook
;; :commands
;; :init
;; :config

(use-package which-key
	:config (which-key-mode)
	(defalias 'which-key-alias 'which-key-add-key-based-replacements))

(use-package elpher
	:bind (	:map elpher-mode-map
		("[" . elpher-back)
		("]" . elpher-down))
	:hook	(elpher-mode . (lambda()
			(setq-local left-margin-width 10)
			(set-window-buffer nil (current-buffer))))
	:init	(easy-menu-add-item global-map '(menu-bar tools)
			["Gopher" elpher :help "Browse Gopherspace"] 'browse-web)
	:config	(advice-add 'eww-browse-url :around 'elpher:eww-browse-url))

(use-package eww
	:ensure nil
	:custom	(browse-url-browser-function 'eww-browse-url)
		(eww-auto-rename-buffer t)
		(eww-bookmarks-directory (concat user-emacs-directory "etc/"))
		(eww-search-prefix "https://duckduckgo.com/html?q=")
		(shr-inhibit-images t)
		(shr-use-colors nil)
		(shr-use-fonts nil)
		(shr-bullet "• ")
		(shr-folding-mode t)
		(shr-indentation 2)	; Left-side margin
		(shr-width nil)		; Fold text for comfiness
		(url-privacy-level '(email agent lastloc))
	:bind (	:map eww-mode-map
		("[" . eww-back-url)
		("]" . eww-forward-url)
		:map eww-bookmark-mode-map
		("w" . eww))
	:config	(url-setup-privacy-info)
	(add-hook 'eww-after-render-hook 'eww-readable) ;; default to 'readable-mode'

	(use-package ace-link :config (ace-link-setup-default))) ;; alternative to tabbing

(use-package flycheck)

(use-package free-keys :defer t)

(use-package go-mode)

(use-package google-this
	:config	(google-this-mode)
		(which-key-alias "C-c /" "google-this"))

(use-package google-translate
	:bind (	("C-c t t" . google-translate-at-point)
		("C-c t <RET>" . google-translate-smooth-translate))
	:config	(setq google-translate-translation-directions-alist '(
			("fr" . "en") ("en" . "fr")))
		(which-key-alias "C-c t" "google-translate"))

(use-package hl-todo
	:custom	(hl-todo-keyword-faces `(
		("TODO"       warning bold)
		("FIXME"      error bold)
		("HACK"       font-lock-constant-face bold)
		("REVIEW"     font-lock-keyword-face bold)
		("NOTE"       success bold)
		("DEPRECATED" font-lock-doc-face bold)))
	:hook	(prog-mode . hl-todo-mode)
		(emacs-lisp-mode . hl-todo-mode))

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

(use-package ssh)

(use-package visible-mark) ; make the mark visible


;; Configure specific machines
(when *natasha*
	(setq	browse-url-secondary-browser-function 'browse-url-generic
		browse-url-generic-program "/Applications/Waterfox.app/Contents/MacOS/waterfox")

	;; Mail / News
	(require 'rmail)
	(setq
	rmail-primary-inbox-list '("imaps://cn914@mail.ncf.ca")
	rmail-movemail-variant-in-use 'mailutils
	rmail-remote-password-required t

	smtpmail-smtp-server "mail.ncf.ca"
	send-mail-function   'smtpmail-send-it
	smtpmail-smtp-service 587

	rmail-mime-prefer-html nil
	rmail-preserve-inbox nil
	rmail-delete-after-output t
	rmail-mail-new-frame t
	rmail-mime-prefer-html nil

	rmail-highlighted-headers "^Subject:"
	rmail-ignored-headers (concat rmail-ignored-headers
		"\\|^In-Reply-To:\\|^Content-Type:\\|^DKIM-Filter:")
	rmail-nonignored-headers nil

	rmail-secondary-file-directory	(concat user-emacs-directory "var/")
	rmail-default-file		(concat rmail-secondary-file-directory "XMAIL")
	rmail-file-name			(concat rmail-secondary-file-directory "RMAIL"))

	(add-hook 'rmail-show-message-hook 'goto-address-mode)
	(add-hook 'rmail-quit-hook 'kill-current-buffer)

	(require 'message)
	(define-key message-mode-map (kbd "A-<return>") 'message-send-and-exit)

	;; RSS
	(use-package elfeed
		:init	(easy-menu-add-item global-map '(menu-bar tools)
			["Read RSS Feeds" elfeed :help "Read RSS Feeds"] "Read Mail")
		:bind (	("C-c f" . elfeed)
			:map elfeed-search-mode-map
			("/" . elfeed-search-live-filter)
			("\\" . elfeed-search-set-filter-nil)
			("[" . beginning-of-buffer) ; top
			("]" . end-of-buffer) ; bottom
			("m" . elfeed-mail-todo)
			:map elfeed-show-mode-map
			("[" . beginning-of-buffer)
			("]" . end-of-buffer)
			("TAB" . shr-next-link)
			("SPC" . scroll-up-half))
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

		(load "rc/feeds" 'noerror 'nomessage)
		(load "init/elfeed-routines"))

	;; Web
	(use-package w3m
		;; let's load it, but not make it the default.
		;; :init (setq browse-url-browser-function 'w3m-browse-url)
		:bind ( ("C-x m" . browse-url-at-point)
			:map w3m-mode-map
			("<left>" . w3m-view-previous-page)
			("&" . macosx-open-url)
			("Q" . my/w3m-quit)
			("R" . tsa/w3m-toggle-readability)
			("M-o" . ace-link-w3m))
		:config (setq
			w3m-bookmark-file (concat user-emacs-directory "etc/w3m-bookmarks.html")
			w3m-confirm-leaving-secure-page nil
			w3m-default-save-directory "~/Downloads"
			w3m-use-filter nil)

		(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
		(require 'mime-w3m)
		;; (require 'w3m-filter)
		;; (add-to-list 'w3m-filter-configuration '(t "Make page readable" ".*" tsa/readability))
		(load "init/w3m-routines.el"))

	(use-package nov ; Read ePub files
		:custom (nov-save-place-file (concat user-emacs-directory "var/nov-places"))
		:init	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

	(use-package save-check
		:vc (save-check :url "https://github.com/skx/save-check.el.git"
		:rev :newest)
		:config (setq save-check-show-eval t)
			(global-save-check-mode t))

	(use-package xkcd
		:init	(setq	xkcd-cache-dir    (concat user-emacs-directory "var/xkcd/")
				xkcd-cache-latest (concat user-emacs-directory "var/xkcd/latest"))
		:config (defun xkcd-add-alt (&rest r)
			(interactive)
			(read-only-mode -1)
			(setq-local cursor-type nil fill-column (window-width))
			(visual-line-mode 1)
			(insert "\n\n" xkcd-alt "\n")
			(read-only-mode t))
		(advice-add 'xkcd-alt-text :override #'xkcd-add-alt)
		(advice-add 'xkcd-get :after #'xkcd-add-alt)))

(when *gnu*
	(setq	browse-url-secondary-browser-function 'browse-url-generic
		browse-url-generic-program "firefox-esr"))


;; Text, Prog, and Markdown modes
(require 'table)
(add-hook 'text-mode-hook (lambda()
	(abbrev-mode)
	(goto-address-mode)
	(table-recognize)
	(visual-line-mode)))
(add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)
(define-key text-mode-map (kbd "C-M-i") nil)

;; HACK convert to :custom
(use-package visual-fill-column
	:hook	(visual-line-mode . visual-fill-column-mode)
	:config (setq visual-fill-column-fringes-outside-margins nil)
		(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(use-package adaptive-wrap
	:hook	(visual-line-mode . adaptive-wrap-prefix-mode))

(use-package hl-sentence) ; highlight current sentence

(use-package typo) ; minor mode for typographic editing

;; prog-mode
(add-hook 'prog-mode-hook (lambda()
	(setq show-trailing-whitespace t)
	(abbrev-mode)
	(when (not (equal major-mode 'lisp-interaction-mode)) (display-line-numbers-mode))
	(goto-address-prog-mode)
	(prettify-symbols-mode)
	(show-paren-local-mode)
	(visual-fill-column-mode -1)))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook (lambda() (setq tab-width 8 truncate-lines -1)))

;; bash
(add-to-list 'auto-mode-alist '("\\.bash*" . sh-mode))
(define-key shell-mode-map (kbd "M-r") nil)
(define-key shell-mode-map (kbd "M-p") nil)

;; html
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-hook 'html-mode-hook (lambda()
	(visual-line-mode -1)
	(visual-fill-column-mode -1)
	(toggle-truncate-lines 1)))

;; do not mark long lines in whitespace-mode
(require 'whitespace)
(delete 'lines whitespace-style)

;; Markdown
(use-package markdown-mode
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
	:init 	(setq markdown-hide-urls t)
	:config (add-to-list 'markdown-uri-types "gemini"))

(load "init/text") ; text functions


;; TeX
(unless *w32* (use-package tex
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

	(use-package reftex :ensure nil :hook (LaTeX-mode . turn-on-reftex))))


;; spell checking
(use-package jinx
	:if (executable-find "aspell")
	:bind (	("M-$" . jinx-correct)
		("C-M-$" . jinx-languages))
	:hook	(emacs-startup . global-jinx-mode)
	:config
	(load "init/jinx-routines")
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


;; Org-mode
(require 'org)
(require 'ox-md)

(setq-default
	org-startup-indented nil
	org-pretty-entities t
	org-use-sub-superscripts "{}"
	org-hide-emphasis-markers t
	org-ellipsis "$"
	org-startup-with-inline-images t
	org-image-actual-width '(300))

(setq	org-directory "~/Documents/org"
	org-agenda-file (concat org-directory "/daily.org")
	org-agenda-files (list org-agenda-file)
	org-agenda-text-search-extra-files '(agenda-archives)
	org-default-notes-file (concat org-directory "/notes.org")
	org-id-locations-file (concat user-emacs-directory "var/org-id-locations")

	org-startup-folded 'content		; folded children content all
	org-startup-shrink-all-tables t

	org-catch-invisible-edits 'smart
	org-ctrl-k-protect-subtree t
	org-cycle-separator-lines 2
	org-footnote-auto-adjust t
	org-footnote-define-inline t
	org-list-allow-alphabetical t
	org-log-done t				; 'CLOSED' logging
	org-log-repeat nil
	org-log-state-notes-into-drawer nil
	org-priority-enable-commands t
	org-return-follows-link t
	org-src-fontify-natively t
	org-special-ctrl-a/e t
	org-support-shift-select 'always

	org-auto-align-tags nil
	org-tags-column 0)

(setq	org-agenda-include-diary nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-start-on-weekday nil
	org-agenda-todo-ignore-deadlines t
	org-agenda-todo-ignore-scheduled t)

(setq 	org-export-with-author t
	org-export-with-date t
	org-export-with-toc nil

	org-export-with-broken-links t
	org-export-with-timestamps t
	org-export-time-stamp-file t
	org-export-date-timestamp-format "%Y-%m-%d"

	org-ascii-text-width 50
	org-ascii-inner-margin 2
	org-ascii-quote-margin 4
	org-ascii-headline-spacing '(0 . 1)

	org-md-headline-style 'atx

	org-latex-compiler "xelatex"
	org-latex-pdf-process (list
		(concat "latexmk -" org-latex-compiler " -recorder -synctex=1 -bibtex-cond %b")))

(setq	org-tags-exclude-from-inheritance '("PROJECT")
	org-todo-keywords '((sequence "TODO" "DONE"))
	org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold)) )
	org-emphasis-alist '(
	("*" bold)
	("**" bold)
	("/" italic)
	("_" italic)
	("=" (:background "maroon" :foreground "white"))
	("~" (:background "deep sky blue" :foreground "MidnightBlue"))
	("+" (:strike-through t)))


	org-agenda-custom-commands '(
	("P" "Project List"	((tags "PROJECT")))
	("O" "Office"		((agenda)(tags-todo "OFFICE")))
	("W" "Weekly Plan"	((agenda)(todo "TODO")(tags "PROJECT")))
	("H" "Home NA Lists"((agenda)(tags-todo "HOME")(tags-todo "COMPUTER"))))

	org-capture-templates '(
	("c" "Cookbook" entry (file "~/Documents/org/cookbook.org")
	"%(org-chef-get-recipe-from-url)" :empty-lines 1)
	("m" "Manual Cookbook" entry (file "~/Documents/org/cookbook.org")
	"* %^{Recipe title: }\n:PROPERTIES:\n:source-url:\n:servings:\n:prep-time:\n:cook-time:\n:ready-in:\n:END:\n** Ingredients\n%?\n** Directions\n\n\n** Notes\n\n")

	;; https://benadha.com/notes/how-i-manage-my-reading-list-with-org-mode/
	("i" "📥 Inbox" entry (file "~/Documents/org/inbox.org") "* %?\n  %i\n" :prepend t)
	("j" "📔 Journal" entry (file+datetree "~/Documents/org/journal.org") "* %? %^G\nEntered on %U\n  %i\n")
	("b" "📑 Bookmark" entry (file "~/Documents/org/bookmarks.org") "* %? %^g\n  %i\n" :prepend t)
	("s" "🛒 Shopping List" entry (file+headline "~/Documents/org/shoppinglist.org" "SHOPPING LIST") "* TODO %?\n  %i\n" :prepend t)

	;; https://github.com/rexim/org-cliplink
	("K" "Cliplink capture task" entry (file "") "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)
	) ; capture templates
	) ; set

(set-face-underline 'org-ellipsis nil)

(use-package org-appear ; automatic visibility toggling of Org elements
	:hook (org-mode . org-appear-mode))

(use-package org-autolist ; pressing "Return" will insert a new list item automatically
	:hook (org-mode . org-autolist-mode))

(use-package org-cliplink) ; insert org-mode links from the clipboard

;; (use-package org-modern ; add some styling to your Org buffer
;; 	:custom	(org-modern-fold-stars nil)
;; 		(org-modern-keyword nil)
;; 		(org-modern-checkbox nil)
;; 		(org-modern-table nil)
;; 		(org-modern-tag nil)
;; 	:hook (org-mode . global-org-modern-mode))

(use-package ox-report) ; export your org file to minutes report PDF file

;; (when *mac* (use-package org-mac-link ; grab links from various mac apps
;;	:config
;;	(define-key org-mode-map (kbd "C-c o g") 'org-mac-link-get-link)))

(when *natasha* (use-package org-chef))

(define-key org-mode-map (kbd "M-[") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-]") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "C-M-<left>" ) 'outline-up-heading)
(define-key org-mode-map (kbd "C-M-<right>") (lambda()(interactive)(org-end-of-subtree)))
(define-key org-mode-map (kbd "s-S-<left>")  (lambda()(interactive)(org-call-with-arg 'org-todo 'left)))
(define-key org-mode-map (kbd "s-S-<right>") (lambda()(interactive)(org-call-with-arg 'org-todo 'right)))
(define-key org-mode-map (kbd "s-S-<up>") 'org-priority-up)
(define-key org-mode-map (kbd "s-S-<down>") 'org-priority-down)

;; primarily for cbc-mode, but also useful for other org files in view-mode
(with-eval-after-load 'view
	(define-key view-mode-map (kbd "[") 'org-previous-link)
	(define-key view-mode-map (kbd "]") 'org-next-link)
	(define-key view-mode-map (kbd "RET") nil))

(define-key org-mode-map (kbd "C-c '") 'org-edit-special-no-fill)

;; ispell should not check code blocks in org mode
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
(add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
(add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

(add-hook 'org-agenda-finalize-hook 'delete-other-windows)

(add-hook 'org-mode-hook (lambda() (visual-fill-column-mode -1)))

(load "init/org") ; org-mode functions

;; fix table.el error
;; https://github.com/doomemacs/doomemacs/issues/6980
(defun myfunc/check_table_p (oldfunc) (funcall oldfunc t))
(advice-add 'org-at-table-p :around 'myfunc/check_table_p)


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


;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up	<next> is fn-down

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>" ) 'move-end-of-line)
(global-set-key (kbd "A-<left>") [home])
(global-set-key (kbd "A-<right>") [end])

(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) (lambda()(interactive)(end-of-buffer)(recenter -1)))
(global-set-key (kbd "C-<prior>") 'scroll-down-line)
(global-set-key (kbd "C-<next>" ) 'scroll-up-line)

;; M-<home>		'beginning-of-buffer-other-window
;; M-<end>		'end-of-buffer-other-window
;; M-<prior>		'scroll-other-window-down
;; M-<next>		'scroll-other-window

(defalias 'b-o-l 'beginning-of-line)
(global-set-key (kbd "M-<up>")   (lambda()(interactive)(backward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<down>") (lambda()(interactive)(forward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<left>") (lambda()(interactive)(backward-page)(recenter-top-bottom)(b-o-l)))
(global-set-key (kbd "M-<right>")(lambda()(interactive)(next-line)(forward-page)(recenter-top-bottom)(b-o-l)))


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

;; mouse
;; https://github.com/purcell/disable-mouse
(setq	mouse-yank-at-point t
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
(when (fboundp 'windmove-default-keybindings)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down))

; tweaking window sizes
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)

;; Winner mode is a global minor mode that records the changes in the window configuration.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
;(winner-mode t)


;; alternate keys
(bind-key "C-a"     'back-to-indentation-or-beginning-of-line) ; move-beginning-of-line
(bind-key "C-w"     'kill-region-or-backward-word) ; kill-region
(bind-key "M-w"     'kill-region-or-thing-at-point) ; kill-ring-save
(bind-key "M-j"     'join-line) ; default-indent-new-line (see 'C-M-j')

(bind-key "C-S-k"   'kill-whole-line)
(bind-key "C-x S-u" 'undo-redo) ; (see also 's-M-z')

(global-set-key (kbd "C-s")	'isearch-forward-regexp)
(global-set-key (kbd "C-r")	'isearch-backward-regexp)
(global-set-key (kbd "M-s s")	'isearch-forward)
(global-set-key (kbd "M-s r")	'isearch-backward)

(global-set-key (kbd "<f12>")	'list-buffers)
(global-set-key (kbd "TAB")	'self-insert-command)

(global-set-key (kbd "M-<f11>")	'toggle-modeline)
(global-set-key (kbd "A-<return>") (kbd "M-<return>"))

;; extended commands (alternates)
;(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; avoid accidental exits
;(global-unset-key (kbd "C-x C-c"))
;(global-set-key (kbd "C-x C-c c") 'save-buffers-kill-terminal)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Darwin overrides
(when *mac*
	(when (display-graphic-p)
		(dolist (key '("C-<f10>")); "<f10>" "S-<f10>" "M-<f10>"))
		(global-unset-key (kbd key))))

	(global-set-key (kbd "C-x C-c") (lambda() (interactive)
		(if (boundp mac-pseudo-daemon-mode) (mac-pseudo-daemon-mode -1))
		(save-buffers-kill-terminal)))
		(which-key-alias "C-x C-c" "save-buffers-kill-terminal")
	(bind-key "s-M-z" 'undo-redo))

;; quit cleanly
(global-set-key (kbd "C-c C-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

;; unbind C-M-? keys
;; FIXME

;; Disable the "numeric argument". Prefer universal argument (C-u) prefix.
(dolist (prefix '("C-" "M-" "C-M-"))
	(keymap-global-unset (concat prefix "-"))
	(dotimes (i 10) (keymap-global-unset (concat prefix (number-to-string i)))))


;; Disabled functions
;(setq disabled-command-function 'enable-me)

(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil) ; C-x C-u
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-02/msg01410.html
(with-eval-after-load 'help-fns (put 'help-fns-edit-variable 'disabled nil))

(put 'suspend-frame 'disabled t) ; C-z / C-x C-z

;; Safe local variables
(add-to-list 'safe-local-variable-values '(org-log-done))
(add-to-list 'safe-local-variable-values '(truncate-lines . -1))


;; Shortcuts

(bind-key "<f5>"	'my/fill-column)
(defun my/fill-column () (interactive) (setq fill-column 0) (message "Maximum width."))

(bind-key "<f6>"	'toggle-fill-column-center)
(bind-key "<f7>"	'ispell-buffer)
(bind-key "<f8>"	'list-bookmarks)
(bind-key "<f9>"	'shortcuts-mode)

(bind-key "C-`"		'scratch-buffer)
(bind-key "C-<escape>"	'my/shell)
(defun my/shell () (interactive) (let ((default-directory "~")) (mistty)))

(bind-key "M-<f1>"	'my/emacs-help)
(bind-key "M-<f2>"	'describe-personal-keybindings)
(bind-key "M-<f3>"	'shortdoc)

(bind-key "M-q"		'my/fill-paragraph)
(defun my/fill-paragraph () (interactive) (fill-paragraph nil t))
(bind-key "M-Q"		'unfill-paragraph)

(bind-key "C-M-;"	'eval-r)
(bind-key "C-M-y"	'undo-yank)

;; Ctrl-c (personal keybindings)
(bind-key "C-c a"	'org-agenda)
(when *mac*
(bind-key "C-c z"	'my/agenda))

(bind-key "C-c b m"	'new-markdown-buffer)
(bind-key "C-c b n"	'new-empty-buffer)
(bind-key "C-c b o"	'new-org-buffer)
(which-key-alias "C-c b" "buffers")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC"	'display-current-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-alias "C-c d" "dates")

(bind-key "C-c e"	'elpher) ; gopher / gemini
(bind-key "C-c g"	'grep-completing-read)

(bind-key "C-c m"	'menu-bar-read-mail)
(which-key-alias "C-c m" "read-mail")
;(bind-key "C-c n"	'newsticker-show-news)

(bind-key "C-c o a"	'org-archive-subtree-default)
(bind-key "C-c o c"	'org-capture)
(bind-key "C-c o k"	'org-cliplink)
(bind-key "C-c o l"	'org-store-link)
(bind-key "C-c o r"	'org-mode-restart)
(bind-key "C-c o t"	'org-toggle-link-display)
(which-key-alias "C-c o" "org")

(bind-key "C-c q"	'dictionary-search)
(bind-key "C-c w"	'eww-list-bookmarks) ; www
(which-key-alias "C-c w" "eww")

(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x d"	'delete-duplicate-lines)
(bind-key "C-c x g"	'replace-garbage-chars)
(bind-key "C-c x l"	'lorem-ipsum-insert-paragraphs)
(bind-key "C-c x n"	'number-paragraphs)

(bind-key "C-c x w"	'whack-whitespace)
(bind-key "C-c x x"	'delete-whitespace-rectangle)
(bind-key "C-c x y"	'whitespace-cleanup)
(which-key-alias "C-c x" "text")

(global-set-key (kbd "C-c 8 c") (kbd "✓"))
(global-set-key (kbd "C-c 8 n") (kbd "№"))
(global-set-key (kbd "C-c 8 p") (kbd "¶"))
(which-key-alias "C-c 8" "key translations")

;; Ctrl-x (buffer functions)
(bind-key "C-x c"	'kill-current-buffer)
(bind-key "C-x n f"	'narrow-to-focus)

(bind-key "C-x x L"	'buf-to-LF)
(bind-key "C-x x c"	'toggle-fill-column)
(bind-key "C-x x k"	'kill-other-buffers)
(bind-key "C-x x l"	'add-file-local-variable)
(bind-key "C-x x m"	'move-buffer-file)
(bind-key "C-x x r"	'rename-file-and-buffer)
(bind-key "C-x x v"	'view-text-file-as-info-manual)
(bind-key "C-x x w"	'preview-html)
(which-key-alias "C-x x" "buffers")

(which-key-alias "C-x 8" "key translations")
(which-key-alias "C-x 8 e" "emojis")
(which-key-alias "C-x a" "abbrev")
(which-key-alias "C-x n" "narrow")
(which-key-alias "C-x p" "project")
(which-key-alias "C-x r" "registers")
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

(when *mac* (bind-key "s-i" 'my/init))

;; Work-specific
(when *w32* (setq
	default-directory "c:/Users/henrypa/OneDrive - City of Ottawa/"
	org-agenda-file (concat default-directory "!.org")))

; LocalWords:  canadian sug aspell memq eval RET kfhelppanels init FN
; LocalWords:  pdfexport melpa vers tls dg defs eshell multisession
; LocalWords:  persistency ido Ibuffer elfeed rc rmh elfeedroutines
; LocalWords:  esr md noindent nEntered shoppinglist Cliplink el kbd
; LocalWords:  INPROGRESS kfhelp setq xm readabilizing JS dev Lorem
; LocalWords:  Gopherspace filesandbuffers ipsum ePub epub xelatex
; LocalWords:  vcusepackage latexmk synctex bibtex cond
