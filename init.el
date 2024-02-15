;;; Emacs configuration / cpjh

;; Initialize terminal
(toggle-frame-maximized)
(delete-selection-mode 1)
(electric-indent-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)

(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst system-short-name (car (split-string (system-name) "\\.")) "Hostname of local machine.")
(defconst *bullwinkle*	(string-equal system-short-name "bullwinkle"))
(defconst *natasha*		(string-equal system-short-name "natasha"))

(when *mac* (add-to-list 'default-frame-alist '(font . "Inconsolata 21"))
	(setq
		mac-function-modifier nil
		mac-control-modifier 'control	; Control
		mac-option-modifier 'meta		; Meta
		mac-command-modifier 'super		; Super
		mac-right-command-modifier 'alt	; Alt
		mac-right-option-modifier nil)	; pass-thru

	(global-set-key (kbd "s-c") 'kill-ring-save)	; ⌘-c = Copy
	(global-set-key (kbd "s-x") 'kill-region) 		; ⌘-x = Cut
	(global-set-key (kbd "s-v") 'yank)				; ⌘-v = Paste
	(global-set-key (kbd "s-a") 'mark-whole-buffer)	; ⌘-a = Select all
	(global-set-key (kbd "s-z") 'undo)				; ⌘-z = Undo

	(global-set-key (kbd "s-s") 'save-buffer)
	(global-set-key (kbd "s-w") 'delete-frame))

(when *gnu* (add-to-list 'default-frame-alist '(font . "Monospace 17")) )

(when *w32* (add-to-list 'default-frame-alist '(font . "Consolas 12"))
	(setq
		w32-lwindow-modifier 'super
		w32-pass-lwindow-to-system nil
		w32-apps-modifier 'hyper)
	(message "Running on Windows."))

(when (display-graphic-p)
(add-to-list 'default-frame-alist '(background-color . "Ivory")))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq
	user-full-name "cpj"
	user-mail-address "cn914@ncf.ca"
	calendar-latitude 45.3
	calendar-longitude -75.7
	calendar-location-name "Ottawa"
	maidenhead "FN25dg")

;; Add directories to load-path
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "opt" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "var" user-emacs-directory))

;; Initialize package manager
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents (package-refresh-contents))

(unless (>= emacs-major-version 29)
	(unless (package-installed-p 'use-package)
	(package-install 'use-package)))
(require 'use-package)
(setf
	use-package-always-ensure t
	use-package-verbose t)

(use-package auto-package-update
	:config (setq
	auto-package-update-delete-old-versions t
	auto-package-update-hide-results t)
	(auto-package-update-maybe))

(use-package quelpa
	:config (setq quelpa-verbose nil))
(unless (package-installed-p 'quelpa-use-package)
	(quelpa '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)

;(load "init/straight")

;; settings
(set-language-environment 'utf-8)
(setq-default
	tab-width 4
	standard-indent 4
	help-window-select t
	indicate-empty-lines t)
(setq
	default-major-mode 'text-mode

	ad-redefinition-action 'accept
	async-shell-command-buffer 'new-buffer
	bookmark-save-flag 1
	bookmark-set-fringe-mark nil
	bookmark-sort-flag nil
	case-fold-search t
	comp-async-report-warnings-errors 'silent
	delete-by-moving-to-trash t
	dictionary-server "dict.org"
	dired-dwim-target t				; suggest other visible dired buffer
	find-file-visit-truename t
	frame-inhibit-implied-resize t
	frame-resize-pixelwise t
	frame-title-format nil
	help-clean-buttons t
	ibuffer-expert t
	inhibit-compacting-font-caches t
	inhibit-default-init t
	isearch-allow-scroll t
	kill-read-only-ok t
	kill-ring-max 512
	kill-whole-line t
	mark-ring-max most-positive-fixnum
	max-lisp-eval-depth 65536
	ns-use-native-fullscreen t
	pop-up-windows nil
	pop-up-frames nil
	recenter-positions '(top)		; top middle bottom
	require-final-newline nil
	ring-bell-function 'ignore
	save-abbrevs 'silent
	search-default-mode 'char-fold-to-regexp ; cafe = café
	sentence-end-double-space nil
	show-paren-style 'parenthesis
	tramp-default-method "ssh"
	tramp-syntax 'simplified		; C-x C-f /remotehost:filename
	trash-directory "~/.Trash"
	use-dialog-box nil
	use-file-dialog nil
	view-read-only t				; turn on view mode when buffer is read-only
	visual-line-fringe-indicators '(nil right-curly-arrow) )

(if (>= emacs-major-version 28)
	(setq use-short-answers t)
	(defalias 'yes-or-no-p 'y-or-n-p))

(when (>= emacs-major-version 28) (setq
	goto-address-mail-face 'default
	shell-kill-buffer-on-exit t))

(when (< emacs-major-version 28)
	(defalias 'show-paren-local-mode 'show-paren-mode))

(when (>= emacs-major-version 29) (setq
	help-enable-variable-value-editing t))

;; files
(setq
	abbrev-file-name			(concat user-emacs-directory "etc/abbrev_defs")
	auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/")
	bookmark-default-file	   	(concat user-emacs-directory "etc/bookmarks")
	eshell-aliases-file			(concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name  		(concat user-emacs-directory "var/eshell/")
	request-storage-directory  	(concat user-emacs-directory "var/request/storage/")
	tramp-auto-save-directory  	(concat user-emacs-directory "var/tramp/auto-save/")
	tramp-persistency-file-name	(concat user-emacs-directory "var/tramp/persistency")
	url-cache-directory			(concat user-emacs-directory "var/url/cache/")
	url-configuration-directory	(concat user-emacs-directory "var/url/configuration/") )

;; custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))

;; backups
(setq
	auto-save-default nil
	backup-by-copying t
	make-backup-files nil)

(unless *w32*
	(require 'backup-each-save)
	(add-hook 'after-save-hook 'backup-each-save))

;; garbage collection
(use-package gcmh
	:config (gcmh-mode 1)
	:diminish)

;; path
(load "init/exec-path")


;; buffers
(load "init/filesandbuffers")

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'emacs-news-view-mode-hook (lambda()
	(form-feed-mode) ))
(add-hook 'help-mode-hook (lambda ()
	(form-feed-mode)
	(goto-address-mode)
	(setq-local font-lock-keywords-only t)))
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; eval-after-loads are run once, before mode hooks
;; mode-hooks execute once for every buffer in which the mode is enabled

(with-eval-after-load 'doc-view-mode
	(define-key doc-view-mode-map (kbd "q") 	'kill-current-buffer) )
(with-eval-after-load 'emacs-news-mode
	(define-key emacs-news-view-mode-map (kbd "<left>")
		(lambda()(interactive)(outline-previous-heading)(recenter-top-bottom)))
	(define-key emacs-news-view-mode-map (kbd "<right>")
		(lambda()(interactive)(outline-next-heading)(recenter-top-bottom))) )
(with-eval-after-load 'help-mode
	(define-key help-mode-map (kbd "q")			'kill-current-buffer)
	(define-key help-mode-map (kbd "<left>")	'help-go-back)
	(define-key help-mode-map (kbd "<right>")	'help-go-forward)
	(define-key help-mode-map (kbd "M-RET")		'goto-address-at-point))
(with-eval-after-load 'Info-mode
	(define-key Info-mode-map (kbd "q")			'kill-current-buffer)
	(define-key Info-mode-map (kbd "<left>" )	'Info-history-back)
	(define-key Info-mode-map (kbd "<right>")	'Info-history-forward))

(easy-menu-add-item  nil '("Buffers") ["Increase text size" text-scale-increase :help "Change text scale"])
(easy-menu-add-item  nil '("Buffers") ["Decrease text size" text-scale-decrease :help "Change text scale"])

;; remove unneeded messages and buffers
(setq inhibit-startup-message t)	; 'About Emacs'
(setq initial-scratch-message nil)	; Makes *scratch* empty
(add-hook 'minibuffer-exit-hook		; Removes *Completions* buffer when done
	(lambda () (let ((buffer "*Completions*")) (and (get-buffer buffer) (kill-buffer buffer)))) )

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ; Don't show *Buffer list*
(add-hook 'window-setup-hook		 ; Show only one active window
	'delete-other-windows)

;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t) ; Dired, etc.
(global-auto-revert-mode 1)

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

(setq enable-recursive-minibuffers t)

;; clean-up old buffers
(midnight-mode +1)


;; frames
;; https://korewanetadesu.com/emacs-on-os-x.html
(when (featurep 'ns)
	(defun ns-raise-emacs ()
	"Raise Emacs."
	(ns-do-applescript "tell application \"Emacs\" to activate"))

	(defun ns-raise-emacs-with-frame (frame)
	"Raise Emacs and select the provided frame."
	(with-selected-frame frame
	(when (display-graphic-p)
	(ns-raise-emacs))))

	(add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
	(when (display-graphic-p) (ns-raise-emacs)))


;; mode line
(use-package doom-modeline
	:ensure t
	:hook	(after-init . doom-modeline-mode)
	:config (setq
		doom-modeline-buffer-modification-icon nil
		doom-modeline-column-zero-based nil
		doom-modeline-enable-word-count t
		doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
		(unless (display-graphic-p) (setq doom-modeline-icon nil))
	(use-package nerd-icons))

(setq
	battery-mode-line-format "%p%% "
	display-time-24hr-format t
	display-time-default-load-average nil
	mode-line-compact nil
	mode-line-position (list mode-line-percent-position " " "(%l,%C)")
	mode-line-right-align-edge 'right-fringe)
(column-number-mode)
(display-battery-mode)
(display-time-mode -1)

;; Startup time
(defun efs/display-startup-time ()
	(message "GNU Emacs %s loaded in %s with %d garbage collections." emacs-version
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
		gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)


;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq
	ido-enable-flex-matching t
	ido-show-dot-for-dired nil)
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil); turn off C-x C-w remapping

;(icomplete-mode) ; IDO for M-x
;(fido-mode) ; makes complete act like IDO mode

(use-package ido-sort-mtime :config	(ido-sort-mtime-mode 1))

(add-to-list 'ido-ignore-buffers "*Messages*")
(add-to-list 'ido-ignore-buffers "*Shell Command Output*")
(add-to-list 'ido-ignore-files ".DS_Store")
(add-to-list 'ido-ignore-files "ido.last")

(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-d")	'ido-dired)

;; Dired
(add-hook 'dired-mode-hook (lambda()(dired-omit-mode 1)))
(with-eval-after-load 'dired
	(require 'dired-x)
	(unless *w32* (setq dired-kill-when-opening-new-dired-buffer t))
	(setq dired-omit-files (concat dired-omit-files
		"\\|^INDEX$\\|-t\\.tex$\\|\\.DS_Store$\\|\\.localized$")
		dired-omit-verbose nil)
	(require 'ls-lisp)
	(setq
		ls-lisp-use-string-collate nil
		ls-lisp-use-insert-directory-program nil
		ls-lisp-ignore-case 't)
	(define-key dired-mode-map (kbd "q")	'kill-dired-buffers)
	(define-key dired-mode-map (kbd "o")	'dired-find-file-ow)
	(defun dired-find-file-ow() (interactive)(dired-find-file-other-window)(delete-other-windows))
	(defalias 'dired-find-file				'dired-find-alternate-file))

;; Ibuffer
;; https://www.emacswiki.org/emacs/IbufferMode
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ; always use Ibuffer
(define-key ibuffer-mode-map (kbd "q")		'kill-current-buffer)
(define-key ibuffer-mode-map (kbd "<up>")	'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>")	'ibuffer-next-line)
(define-key ibuffer-mode-map (kbd "<left>") 'ibuffer-previous-header)
(define-key ibuffer-mode-map (kbd "<right>")'ibuffer-next-header)
(define-key ibuffer-mode-map (kbd "<return>")(lambda()(interactive)(ibuffer-visit-buffer)
	(let ((buffer "*Ibuffer*")) (and (get-buffer buffer) (kill-buffer buffer))) ))
(add-hook 'ibuffer-mode-hook (lambda()
	(ibuffer-switch-to-saved-filter-groups "home")
	(ibuffer-update nil t) ))

(setq
	ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
	ibuffer-saved-filter-groups (quote (("home"
	   	("Dired" (mode . dired-mode) )
		("Emacs" (or
			(name . "^\\*scratch\\*$")
			(name . "^\\*Messages\\*$")
			(name . "\\.el") ))
		("Markdown" (name . "\\.md"))
		("Org" (name . "\\.org"))
		("planner" (or
			(name . "^\\*Calendar\\*$")
			(name . "^diary$")
			(name . "^\\*Org Agenda\\*")))
		;("perl" (mode . cperl-mode))
		;("erc" (mode . erc-mode))
		("Eww"   (mode . eww-mode))
		("gnus" (or
			(mode . message-mode)
			(mode . bbdb-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(name . "^\\.bbdb$")
			(name . "^\\.newsrc-dribble")))
		))))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*Shell Command Output\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp/")


;; calendar
(load "init/calendar")
(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)
(add-hook 'calendar-mode-hook (lambda()
	(local-set-key (kbd "?") (lambda() (interactive)(info "(emacs)Calendar/Diary")
		(delete-other-windows)(calendar-exit 'kill)
		(local-set-key (kbd "q") (lambda() (interactive)(kill-current-buffer)(calendar))) ))
	(local-set-key (kbd "q") (lambda() (interactive)(calendar-exit 'kill)
		(let ((buffer "*wclock*"))(and (get-buffer buffer) (kill-buffer buffer))) ))
	(local-set-key (kbd "w") 'calendar-world-clock)
	(local-set-key (kbd "y") 'calendar-holidays)
	(easy-menu-add-item nil '("Holidays") ["Holidays this year" calendar-holidays :help "Holidays"])
	(easy-menu-add-item nil '("Sun/Moon") ["World clock" calendar-world-clock :help "World clock"])))
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-mode-hook (lambda()
	(local-set-key (kbd "C-c C-q") 'kill-current-buffer) ))
(add-hook 'diary-fancy-display-mode-hook (lambda()
	(local-set-key (kbd "q") 'kill-current-buffer) ))
(add-hook 'special-mode-hook (lambda()
	(local-set-key (kbd "q") 'kill-current-buffer) ))

(setq
	diary-file "~/Documents/diary"
	diary-list-includes-blanks t
	diary-show-holidays-flag nil

	calendar-date-style 'iso
	calendar-mark-holidays-flag t
	calendar-view-holidays-initially-flag nil
	calendar-mark-diary-entries-flag t
	calendar-month-header '(propertize
		(format "%s %d" (calendar-month-name month) year)
		'font-lock-face 'calendar-month-header)
	world-clock-time-format "%9A %2e %9B %R %Z"

	calendar-christian-all-holidays-flag t
	calendar-chinese-all-holidays-flag t
	holiday-general-holidays nil
	holiday-bahai-holidays nil
	;holiday-hebrew-holidays nil
	;holiday-islamic-holidays nil
	)


;; print functions
(load "init/page-dimensions")
(define-key global-map [menu-bar file print] nil)
(bind-key "M-p a"  	'print-to-a5-printer)
(bind-key "M-p r"  	'print-to-receipt-printer)
(bind-key "s-p" 	'print-buffer-or-region)
	(defun print-buffer-or-region () (interactive)
	(print-region (point-min) (point-max)))

(when *mac* (setq
	printer-name "Munbyn_ITPP047"
	lpr-switches '("-o cpi=12 -o lpi=8 -o print-quality=4")
	lpr-page-header-switches '("-t")

	ps-printer-name "Brother_HL_L2370DW"
	ps-lpr-switches '("-o media=a5")
	ps-paper-type 'a5

	font-size 12
	ps-font-family 'Courier
	ps-footer-font-family 'Courier
	ps-print-color-p nil
	ps-print-header nil

	ps-print-footer t
	ps-print-footer-frame nil
	ps-footer-lines 1
	ps-right-footer nil
	ps-left-footer (list (concat
	"{pagenumberstring dup stringwidth pop"
	" 2 div PrintWidth 2 div exch sub 0 rmoveto}"))

	ps-top-margin 42
	ps-bottom-margin 14
	ps-left-margin 28
	ps-right-margin 28))

(load "init/print")


;; Initialize packages
(use-package diminish)

;; (use-package bookmark+
;; 	:quelpa (bookmark+ :fetcher wiki :files (
;; 		"bookmark+.el"
;; 		"bookmark+-mac.el"
;; 		"bookmark+-bmu.el"
;; 		"bookmark+-1.el"
;; 		"bookmark+-key.el"
;; 		"bookmark+-lit.el"
;; 		"bookmark+-doc.el"
;; 		"bookmark+-chg.el"))
;; 	:defer 2)

(use-package elpher
	:config
		(setq elpher-bookmarks-file (concat user-emacs-directory "var/elpher-bookmarks"))
		(easy-menu-add-item  nil '("tools") ["Gopher" elpher :help "Browse Gopherspace"] "Browse the Web...")

		(defun elpher:eww-browse-url (original url &optional new-window) "Handle gemini links."
			(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url) (elpher-go url))
			(t (funcall original url new-window))))
		(advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
		(defun elpher-up () (interactive)(backward-paragraph)(recenter-top-bottom))
		(defun elpher-down () (interactive)(forward-paragraph)(recenter-top-bottom))

		(add-hook 'elpher-mode-hook (lambda () (setq-local
			left-margin-width 10
			gnutls-verify-error nil)
			(set-window-buffer nil (current-buffer))
			(local-set-key (kbd "<left>") 'elpher-back)
			(local-set-key (kbd "<right>") 'elpher-down))))

(require 'eww)
(define-key eww-mode-map (kbd "<left>") 'eww-back-url)
(define-key eww-bookmark-mode-map (kbd "w")	'eww)
(setq
	browse-url-browser-function 'eww-browse-url
	eww-bookmarks-directory (concat user-emacs-directory "etc/")
	shr-use-colors nil
	shr-use-fonts nil
	shr-bullet "• "
	shr-folding-mode t
	eww-search-prefix "https://duckduckgo.com/html?q="
	url-privacy-level '(email agent lastloc)

	shr-indentation 2	; Left-side margin
	shr-width nil)		; Fold text for comfiness
(url-setup-privacy-info)

(use-package ace-link
	:config
		(ace-link-setup-default))

(use-package w3m
	:config
		(setq w3m-bookmark-file (concat user-emacs-directory "etc/w3m-bookmarks.html"))
		(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
		(define-key w3m-mode-map (kbd "<left>") 'w3m-view-previous-page))

(use-package flycheck)

(use-package free-keys :defer t)

(use-package google-this
	:config
		(google-this-mode)
		(which-key-add-key-based-replacements "C-c /" "google-this")
	:diminish)

(use-package hl-todo
    :hook
		(prog-mode . hl-todo-mode)
		(emacs-lisp-mode . hl-todo-mode)
    :config (setq hl-todo-keyword-faces `(
		("TODO"       warning bold)
		("FIXME"      error bold)
		("HACK"       font-lock-constant-face bold)
		("REVIEW"     font-lock-keyword-face bold)
		("NOTE"       success bold)
		("DEPRECATED" font-lock-doc-face bold) )))

(use-package lorem-ipsum
	:config
		(setq-default lorem-ipsum-sentence-separator " ")
		(easy-menu-add-item  nil '("edit") ["Lorem-ipsum" lorem-ipsum-insert-paragraphs :help "Insert..."]))

(use-package form-feed ; ^L
	:config
		(global-form-feed-mode)
	:diminish)

(use-package ssh)

(use-package sudo-edit)

(use-package typo) ; minor mode for typographic editing

(use-package visible-mark)

(use-package which-key
	:config
		(which-key-mode)
	:diminish)

;; Diminish built-in modes
(diminish 'abbrev-mode)
(diminish 'eldoc-mode "Ed")
(diminish 'visual-line-mode "VLM")


;; Configure specific machines
(when *natasha* (setq
	;browse-url-browser-function 'w3m-browse-url
	browse-url-secondary-browser-function 'browse-url-generic
	;browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox"
	browse-url-generic-program "/Applications/Waterfox.app/Contents/MacOS/waterfox")

	;; Mail / News
	(require 'rmail) (setq
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
	rmail-default-file			(concat rmail-secondary-file-directory "XMAIL")
	rmail-file-name				(concat rmail-secondary-file-directory "RMAIL"))
	(add-hook 'rmail-show-message-hook 'goto-address-mode)
	(add-hook 'rmail-quit-hook 'kill-current-buffer)

	(add-hook 'message-mode-hook (lambda()
	(local-set-key (kbd "A-<return>") 'message-send-and-exit) ))

	(use-package elfeed
		:config	(setq
			elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/")
	   		elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/")
			elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el")
			elfeed-show-truncacte-long-urls t
			elfeed-sort-order 'ascending
			elfeed-use-curl t)

		(eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
		(easy-menu-add-item  nil '("tools") ["Read Web Feeds" elfeed :help "Read RSS feeds"] "Read Mail")

		(bind-key "C-c f" 'elfeed)
		(define-key elfeed-search-mode-map (kbd "q") (lambda()(interactive)(kill-current-buffer)
			(let ((buffer "*elfeed-log*")) (and (get-buffer buffer) (kill-buffer buffer)))
			(let ((buffer "*sent mail to cn*")) (and (get-buffer buffer) (kill-buffer buffer)))))
		(define-key elfeed-search-mode-map (kbd "/") 'elfeed-search-live-filter)
		(define-key elfeed-search-mode-map (kbd "s") nil)
		(define-key elfeed-search-mode-map (kbd "m") 'elfeed-mail-todo)
		(define-key elfeed-show-mode-map (kbd "TAB") 'shr-next-link)
		(define-key elfeed-show-mode-map (kbd "SPC") 'scroll-up-half)
		(define-key elfeed-show-mode-map (kbd "A-<up>"  ) 'backward-paragraph)
		(define-key elfeed-show-mode-map (kbd "A-<down>") 'forward-paragraph)

		;; (use-package elfeed-org
		;; 	:config (setq
		;; 		rmh-elfeed-org-files (list (concat user-emacs-directory "etc/rc/elfeed.org")))
		;; 	(elfeed-org))

		(use-package elfeed-summary)

		(load "rc/feeds" 'noerror 'nomessage)	; feeds
		(load "init/elfeed"))					; routines

	;; Stack Exchange
	(use-package sx
		:config
			(bind-keys
			:prefix "C-c S"
			:prefix-map my-sx-map
			:prefix-docstring "Global keymap for SX."
				("q" . sx-tab-all-questions)
				("i" . sx-inbox)
				("o" . sx-open-link)
				("u" . sx-tab-unanswered-my-tags)
				("a" . sx-ask)
				("s" . sx-search))
			(setq sx-cache-directory (concat user-emacs-directory "var/sx")))
	)

(when *mac*
	;(load "init/deft")	; note functions (bound to <f7>)
	;(load "init/sn")	; simplenote	 (bound to <f8>)
	)

(when *gnu*
	(setq	browse-url-secondary-browser-function 'browse-url-generic
			browse-url-generic-program "firefox-esr"))

(unless *w32* (use-package pdf-tools
	:load-path  "site-lisp/pdf-tools/lisp"
	:magic ("%PDF" . pdf-view-mode)
	:config (pdf-tools-install :no-query) ))


;; Emacs Text and Markdown modes
(add-hook 'text-mode-hook (lambda ()
	(abbrev-mode)
	(goto-address-mode)
	(visual-line-mode)))
(add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)

(use-package visual-fill-column
	:config (setq visual-fill-column-fringes-outside-margins nil)

	(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
	(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
	(add-hook 'prog-mode-hook (lambda() (visual-fill-column-mode -1))))

(use-package adaptive-wrap)
	(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;; fix html-mode
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(delete '("\\.html$" . text-mode) auto-mode-alist)
(add-hook 'html-mode-hook (lambda ()
	(visual-line-mode -1)
	(visual-fill-column-mode -1)
	(toggle-truncate-lines 1)))

(use-package markdown-mode
	:mode
		(("README\\.md\\'" . gfm-mode)
		("\\.md\\'" . markdown-mode)
		("\\.markdown\\'" . markdown-mode)
		("\\.gmi\\'" . markdown-mode))
	:commands (markdown-mode gfm-mode)
	:init (setq markdown-hide-urls t)
	:config (setq
		markdown-command "multimarkdown"
		markdown-enable-prefix-prompts nil
		markdown-italic-underscore t
		markdown-unordered-list-item-prefix "* ")
		(add-to-list 'markdown-uri-types "gemini"))

(use-package markdown-preview-mode)

(load "init/text") ; text functions

(add-hook 'prog-mode-hook (lambda()
	(setq show-trailing-whitespace t)
	(abbrev-mode)
	(when (not (memq major-mode
		(list 'lisp-interaction-mode)))
		(display-line-numbers-mode))
	(goto-address-prog-mode)
	(prettify-symbols-mode)
	(show-paren-local-mode)))


;; spell checking
(setq
	flyspell-doublon-as-error-flag nil
	flyspell-issue-welcome-flag nil
	flyspell-issue-message-flag nil
	ispell-dictionary "canadian"
	ispell-extra-args '("--sug-mode=ultra")
	ispell-list-command "--list"	; correct command
	ispell-program-name "aspell"	; spell checker
	ispell-silently-savep t)   		; save personal list automatically

(unless *w32*
	(dolist (hook '(text-mode-hook markdown-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
	(add-hook 'prog-mode-hook 'flyspell-prog-mode))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))

(use-package flyspell-lazy
	:after flyspell
	:config (setq
		flyspell-lazy-idle-seconds 1
		flyspell-lazy-window-idle-seconds 3)
		(flyspell-lazy-mode 1))

(eval-after-load "flyspell" '(progn
	(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)))

(defun my/list-packages ()
	(interactive)
	(unless (ispell-process-status) (ispell-start-process))
	(list-packages))


;; Org-mode
(use-package org
	:config (setq
	org-directory "~/Documents/org"
	org-agenda-file (concat org-directory "/daily.org")
	org-agenda-files (list org-agenda-file)
	org-agenda-text-search-extra-files '(agenda-archives)
	org-default-notes-file (concat org-directory "/notes.org")

	org-startup-folded 'content			; folded children content all
	org-startup-shrink-all-tables t

	org-catch-invisible-edits 'smart
	org-ctrl-k-protect-subtree t
	org-ellipsis "."
	org-enable-priority-commands nil
	org-export-preserve-breaks t
	org-export-with-toc nil
	org-footnote-auto-adjust t
	org-log-done t						; 'CLOSED' logging
	org-log-repeat nil
	org-log-state-notes-into-drawer nil
	org-pretty-entities t
	org-special-ctrl-a/e t
	org-support-shift-select t

	org-agenda-include-diary nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-start-on-weekday nil
	org-agenda-todo-ignore-deadlines t
	org-agenda-todo-ignore-scheduled t

	org-tags-exclude-from-inheritance '("PROJECT")

	org-todo-keywords '((sequence "TODO" "DONE"))

	org-todo-keyword-faces '(
	("INPROGRESS" . (:foreground "blue" :weight bold)) ) ; add in-progress keyword

	org-emphasis-alist '(
	("*" bold)
	("**" bold)
	("/" italic)
	("_" italic)
	("=" (:background "maroon" :foreground "white"))
	("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t)))

	org-agenda-custom-commands '(
	("P" "Project List" (
		(tags "PROJECT")))
	("O" "Office" (
		(agenda)
		(tags-todo "OFFICE")))
	("W" "Weekly Plan" (
		(agenda)
		(todo "TODO")
		(tags "PROJECT")))
	("H" "Home NA Lists" (
		(agenda)
		(tags-todo "HOME")
		(tags-todo "COMPUTER"))))

	org-capture-templates '(
	("c" "Cookbook" entry (file "~/Documents/org/cookbook.org")
   		"%(org-chef-get-recipe-from-url)" :empty-lines 1)
	("m" "Manual Cookbook" entry (file "~/Documents/org/cookbook.org")
		"* %^{Recipe title: }\n
		:PROPERTIES:\n
		:source-url:\n
		:servings:\n
		:prep-time:\n
		:cook-time:\n
		:ready-in:\n
		:END:\n
		** Ingredients\n
		%?\n
		** Directions\n\n")

	;; https://benadha.com/notes/how-i-manage-my-reading-list-with-org-mode/
	("i" "📥 Inbox" entry (file "~/Documents/org/inbox.org")
		"* %?\n  %i\n" :prepend t)
	("j" "📔 Journal" entry (file+datetree "~/Documents/org/journal.org")
		"* %? %^G\nEntered on %U\n  %i\n")
	("b" "📑 Bookmark" entry (file "~/Documents/org/bookmarks.org")
		"* %? %^g\n  %i\n" :prepend t)
	("s" "🛒 Shopping List" entry (file+headline "~/Documents/org/shoppinglist.org" "SHOPPING LIST")
		"* TODO %?\n  %i\n" :prepend t)

	;; https://github.com/rexim/org-cliplink
	("K" "Cliplink capture task" entry (file "")
		"* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)
	)) ; set

	(define-key org-mode-map (kbd "C-<") 'org-backward-heading-same-level)
	(define-key org-mode-map (kbd "C->") 'org-forward-heading-same-level)
	(define-key org-mode-map (kbd "A-<left>" ) 'outline-up-heading)
	(define-key org-mode-map (kbd "A-<right>") (lambda()(interactive)(org-end-of-subtree)))

	(add-hook 'org-agenda-finalize-hook 'delete-other-windows)

	(add-hook 'org-mode-hook (lambda ()
		(org-autolist-mode)
		(prettify-symbols-mode)
		(visual-fill-column-mode -1) ))
	) ; use-package org

(use-package org-autolist ; pressing "Return" will insert a new list item automatically
	:diminish "AL")

(use-package org-chef :ensure t)

(use-package org-cliplink) ; insert org-mode links from the clipboard

(use-package org-d20)

;; (use-package org-modern
;; 	:config
;; 	(with-eval-after-load 'org (global-org-modern-mode)))

(when *mac* (use-package org-mac-link))

(when *natasha* (use-package org-roam
	:ensure t
	:custom
		(org-roam-directory (file-truename (concat org-directory "/Roam/")))
	:bind (
		("C-c n l" . org-roam-buffer-toggle)
		("C-c n f" . org-roam-node-find)
		("C-c n g" . org-roam-graph)
		("C-c n i" . org-roam-node-insert)
		("C-c n c" . org-roam-capture)
		;; Dailies
		("C-c n j" . org-roam-dailies-capture-today))
	:config
		(which-key-add-key-based-replacements "C-c n" "org-roam")
		(org-roam-setup)
		(org-roam-db-autosync-mode)))

(load "init/org") ; org-mode functions

;; fix table.el error
(defun myfunc/check_table_p (oldfunc) (funcall oldfunc t))
(advice-add 'org-at-table-p :around 'myfunc/check_table_p)


;; sundry
(load "init/misc")
(load "init/scripts" 'noerror)

(load "init/pdfexport")
(eval-after-load 'latex-mode
	'(define-key latex-mode-map (kbd "C-c r") 'latex-compile-and-update-other-buffer))
(eval-after-load 'markdown-mode
	'(define-key markdown-mode-map (kbd "C-c r") 'md-compile-and-update-other-buffer))
(define-key org-mode-map (kbd "C-c o r") 'org-compile-latex-and-update-other-buffer)


;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up		<next> is fn-down

(global-set-key (kbd "<home>"   ) 'move-beginning-of-line)
(global-set-key (kbd "<end>"    ) 'move-end-of-line)
;; <prior>		'scroll-down-command
;; <next>		'scroll-up-command

(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) 'end-of-buffer)
(global-set-key (kbd "C-<prior>") 'scroll-down-line)
(global-set-key (kbd "C-<next>" ) 'scroll-up-line)

;; M-<home>		'beginning-of-buffer-other-window
;; M-<end>		'end-of-buffer-other-window
;; M-<prior>	'scroll-other-window-down
;; M-<next>		'scroll-other-window

(global-set-key (kbd "M-<left>") (lambda()(interactive)(backward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<right>")(lambda()(interactive)(forward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<up>")   (lambda()(interactive)(backward-page)(recenter-top-bottom)))
(global-set-key (kbd "M-<down>") (lambda()(interactive)(forward-page) (recenter-top-bottom)))

(global-unset-key (kbd "s-<left>" ))
(global-unset-key (kbd "s-<right>"))
(global-unset-key (kbd "s-<up>"   ))
(global-unset-key (kbd "s-<down>" ))

(global-set-key (kbd "<swipe-left>")  'mac-next-buffer)
(global-set-key (kbd "<swipe-right>") 'mac-previous-buffer)

;; scroll settings
(setq
	auto-window-vscroll nil
	scroll-conservatively 10000
	scroll-margin 0
	scroll-preserve-screen-position 1
	scroll-step 0)
(global-set-key (kbd "C-<") 'scroll-left)
(global-set-key (kbd "C->") 'scroll-right)

;; half-scroll
(defun window-half-height ()(max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-up-half ()	(interactive) (scroll-up (window-half-height)))
(defun scroll-down-half ()	(interactive) (scroll-down (window-half-height)))
(global-set-key [next] 'scroll-up-half)
(global-set-key [prior] 'scroll-down-half)
(global-set-key (kbd "A-<down>") 'scroll-up-half)
(global-set-key (kbd "A-<up>") 'scroll-down-half)

;; mouse
;; https://github.com/purcell/disable-mouse
(use-package disable-mouse)
(global-disable-mouse-mode)
;(mouse-avoidance-mode banish)


;; window navigation
(when (fboundp 'windmove-default-keybindings)
	(global-set-key (kbd "s-<up>")		'windmove-up)
	(global-set-key (kbd "s-<down>")	'windmove-down)
	(global-set-key (kbd "s-<right>")	'windmove-right)
	(global-set-key (kbd "s-<left>")	'windmove-left))

; tweaking window sizes
(global-set-key (kbd "C-{") 'shrink-window-horizontally)
(global-set-key (kbd "C-}") 'enlarge-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)


;; alternate keys
(bind-key "C-S-k"	'kill-whole-line)

(global-set-key (kbd "C-s")		'isearch-forward-regexp)
(global-set-key (kbd "C-r")		'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")	'isearch-forward)
(global-set-key (kbd "C-M-r")	'isearch-backward)

(global-set-key (kbd "<f12>")	'list-buffers)
(global-set-key (kbd "TAB")		'self-insert-command)

(global-set-key (kbd "A-<return>")(kbd "M-<return>"))

;; avoid accidental exits
;(global-unset-key (kbd "C-x C-c"))
;(global-set-key (kbd "C-x C-c c") 'save-buffers-kill-terminal)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Darwin overrides
(when *mac*
	(global-set-key   (kbd "s-o")	'find-file)
	(global-set-key   (kbd "s-S")	'write-file)

	;; minimize, new frame, quit, set-font
	(dolist (key '("s-m" "s-n" "s-q" "s-t"))
	(global-unset-key (kbd key)))

	(when (display-graphic-p)
	(dolist (key '("<f10>" "S-<f10>" "C-<f10>" "M-<f10>"))
	(global-unset-key (kbd key))) ))


;; Disabled keys
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)	; C-x C-u
(put 'downcase-region 'disabled nil); C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'suspend-frame 'disabled t)	; C-z / C-x C-z


;; Shortcuts

(bind-key "<f5>"	'toggle-fill-column)
(bind-key "<f6>"	'toggle-fill-column-center)
(bind-key "<f7>"	'ispell-word)
(bind-key "<f8>"	'list-bookmarks)

(bind-key "M-<f1>" 'my/emacs-help)
(bind-key "M-<f2>" 'describe-personal-keybindings)
(bind-key "M-<f3>" 'shortdoc)

(bind-key "M-Q"		'unfill-paragraph)

(bind-key "C-M-;"	'eval-r) (defun eval-r (b e) (interactive "r")(eval-region b e)(deactivate-mark))
(bind-key "C-M-Y"	'undo-yank)

(bind-key "C-c !"	'shell)

(bind-key "C-c a a"	'org-agenda) (when *mac*
(bind-key "C-c a d"	'daily-agenda) (defun daily-agenda () (interactive)(find-file org-agenda-file)))
(which-key-add-key-based-replacements "C-c a" "org agenda")

(bind-key "C-c b m" 'new-markdown-buffer)
(bind-key "C-c b n" 'new-empty-buffer)
(bind-key "C-c b o" 'new-org-buffer)
(bind-key "C-c b s" 'scratch-buffer)
(which-key-add-key-based-replacements "C-c b" "buffers")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC" 'display-current-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-add-key-based-replacements "C-c d" "dates")

(bind-key "C-c g"	'elpher-show-bookmarks) ; gopher / gemini

(bind-key "C-c m"	'menu-bar-read-mail)
;(bind-key "C-c n"	'newsticker-show-news)

(bind-key "C-c o a" 'org-archive-subtree-default)
(bind-key "C-c o c"	'org-capture)
(bind-key "C-c o k" 'org-cliplink)
(bind-key "C-c o l"	'org-store-link)
(bind-key "C-c o t" 'org-toggle-link-display)
(which-key-add-key-based-replacements "C-c o" "org")

(bind-key "C-c p"	'markdown-preview-file)
(bind-key "C-c s"	'dictionary-search)
(bind-key "C-c w"	'eww-list-bookmarks) ; www

(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x d" 'delete-duplicate-lines)
(bind-key "C-c x f" 'toggle-fill-column)
(bind-key "C-c x l" 'lorem-ipsum-insert-paragraphs)
(bind-key "C-c x n"	'number-paragraphs)
(bind-key "C-c x q"	'replace-smart-quotes)
(bind-key "C-c x t" 'delete-trailing-whitespace)
(bind-key "C-c x w" 'delete-whitespace-rectangle)
(which-key-add-key-based-replacements "C-c x" "text")

(global-set-key (kbd "C-c 8 c") (kbd "✓"))
(global-set-key (kbd "C-c 8 n") (kbd "№"))
(global-set-key (kbd "C-c 8 p") (kbd "¶"))
(which-key-add-key-based-replacements "C-c 8" "key translations")

(bind-key "C-c C-r" 'sudo-edit)

;; Ctrl-x (buffer functions)
(bind-key "C-x c" 'kill-current-buffer)

(bind-key "C-x x k"	'kill-other-buffers)
(bind-key "C-x x r"	'rename-file-and-buffer)
(bind-key "C-x x v" 'view-text-file-as-info-manual)
(bind-key "C-x x w" 'preview-html)(defun preview-html()(interactive)(shr-render-buffer (current-buffer)))

(which-key-add-key-based-replacements "C-x 8" "key translations")
(which-key-add-key-based-replacements "C-x 8 e" "emojis")

;; add Hyper- keys (C-M-s-?) to terminal frames (iTerm2)
(add-hook 'server-after-make-frame-hook (lambda() (interactive)
	(unless (display-graphic-p) (cl-loop for char from ?a to ?z do
	(define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "H-%c" char)))))))


;; Aliases
(defalias 'di 'daily-info)
(defalias 'doe 'toggle-debug-on-error)
(defalias 'flv 'add-file-local-variable)
(defalias 'cr 'customize-rogue)
(defalias 'la 'list-abbrevs)
(defalias 'lc 'list-colors-display)
(defalias 'lp 'my/list-packages)
(defalias 'recs 'recover-session)
(defalias 'which-key-alias 'which-key-add-key-based-replacements)

(defalias 'arm 'auto-revert-mode)
(defalias 'art 'auto-revert-tail-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'fly 'flyspell-mode)
(defalias 'fci 'display-fill-column-indicator-mode)
(defalias 'fm 'fundamental-mode)
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
(when *w32*
	(setq default-directory "c:/Users/henrypa/OneDrive - City of Ottawa/")
	(bind-key "C-c a o"	'office.org)
	(defun office.org ()(interactive)(find-file (concat default-directory "!.org"))) )

; LocalWords:  LocalWords Inconsolata natasha elfeed rc keymap SX tls
; LocalWords:  el icomplete init pdfexport filesandbuffers RSS Lorem
; LocalWords:  Gopherspace ipsum Monospace Consolas MidnightBlue sexp
; LocalWords:  bashrc defun modeline waterfox comfiness ibuffer Eww
; LocalWords:  RET quelpa café remotehost fido hebrew islamic henrypa
; LocalWords:  melpa vers defs eshell persistency url ido Ibuffer
