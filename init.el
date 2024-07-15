;;; Emacs configuration / cpjh

;; Initialize terminal
(delete-selection-mode 1)
(electric-indent-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)

(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst system-short-name (car (split-string (system-name) "\\.")) "Hostname of local machine.")
(defconst *bullwinkle*		(string-equal system-short-name "bullwinkle"))
(defconst *natasha*			(string-equal system-short-name "natasha"))

(when *mac* (add-to-list 'default-frame-alist '(font . "Inconsolata 21"))
	(setq
		mac-function-modifier nil
		mac-control-modifier 'control	; Control
		mac-option-modifier 'meta		; Meta
		mac-command-modifier 'super		; Super
		mac-right-command-modifier 'alt	; Alt
		mac-right-option-modifier nil)	; pass-thru

	(global-set-key (kbd "s-c") 'ns-copy-including-secondary)	; ⌘-c = Copy
	(global-set-key (kbd "s-x") 'kill-region) 					; ⌘-x = Cut
	(global-set-key (kbd "s-v") 'yank)							; ⌘-v = Paste
	(global-set-key (kbd "s-y") 'ns-paste-secondary)

	(global-set-key (kbd "s-a") 'mark-whole-buffer)
	(global-set-key (kbd "s-E") 'edit-abbrevs)
	(global-set-key (kbd "s-h") 'ns-do-hide-emacs)
	(global-set-key (kbd "s-k") 'kill-current-buffer)
	(global-set-key (kbd "s-l") 'goto-line)
	(global-set-key (kbd "s-o")	'find-file)
	(global-set-key (kbd "s-S")	'write-file)
	(global-set-key (kbd "s-s") 'save-buffer)
	(global-set-key (kbd "s-u") 'revert-buffer)
	(global-set-key (kbd "s-w") 'delete-frame)
	(global-set-key (kbd "s-z") 'undo)

	(global-set-key (kbd "s-1")(kbd "C-x 1"))

	(dolist (key '("s-C" "s-D" "s-d" "s-e" "s-F" "s-f" "s-g" "s-j"
		"s-L" "s-M" "s-m" "s-n" "s-p" "s-q" "s-t"))
		(global-unset-key (kbd key)))

	(toggle-frame-maximized))

(when *gnu* (add-to-list 'default-frame-alist '(font . "Monospace 17")) )

(when *w32* (add-to-list 'default-frame-alist '(font . "Consolas 12"))
	(setq
		w32-lwindow-modifier 'super
		w32-pass-lwindow-to-system nil
		w32-apps-modifier 'hyper)
	(message "Running on Windows."))

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
	goto-address-mail-face 'default
	help-clean-buttons t
	help-enable-variable-value-editing t
	ibuffer-expert t
	inhibit-compacting-font-caches t
	inhibit-default-init t
	isearch-allow-scroll t
	kill-read-only-ok t
	kill-ring-max 512
	kill-whole-line t
	ls-lisp-use-localized-time-format t
	Man-notify-method 'pushy
	mark-ring-max most-positive-fixnum
	max-lisp-eval-depth 65536
	ns-use-native-fullscreen t
	package-archive-column-width 1
	pop-up-windows nil
	;pop-up-frames nil
	recenter-positions '(top) ; top middle bottom
	require-final-newline nil
	resize-mini-windows t
	revert-buffer-quick-short-answers t
	ring-bell-function 'ignore
	save-abbrevs 'silent
	search-default-mode 'char-fold-to-regexp ; cafe = café
	sentence-end-double-space nil
	shell-kill-buffer-on-exit t
	show-paren-style 'parenthesis
	trash-directory "~/.Trash"
	use-dialog-box nil
	use-file-dialog nil
	use-short-answers t
	view-read-only nil				; turn on view mode when buffer is read-only
	visual-line-fringe-indicators '(nil right-curly-arrow))

(when (< emacs-major-version 28) (defalias 'show-paren-local-mode 'show-paren-mode))

;; files
(setq
	abbrev-file-name			(concat user-emacs-directory "etc/abbrev_defs")
	auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/")
	bookmark-default-file	   	(concat user-emacs-directory "etc/bookmarks")
	eshell-aliases-file			(concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name  		(concat user-emacs-directory "var/eshell/")
	multisession-directory		(concat user-emacs-directory "var/multisession")
	nsm-settings-file			(concat user-emacs-directory "var/network-security.data")
	request-storage-directory  	(concat user-emacs-directory "var/request/storage/")

	transient-history-file		(concat user-emacs-directory "var/transient/history.el")
	transient-levels-file		(concat user-emacs-directory "var/transient/levels.el")
	transient-values-file		(concat user-emacs-directory "var/transient/values.el")

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

;; path
(if (boundp 'emacs-edition) (message "Running '%s'." emacs-edition))
(if *mac* (use-package exec-path-from-shell
	:config (exec-path-from-shell-initialize)))

;; garbage collection
(use-package gcmh
	:config (gcmh-mode 1)
	:diminish)


;; buffers
(load "init/filesandbuffers")
(require 'formfeed-hline)
(formfeed-hline-mode)

(add-hook 'before-save-hook 'time-stamp)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'help-mode-hook (lambda() (setq-local font-lock-keywords-only t) (goto-address-mode)))
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; eval-after-loads are run once, before mode hooks
;; mode-hooks execute once for every buffer in which the mode is enabled

(with-eval-after-load 'doc-view-mode
	(define-key doc-view-mode-map (kbd "q")		'kill-current-buffer))
(with-eval-after-load 'emacs-news-mode
	(define-key emacs-news-view-mode-map (kbd "<left>")
	(lambda()(interactive)(outline-previous-heading)(recenter-top-bottom)))
	(define-key emacs-news-view-mode-map (kbd "<right>")
	(lambda()(interactive)(outline-next-heading)(recenter-top-bottom))))
(with-eval-after-load 'help-mode
	(define-key help-mode-map (kbd "q")			'kill-current-buffer)
	(define-key help-mode-map (kbd "A-<left>")	'help-go-back)
	(define-key help-mode-map (kbd "A-<right>")	'help-go-forward)
	(define-key help-mode-map (kbd "M-RET")		'goto-address-at-point))
(with-eval-after-load 'Info-mode
	(define-key Info-mode-map (kbd "q")			'kill-current-buffer)
	(define-key Info-mode-map (kbd "A-<left>" )	'Info-history-back)
	(define-key Info-mode-map (kbd "A-<right>")	'Info-history-forward))
(with-eval-after-load 'view
	(define-key view-mode-map (kbd "q")			'View-kill-and-leave))

;; remove unneeded messages and buffers
(setq inhibit-startup-message t)	; 'About Emacs'
(add-hook 'minibuffer-exit-hook		; Removes *Completions* buffer when done
	(lambda() (let ((buffer "*Completions*")) (and (get-buffer buffer) (kill-buffer buffer)))) )

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ; Don't show *Buffer list*
(add-hook 'window-setup-hook		 ; Show only one active window
	'delete-other-windows)

;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t) ; Dired, etc.

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

(setq enable-recursive-minibuffers t)

;; *scratch*
(setq initial-scratch-message nil)	; Makes *scratch* empty

;; Tramp
(setq
	tramp-default-method "ssh"
	tramp-syntax 'simplified		; C-x C-f /remotehost:filename

	tramp-auto-save-directory  	(concat user-emacs-directory "var/tramp/auto-save/")
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
(when (featurep 'ns)
	(defun ns-raise-emacs ()
	"Raise Emacs."
		(ns-do-applescript "tell application \"Emacs\" to activate"))

	(defun ns-raise-emacs-with-frame (frame)
	"Raise Emacs and select the provided frame."
		(with-selected-frame frame
		(when (display-graphic-p)
			(ns-raise-emacs)
			(toggle-frame-maximized))))
	(add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame))

;; start Emacs server
(when *mac* (use-package mac-pseudo-daemon
	:config	(mac-pseudo-daemon-mode)
			(server-start))
	(if (boundp 'server-process) (message "Server started.")))

;; add Hyper- keys (C-M-s-…) to terminal frames (iTerm2)
(add-hook 'server-after-make-frame-hook (lambda()
	(unless (display-graphic-p) (cl-loop for char from ?a to ?z do
	(define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "H-%c" char)))))))


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
	ido-save-directory-list-file (concat user-emacs-directory "var/ido.last")
	ido-enable-flex-matching t
	ido-show-dot-for-dired nil)
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil); turn off C-x C-w remapping

;(icomplete-mode) ; IDO for M-x
;(fido-mode) ; makes complete act like IDO mode

(use-package ido-sort-mtime :config	(ido-sort-mtime-mode 1))

(add-to-list 'ido-ignore-buffers "*Messages*")
(add-to-list 'ido-ignore-buffers "*Shell Command Output*")
(add-to-list 'ido-ignore-buffers "^*tramp/")
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
		("Planner" (or
			(mode . calendar-mode)
			(mode . diary-mode)
			(mode . diary-fancy-display-mode)
			(name . "^\\*daily-info\\*")
			(name . "^\\*Org Agenda\\*")
			(name . "^calendar@*")))
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
			(name . "^\\.newsrc-dribble"))) ))))

(define-key ibuffer-mode-map (kbd "q")		'kill-current-buffer)
(define-key ibuffer-mode-map (kbd "<up>")	'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>")	'ibuffer-next-line)
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


;; calendar
(load "init/calendar")
(setq
	diary-file "~/Documents/diary"

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

(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)
(add-hook 'calendar-mode-hook (lambda()
	(local-set-key (kbd "m") nil) ; don't allow marking of diary entries
	(local-set-key (kbd "?") (lambda() (interactive)(info "(emacs)Calendar/Diary")
		(delete-other-windows)(calendar-exit 'kill)
		(local-set-key (kbd "q") (lambda() (interactive)(kill-current-buffer)(calendar))) ))
	(local-set-key (kbd "q") (lambda() (interactive)(calendar-exit 'kill)
		(let ((buffer "*wclock*"))(and (get-buffer buffer) (kill-buffer buffer))) ))
	(local-set-key (kbd "w") 'calendar-world-clock)
	(local-set-key (kbd "y") 'calendar-holidays)
	(easy-menu-add-item nil '("Holidays") ["Holidays this year" calendar-holidays :help "Holidays"])
	(easy-menu-add-item nil '("Sun/Moon") ["World clock" calendar-world-clock :help "World clock"])))

(advice-add 'diary-fancy-display :after (lambda() (view-mode -1)))
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-mode-hook (lambda()
	(local-set-key (kbd "C-c C-q") 'kill-current-buffer) ))
(add-hook 'diary-fancy-display-mode-hook (lambda()
	(local-set-key (kbd "q") 'kill-current-buffer)
	(alt-clean-equal-signs)))

(add-hook 'special-mode-hook (lambda()
	(local-set-key (kbd "q") 'kill-current-buffer) ))


;; print functions
(load "init/page-dimensions")
(define-key global-map [menu-bar file print] nil)
(setq lpr-page-header-switches '("-t"))

(bind-key "M-p f a" 'fill-to-a5-printer)
(bind-key "M-p f r"	'fill-to-receipt-printer)

(bind-key "M-p p" 	'print-buffer-or-region)
(load "init/print")


;; Initialize packages
(use-package diminish)

(use-package which-key
	:config
		(which-key-mode)
		(defalias 'which-key-alias 'which-key-add-key-based-replacements)
	:diminish)

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

		(define-key elpher-mode-map (kbd "A-<left>") 'elpher-back)
		(define-key elpher-mode-map (kbd "A-<right>") 'elpher-down)

		(add-hook 'elpher-mode-hook (lambda() (setq-local
			left-margin-width 10
			gnutls-verify-error nil)
			(set-window-buffer nil (current-buffer)))))

(require 'eww)
(setq
	browse-url-browser-function 'eww-browse-url
	eww-bookmarks-directory (concat user-emacs-directory "etc/")
	eww-auto-rename-buffer t
	shr-use-colors nil
	shr-use-fonts nil
	shr-bullet "• "
	shr-folding-mode t
	eww-search-prefix "https://duckduckgo.com/html?q="
	url-privacy-level '(email agent lastloc)

	shr-indentation 2	; Left-side margin
	shr-width nil)		; Fold text for comfiness

(define-key eww-mode-map (kbd "A-<left>") 'eww-back-url)
(define-key eww-mode-map (kbd "A-<right>") 'eww-forward-url)
(define-key eww-bookmark-mode-map (kbd "w")	'eww)

(url-setup-privacy-info)
(add-hook 'eww-after-render-hook 'eww-readable) ;; default to 'readable-mode'

(use-package ace-link :config (ace-link-setup-default))

(use-package w3m
	:config
		(setq w3m-bookmark-file (concat user-emacs-directory "etc/w3m-bookmarks.html"))
		(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
	:bind (:map w3m-mode-map ("<left>" . w3m-view-previous-page)))

(use-package flycheck)

(use-package free-keys :defer t)

(use-package google-this
	:config
		(google-this-mode)
		(which-key-alias "C-c /" "google-this")
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

(use-package nov ; Read ePub files
	:init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	:config (setq nov-save-place-file (concat user-emacs-directory "var/nov-places")))

(use-package ssh)

(use-package typo) ; minor mode for typographic editing

(use-package visible-mark)

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
	rmail-default-file				(concat rmail-secondary-file-directory "XMAIL")
	rmail-file-name					(concat rmail-secondary-file-directory "RMAIL"))
	(add-hook 'rmail-show-message-hook 'goto-address-mode)
	(add-hook 'rmail-quit-hook 'kill-current-buffer)

	(add-hook 'message-mode-hook (lambda()
	(local-set-key (kbd "A-<return>") 'message-send-and-exit) ))

	(use-package elfeed
		:config	(setq
			elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/")
	   		elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/")
			elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el")
			elfeed-show-truncate-long-urls nil
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
		;; (define-key elfeed-show-mode-map (kbd "A-<up>"  ) 'backward-paragraph)
		;; (define-key elfeed-show-mode-map (kbd "A-<down>") 'forward-paragraph)

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


;; Emacs Text, Prog, and Markdown modes
(add-hook 'text-mode-hook (lambda()
	(abbrev-mode)
	(goto-address-mode)
	(visual-line-mode)))
(add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)
(define-key text-mode-map (kbd "C-M-i") nil)

(use-package visual-fill-column :config
	(setq visual-fill-column-fringes-outside-margins nil)
	(advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
	(add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package adaptive-wrap)
	(add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

;; prog-mode
(add-hook 'prog-mode-hook (lambda()
	(setq show-trailing-whitespace t)
	(abbrev-mode)
	(when (not (equal major-mode 'lisp-interaction-mode)) ;(memq major-mode (list 'lisp-interaction-mode))
		(display-line-numbers-mode))
	(goto-address-prog-mode)
	(prettify-symbols-mode)
	(show-paren-local-mode)
	(visual-fill-column-mode -1)))

;; fix html-mode
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(delete '("\\.html$" . text-mode) auto-mode-alist)
(add-hook 'html-mode-hook (lambda()
	(visual-line-mode -1)
	(visual-fill-column-mode -1)
	(toggle-truncate-lines 1)))

;; Markdown
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


;; spell checking
(setq
	flyspell-doublon-as-error-flag nil
	flyspell-issue-welcome-flag nil
	flyspell-issue-message-flag nil
	flyspell-use-meta-tab nil
	ispell-dictionary "canadian"
	ispell-extra-args '("--sug-mode=ultra")
	ispell-list-command "--list"	; correct command
	ispell-program-name "aspell"	; spell checker
	ispell-silently-savep t)   		; save personal list automatically

(with-eval-after-load 'flyspell (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word))

(unless *w32*
	(dolist (hook '(text-mode-hook markdown-mode-hook))
	(add-hook hook (lambda() (flyspell-mode 1))))
	;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
	)

(dolist (hook '(change-log-mode-hook emacs-news-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda() (flyspell-mode -1))))

(use-package flyspell-lazy
	:after flyspell
	:config (setq
		flyspell-lazy-idle-seconds 1
		flyspell-lazy-window-idle-seconds 3)
		(flyspell-lazy-mode 1))

(use-package flyspell-correct
	:after flyspell
	:bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Tex
(add-hook 'tex-mode-hook (lambda() (setq ispell-parser 'tex)))


;; Org-mode
(setq-default
	org-startup-indented nil
	;; #+STARTUP: indent
	;; #+STARTUP: noindent
	org-pretty-entities t
	org-use-sub-superscripts "{}"
	org-hide-emphasis-markers t
	org-startup-with-inline-images t
	org-image-actual-width '(300))

(setq
	org-directory "~/Documents/org"
	org-agenda-file (concat org-directory "/daily.org")
	org-agenda-files (list org-agenda-file)
	org-agenda-text-search-extra-files '(agenda-archives)
	org-default-notes-file (concat org-directory "/notes.org")
	org-id-locations-file (concat user-emacs-directory "var/org-id-locations")

	org-startup-folded 'content			; folded children content all
	org-startup-shrink-all-tables t

	org-catch-invisible-edits 'smart
	org-ctrl-k-protect-subtree t
	;org-cycle-separator-lines -1		; show all blank lines between headings
	org-ellipsis "$"
	org-enable-priority-commands nil
	org-footnote-auto-adjust t
	org-list-allow-alphabetical t
	org-log-done t						; 'CLOSED' logging
	org-log-repeat nil
	org-log-state-notes-into-drawer nil
	org-special-ctrl-a/e t
	org-support-shift-select t

	org-agenda-include-diary nil
	org-agenda-skip-deadline-if-done t
	org-agenda-skip-scheduled-if-done t
	org-agenda-start-on-weekday nil
	org-agenda-todo-ignore-deadlines t
	org-agenda-todo-ignore-scheduled t

	org-export-date-timestamp-format "%Y-%m-%d"
	org-export-preserve-breaks t
	org-export-with-author nil
	org-export-with-date nil
	org-export-with-toc nil

	org-ascii-text-width 50
	org-ascii-inner-margin 2
	org-ascii-quote-margin 4
	org-ascii-headline-spacing '(0 . 1))

(setq
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
	("P" "Project List"	((tags "PROJECT")))
	("O" "Office" 		((agenda)(tags-todo "OFFICE")))
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
	("s" "🛒 Shopping List" entry (file+headline "~/Documents/org/shoppinglist.org" "SHOPPING LIST")
		"* TODO %?\n  %i\n" :prepend t)

	;; https://github.com/rexim/org-cliplink
	("K" "Cliplink capture task" entry (file "")
		"* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)
	) ; capture templates
	) ; set

(require 'org)
(require 'ox-md)

(use-package org-appear ; automatic visibility toggling of Org elements depending on cursor position
	:hook (org-mode . org-appear-mode))

(use-package org-autolist ; pressing "Return" will insert a new list item automatically
	:hook (org-mode . org-autolist-mode)
	:diminish "AL")

(use-package org-cliplink) ; insert org-mode links from the clipboard

(use-package org-modern ; add some styling to your Org buffer
	:hook (org-mode . global-org-modern-mode)
	:custom
	(org-modern-fold-stars nil)
	(org-modern-keyword nil)
	(org-modern-checkbox nil)
	(org-modern-table nil))

;; (when *mac* (use-package org-mac-link ; grab links from various mac apps
;; 	:config
;; 	(define-key org-mode-map (kbd "C-c o g") 'org-mac-link-get-link)))

(when *natasha*
	(use-package org-chef :ensure t)
	(use-package org-d20)
	(use-package org-roam
	:ensure t
	:custom
		(org-roam-db-location (concat user-emacs-directory "var/org-roam.db"))
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
		(org-roam-setup)
		(org-roam-db-autosync-mode))
		(which-key-alias "C-c n" "org-roam"))

(define-key org-mode-map (kbd "M-[") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-]") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "A-<left>" ) 'outline-up-heading)
(define-key org-mode-map (kbd "A-<right>") (lambda()(interactive)(org-end-of-subtree)))
	;; interactive b/c original function is not interactive (I know, right?)
(define-key org-mode-map (kbd "C-c '") (lambda()(interactive)(org-edit-special)(visual-fill-column-mode -1)))

(add-hook 'org-agenda-finalize-hook 'delete-other-windows)

(add-hook 'org-mode-hook (lambda()
	(prettify-symbols-mode)
	(org-no-ellipsis-in-headlines)
	(visual-fill-column-mode -1) ))

(load "init/org") ; org-mode functions

;; fix table.el error
;; https://github.com/doomemacs/doomemacs/issues/6980
(defun myfunc/check_table_p (oldfunc) (funcall oldfunc t))
(advice-add 'org-at-table-p :around 'myfunc/check_table_p)


;; sundry
(load "init/misc")
(load "init/scripts" 'noerror)

;; bash
(add-to-list 'auto-mode-alist '("\\.bash*" . sh-mode))
(define-key shell-mode-map (kbd "M-r") nil)

;; pdf-export
(load "init/pdfexport")
(eval-after-load 'latex-mode
	'(define-key latex-mode-map (kbd "C-c r") 'latex-compile-and-update-other-buffer))
(eval-after-load 'markdown-mode
	'(define-key markdown-mode-map (kbd "C-c r") 'md-compile-and-update-other-buffer))
(eval-after-load 'org-mode
	'(define-key org-mode-map (kbd "C-c o r") 'org-compile-latex-and-update-other-buffer))


;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up		<next> is fn-down

(global-set-key (kbd "<home>"   ) 'move-beginning-of-line)
(global-set-key (kbd "<end>"    ) 'move-end-of-line)
;; <prior>		'scroll-down-command
;; <next>		'scroll-up-command

(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) (lambda()(interactive)(end-of-buffer)(recenter -1)))
(global-set-key (kbd "C-<prior>") 'scroll-down-line)
(global-set-key (kbd "C-<next>" ) 'scroll-up-line)

;; M-<home>		'beginning-of-buffer-other-window
;; M-<end>		'end-of-buffer-other-window
;; M-<prior>	'scroll-other-window-down
;; M-<next>		'scroll-other-window

(global-set-key (kbd "M-<up>")   (lambda()(interactive)(backward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<down>") (lambda()(interactive)(forward-paragraph)(recenter-top-bottom)))
(global-set-key (kbd "M-<left>") (lambda()(interactive)(backward-page)(recenter-top-bottom)))
(global-set-key (kbd "M-<right>")(lambda()(interactive)(forward-page)(recenter-top-bottom)))


;; scroll settings
(setq
	auto-window-vscroll nil
	next-screen-context-lines 0
	scroll-conservatively 10000
	scroll-margin 0
	scroll-preserve-screen-position t
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
;(use-package disable-mouse)
;(global-disable-mouse-mode)

(mouse-avoidance-mode 'banish)

(when *mac*
	;; https://lmno.lol/alvaro/hey-mouse-dont-mess-with-my-emacs-font-size
	(global-set-key (kbd "<pinch>") 'ignore)
	(global-set-key (kbd "<C-wheel-up>") 'ignore)
	(global-set-key (kbd "<C-wheel-down>") 'ignore)
	(global-set-key (kbd "<swipe-left>")  'mac-next-buffer)
	(global-set-key (kbd "<swipe-right>") 'mac-previous-buffer))


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
(bind-key "C-x S-u" 'undo-redo)

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
	(when (display-graphic-p)
	(dolist (key '("<f10>" "S-<f10>" "C-<f10>")); "M-<f10>"))
	(global-unset-key (kbd key))))

	(global-set-key (kbd "C-x C-c") (lambda() (interactive)
		(if mac-pseudo-daemon-mode (mac-pseudo-daemon-mode -1))
		(save-buffers-kill-terminal)))
		(which-key-alias "C-x C-c" "save-buffers-kill-terminal")
	(bind-key "s-M-z" 'undo-redo))


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

(bind-key "<f6>"	'toggle-fill-column-center)
(bind-key "<f7>"	'flyspell-buffer)
(bind-key "<f8>"	'list-bookmarks)

(bind-key "C-`" 	'scratch-buffer)
(bind-key "C-!" 	'shell)

(bind-key "M-<f1>" 'my/emacs-help)
(bind-key "M-<f2>" 'describe-personal-keybindings)
(bind-key "M-<f3>" 'shortdoc)

(bind-key "M-Q"		'unfill-paragraph)
(which-key-alias "M-p f" "fill buffer")

(bind-key "C-M-;"	'eval-r) (defun eval-r (b e) (interactive "r")(eval-region b e)(deactivate-mark))
(bind-key "C-M-y"	'undo-yank)

(bind-key "C-c a"	'org-agenda) (when *mac*
(bind-key "C-c z"	'my/agenda) (defun my/agenda () (interactive)(find-file org-agenda-file)))

(bind-key "C-c b m" 'new-markdown-buffer)
(bind-key "C-c b n" 'new-empty-buffer)
(bind-key "C-c b o" 'new-org-buffer)
(which-key-alias "C-c b" "buffers")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC" 'display-current-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-alias "C-c d" "dates")

(bind-key "C-c g"	'elpher) ; gopher / gemini

(bind-key "C-c m"	'menu-bar-read-mail)
(which-key-alias "C-c m" "read-mail")
;(bind-key "C-c n"	'newsticker-show-news)

(bind-key "C-c o a" 'org-archive-subtree-default)
(bind-key "C-c o c"	'org-capture)
(bind-key "C-c o k" 'org-cliplink)
(bind-key "C-c o l"	'org-store-link)
(bind-key "C-c o t" 'org-toggle-link-display)
(which-key-alias "C-c o" "org")

(bind-key "C-c p"	'markdown-preview-file)
(bind-key "C-c q"	'dictionary-search)
(bind-key "C-c w"	'eww-list-bookmarks) ; www

(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x d" 'delete-duplicate-lines)
(bind-key "C-c x f" 'toggle-fill-column)
(bind-key "C-c x g" 'replace-garbage-chars)
(bind-key "C-c x i" 'lorem-ipsum-insert-paragraphs)
(bind-key "C-c x l" 'toggle-truncate-lines)
(bind-key "C-c x n"	'number-paragraphs)
(bind-key "C-c x t" 'delete-trailing-whitespace)
(bind-key "C-c x v" 'add-file-local-variable)
(bind-key "C-c x w" 'delete-whitespace-rectangle)
(which-key-alias "C-c x" "text")

(global-set-key (kbd "C-c 8 c") (kbd "✓"))
(global-set-key (kbd "C-c 8 n") (kbd "№"))
(global-set-key (kbd "C-c 8 p") (kbd "¶"))
(which-key-alias "C-c 8" "key translations")

(bind-key "C-c C-r" 'sudo-edit)

;; Ctrl-x (buffer functions)
(which-key-alias "C-x a" "abbrev")
(which-key-alias "C-x n" "narrow")
(which-key-alias "C-x p" "project")
(which-key-alias "C-x r" "registers")

(bind-key "C-x c" 'kill-current-buffer)

(bind-key "C-x x k"	'kill-other-buffers)
(bind-key "C-x x l" 'buf-to-LF)
(bind-key "C-x x m" 'move-buffer-file)
(bind-key "C-x x r"	'rename-file-and-buffer)
(bind-key "C-x x v" 'view-text-file-as-info-manual)
(bind-key "C-x x w" 'preview-html)
	(defun preview-html () (interactive)(shr-render-buffer (current-buffer)))

(which-key-alias "C-x 8" "key translations")
(which-key-alias "C-x 8 e" "emojis")


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
(when *w32* (setq
	default-directory "c:/Users/henrypa/OneDrive - City of Ottawa/"
	org-agenda-file (concat default-directory "!.org")))
