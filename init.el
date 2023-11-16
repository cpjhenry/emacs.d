;;; Emacs configuration / cpjh

;; Initialize terminal
(toggle-frame-maximized)
(electric-indent-mode -1)
(scroll-bar-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)

(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst *bullwinkle* (string-equal (system-name) "bullwinkle"))
(defconst *natasha* (string-equal (system-name) "natasha"))

(when *mac* (add-to-list 'default-frame-alist '(font . "Inconsolata 21"))
	(setq
		; Mac command key is Super
		; Mac option  key is Meta
		; Mac control key is Control
		mac-function-modifier 'hyper	; Hyper
		mac-right-command-modifier 'alt	; Alt
		mac-right-option-modifier nil)	; pass-thru
	(define-key key-translation-map (kbd "<C-mouse-1>") (kbd "<mouse-2>")) )
(when *gnu* (add-to-list 'default-frame-alist '(font . "Monospace 17")) )
(when *w32* (add-to-list 'default-frame-alist '(font . "Consolas 12"))
	(setq
		w32-lwindow-modifier 'super
		w32-pass-lwindow-to-system nil
		w32-apps-modifier 'hyper)
	(message "Running on Windows.") )

(when (display-graphic-p)
(add-to-list 'default-frame-alist '(background-color . "Ivory")) )
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))

(setq
	user-mail-address "cpjhenry@gmail.com"
	calendar-latitude 45.3
	calendar-longitude -75.7
	calendar-location-name "Ottawa"
	maidenhead "FN25dg")

;; Initialize package manager
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(require 'package)
(package-initialize t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setf
	use-package-always-ensure t
	use-package-verbose t)
(use-package auto-package-update
	:config
	(setq auto-package-update-delete-old-versions t)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe) )

;; Add directories to load-path
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "opt" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "var" user-emacs-directory))

;; settings
(set-language-environment 'utf-8)
(setq-default
	tab-width 4
	standard-indent 4
	fill-column 55
	help-window-select t
	indicate-empty-lines t)
(setq
	initial-major-mode 'fundamental-mode
	default-major-mode 'text-mode

	ad-redefinition-action 'accept
	bookmark-save-flag 1
	bookmark-set-fringe-mark nil
	bookmark-sort-flag nil
	case-fold-search t
	comp-async-report-warnings-errors 'silent
	delete-by-moving-to-trash t
	dictionary-server "dict.org"
	eww-search-prefix "https://www.google.ca/search?q="
	flyspell-issue-message-flag nil
	frame-inhibit-implied-resize t
	frame-title-format nil
	help-clean-buttons t
	ibuffer-expert t
	inhibit-default-init t
	ispell-list-command "--list"	; correct command
	ispell-program-name "aspell"	; spell checker
	ispell-silently-savep t			; save personal list automatically
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
	scroll-preserve-screen-position t
	sentence-end-double-space nil
	show-paren-style 'mixed
	tramp-default-method "ssh"
	tramp-syntax 'simplified		; C-x C-f /remotehost:filename
	trash-directory "~/.Trash"
	use-dialog-box nil
	use-file-dialog nil
	visual-line-fringe-indicators '(nil right-curly-arrow) )

(if (>= emacs-major-version 28)
	(setq use-short-answers t)
	(defalias 'yes-or-no-p 'y-or-n-p) )

(when (>= emacs-major-version 28)
	(setq
   		goto-address-mail-face 'default) )

(when (< emacs-major-version 28)
	(defalias 'show-paren-local-mode 'show-paren-mode) )

;; files
(setq
	abbrev-file-name			(concat user-emacs-directory "etc/abbrev_defs")
	auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/")
	bookmark-default-file	   	(concat user-emacs-directory "etc/bookmarks")
	eshell-aliases-file			(concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name  		(concat user-emacs-directory "var/eshell/")
	eww-bookmarks-directory		(concat user-emacs-directory "etc/")
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

(unless *w32*	(require 'backup-each-save)
				(add-hook 'after-save-hook 'backup-each-save))

;; garbage collection
(use-package gcmh
	:config (gcmh-mode 1)
	:diminish)


;; buffers
(load "init/filesandbuffers")

(add-hook 'before-save-hook
	'time-stamp)
(add-hook 'dired-mode-hook (lambda()
	(dired-omit-mode 1) ))
(add-hook 'emacs-lisp-mode-hook (lambda()
	(setq show-trailing-whitespace t)
	(goto-address-mode)
	(prettify-symbols-mode)
	(show-paren-local-mode) ))
(add-hook 'emacs-news-view-mode-hook (lambda()
	(form-feed-mode) ))
(add-hook 'eww-mode-hook (lambda()
	(define-key eww-mode-map (kbd "<left>") 'eww-back-url) ))
(add-hook 'ibuffer-mode-hook (lambda()
	(ibuffer-switch-to-saved-filter-groups "home")
	(ibuffer-update nil t) ))

(add-hook 'prog-mode-hook (lambda()
	(abbrev-mode)
	(when (not (memq major-mode (list 'lisp-interaction-mode)))
		(display-line-numbers-mode) )
	(flyspell-prog-mode)
	(show-paren-local-mode) ))

(remove-hook
	'file-name-at-point-functions
	'ffap-guess-file-name-at-point)

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
	(define-key help-mode-map (kbd "<right>")	'help-go-forward) )
(with-eval-after-load 'Info-mode
	(define-key Info-mode-map (kbd "q")			'kill-current-buffer)
	(define-key Info-mode-map (kbd "<left>" )	'Info-history-back)
	(define-key Info-mode-map (kbd "<right>")	'Info-history-forward) )

(easy-menu-add-item  nil '("Buffers") ["Increase text size" text-scale-increase])
(easy-menu-add-item  nil '("Buffers") ["Decrease text size" text-scale-decrease])

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

;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil); turn off C-x C-w remapping

(use-package ido-sort-mtime :config	(ido-sort-mtime-mode 1))

(add-to-list 'ido-ignore-buffers "*Messages*")
(add-to-list 'ido-ignore-buffers "*Shell Command Output*")
(add-to-list 'ido-ignore-files ".DS_Store")
(add-to-list 'ido-ignore-files "ido.last")

(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-d")	'ido-dired)

;; iBuffer
;; https://www.emacswiki.org/emacs/IbufferMode
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(define-key ibuffer-mode-map (kbd "q")		'kill-current-buffer)
(define-key ibuffer-mode-map (kbd "<up>")	'ibuffer-previous-line)
(define-key ibuffer-mode-map (kbd "<down>")	'ibuffer-next-line)
(define-key ibuffer-mode-map (kbd "<right>")'ibuffer-previous-header)
(define-key ibuffer-mode-map (kbd "<left>")	'ibuffer-next-header)
(define-key ibuffer-mode-map (kbd "<return>")(lambda()(interactive)(ibuffer-visit-buffer)
	(let ((buffer "*Ibuffer*")) (and (get-buffer buffer) (kill-buffer buffer))) ))

(setq
	ibuffer-hidden-filter-groups (list "Helm" "*Internal*")
	ibuffer-saved-filter-groups (quote (("home"
	   	("dired" (mode . dired-mode) )
		("emacs" (or
			(name . "^\\*scratch\\*$")
			(name . "^\\*Messages\\*$")
			(name . "\\.el") ))
		("org" (name . "\\.org") )
		("planner" (or
			(name . "^\\*Calendar\\*$")
			(name . "^diary$")
			(name . "^\\*Org Agenda\\*") ))
		;("perl" (mode . cperl-mode) )
		;("erc" (mode . erc-mode) )
		;("gnus" (or
		;	(mode . message-mode)
		;	(mode . bbdb-mode)
		;	(mode . mail-mode)
		;	(mode . gnus-group-mode)
		;	(mode . gnus-summary-mode)
		;	(mode . gnus-article-mode)
		;	(name . "^\\.bbdb$")
		;	(name . "^\\.newsrc-dribble") ))
		)) ))

(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*Shell Command Output\\*")
(add-to-list 'ibuffer-never-show-predicates "^\\*tramp/")

(use-package ibuffer-tramp)


;; calendar
(load "init/calendar")
(advice-add 'calendar-exit :before #'my/save-diary-before-calendar-exit)
(add-hook 'calendar-mode-hook (lambda()
	(local-set-key (kbd "q") (lambda()(interactive)(calendar-exit 'kill)
							 (let ((buffer "*wclock*")) (and (get-buffer buffer) (kill-buffer buffer)))))
	(local-set-key (kbd "w") (lambda()(interactive)(world-clock)(next-window-any-frame)(fit-window-to-buffer)))
	(local-set-key (kbd "y") (lambda()(interactive)(list-holidays (string-to-number (format-time-string "%Y")))))
	))
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
	calendar-mark-diary-entries-flag t)


;; print functions
(load "init/page-dimensions")
(easy-menu-add-item nil '("file" "print") ["Enscript" spool-to-enscript t])
(easy-menu-add-item nil '("file" "print") ["Enscript (region)" spool-to-enscript-region t])
(define-key menu-bar-print-menu [print-buffer] nil)
(define-key menu-bar-print-menu [print-region] nil)
(define-key menu-bar-print-menu [ps-print-buffer] nil)
(define-key menu-bar-print-menu [ps-print-region] nil)

(when *mac* (setq
	printer-name "Brother_HL_L2370DW"
	ps-paper-type 'a5
	ps-lpr-switches '("-o media=a5")

	ps-left-margin 28
	ps-right-margin 28
	ps-top-margin 28
	ps-bottom-margin 28

	ps-font-size 11
	ps-print-color-p nil
	ps-print-header nil
	ps-print-footer nil) )


;; Mode Line
(setq
	battery-mode-line-format "%p%% "
	display-time-24hr-format t
	display-time-default-load-average nil
	mode-line-compact t
	mode-line-position (list mode-line-percent-position " " "(%l,%C)") )
(column-number-mode)
(display-battery-mode)
(display-time-mode -1)

;; Startup time
(defun efs/display-startup-time ()
	(message "GNU Emacs %s loaded in %s with %d garbage collections." emacs-version
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
		gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)


;; Initialize packages
(use-package diminish)

(use-package elpher
	:config	(setq elpher-bookmarks-file (concat user-emacs-directory "var/elpher-bookmarks"))
			(easy-menu-add-item  nil '("tools") ["Gopher" elpher t])
			(defun elpher:eww-browse-url (original url &optional new-window) "Handle gemini links."
				(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url) (elpher-go url))
				(t (funcall original url new-window))) )
			(advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
			(defun elpher-up() (interactive)(backward-paragraph)(recenter-top-bottom))
			(defun elpher-down() (interactive)(forward-paragraph)(recenter-top-bottom)) )
	(add-hook 'elpher-mode-hook (lambda ()
		(setq-local
			left-margin-width 10
			gnutls-verify-error nil)
		(set-window-buffer nil (current-buffer))
		(local-set-key (kbd "<left>") 'elpher-back)
		(local-set-key (kbd "<right>") 'elpher-down) ))

(require 'eww)
(define-key eww-bookmark-mode-map (kbd "w")	'eww)

(use-package google-this
	:config (google-this-mode)
	(which-key-add-key-based-replacements "C-c /" "google-this")
	:diminish)

(use-package hl-todo
    :hook (prog-mode . hl-todo-mode)
          (emacs-lisp-mode . hl-todo-mode)
    :config
    (setq hl-todo-keyword-faces
          `(("TODO"       warning bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold) )))

(use-package lorem-ipsum
	:config	(setq-default lorem-ipsum-sentence-separator " ")
			(easy-menu-add-item  nil '("edit") ["Lorem-ipsum" lorem-ipsum-insert-paragraphs t]) )

(use-package form-feed ; ^L
	:config (global-form-feed-mode)
	:diminish)

(use-package smooth-scrolling
	:config (smooth-scrolling-mode) )

(use-package ssh)

(use-package sudo-edit)

(use-package visible-mark)

(use-package wc-mode)

(use-package which-key
	:config (which-key-mode)
	:diminish)

;; Diminish built-in modes
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'visual-line-mode "VLM")


;; Configure specific machines
(when *natasha*
	(setq 	browse-url-browser-function 'browse-url-generic
		;	browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox"
			browse-url-generic-program "/Applications/Waterfox.app/Contents/MacOS/waterfox")
	(load "init/elfeed") )

(when *mac*
;	(load "init/deft")	; note functions (bound to <f7>)
;	(load "init/sn")	; simplenote	 (bound to <f8>)
	)

(when *gnu*
	(setq	browse-url-browser-function 'browse-url-generic
			browse-url-generic-program "firefox-esr") )

(unless *w32* (use-package pdf-tools
	:load-path "site-lisp/pdf-tools/lisp"
	:magic ("%PDF" . pdf-view-mode)
	:config
		(pdf-tools-install :no-query) ))


;; Emacs Text and Markdown modes
(add-hook 'text-mode-hook (lambda ()
	(abbrev-mode)
	(unless *w32* (flyspell-mode))
	(goto-address-mode)
	(visual-line-mode)
	(wc-mode) ))
(eval-after-load "flyspell" '(progn
	(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)))

(use-package olivetti
	:config	(setq olivetti-body-width 80) )

(use-package markdown-mode
	:config (setq
		markdown-command "multimarkdown"
		markdown-enable-prefix-prompts nil
		markdown-hide-urls t
		markdown-italic-underscore t
		markdown-unordered-list-item-prefix "* ")
		(add-to-list 'markdown-uri-types "gemini")
	:mode
		(("README\\.md\\'" . gfm-mode)
		("\\.md\\'" . markdown-mode)
		("\\.markdown\\'" . markdown-mode)
		("\\.gmi\\'" . markdown-mode))
	:commands (markdown-mode gfm-mode) )

(load "init/text") ; text functions


;; Org-mode
(use-package org
	:config (setq
		org-directory "~/Documents/org"
		org-agenda-file (concat org-directory "/daily.org")
		org-agenda-files (list org-agenda-file)
		org-agenda-text-search-extra-files '(agenda-archives)
		org-default-notes-file (concat org-directory "/notes.org")

		org-startup-folded 'content			; folded children content all
		org-catch-invisible-edits 'smart
		org-ctrl-k-protect-subtree t
		org-ellipsis "."
		org-enable-priority-commands nil
		org-export-preserve-breaks t
		org-export-with-toc nil
		org-footnote-auto-adjust t
		org-log-done t						; 'CLOSED' logging
		org-log-state-notes-into-drawer nil
		org-log-repeat nil
		org-special-ctrl-a/e t
		org-support-shift-select t
		org-tags-exclude-from-inheritance '("PROJECT")

		org-agenda-include-diary nil
		org-agenda-skip-scheduled-if-done t
		org-agenda-skip-deadline-if-done t
		org-agenda-todo-ignore-scheduled t
		org-agenda-todo-ignore-deadlines t
		org-agenda-start-on-weekday nil

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
	    ("+" (:strike-through t)) )

		org-agenda-custom-commands '(
		("P" "Project List" (
			(tags "PROJECT") ) )
		("O" "Office" (
			(agenda)
			(tags-todo "OFFICE") ) )
		("W" "Weekly Plan" (
			(agenda)
			(todo "TODO")
			(tags "PROJECT") ) )
		("H" "Home NA Lists" (
			(agenda)
			(tags-todo "HOME")
			(tags-todo "COMPUTER") ) ) )
		) ; set

	(setq
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
		) ; capture templates
		) ; set
		(add-hook 'org-agenda-finalize-hook 'delete-other-windows)

		(add-hook 'org-mode-hook (lambda ()
			(org-autolist-mode)
			(org-indent-mode)
			(prettify-symbols-mode) ))

		(use-package org-autolist
			:diminish "AL")

		(use-package org-chef :ensure t)

		(use-package org-cliplink)

		(use-package org-modern)
		(with-eval-after-load 'org (global-org-modern-mode))

		(load "init/org")							; org-mode functions
		(load "org-phscroll" 'noerror 'nomessage)	; org-table fix
		) ; use-package


;; sundry
(load "init/misc")

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
(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) 'end-of-buffer)

(global-set-key (kbd "<C-M-prior>") 'backward-page)
(global-set-key (kbd "<C-M-next>") 'forward-page)

(global-unset-key (kbd "C-<prior>"))
(global-unset-key (kbd "C-<next>" ))

(global-unset-key (kbd "M-<left>" ))
(global-unset-key (kbd "M-<right>"))
(global-unset-key (kbd "M-<up>"   ))
(global-unset-key (kbd "M-<down>" ))

(global-unset-key (kbd "s-<left>" ))
(global-unset-key (kbd "s-<right>"))
(global-unset-key (kbd "s-<up>"   ))
(global-unset-key (kbd "s-<down>" ))


;; alternate keys
(bind-key "C-S-k"	'kill-whole-line)

(global-set-key (kbd "C-s")		'isearch-forward-regexp)
(global-set-key (kbd "C-r")		'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")	'isearch-forward)
(global-set-key (kbd "C-M-r")	'isearch-backward)

(global-set-key (kbd "<f12>")	'list-buffers)
(global-set-key (kbd "TAB")		'self-insert-command)

(global-set-key (kbd "A-<return>")(kbd "M-<return>"))

(global-unset-key (kbd "C-x C-z"))

;; Darwin overrides
(when *mac*
	(global-set-key   (kbd "s-o")	'find-file)
	(global-set-key   (kbd "s-S")	'write-file)

	(global-unset-key (kbd "s-m"))
	(global-unset-key (kbd "s-n"))
	(global-unset-key (kbd "s-q"))
	(global-unset-key (kbd "s-w"))

	(when (display-graphic-p)
	(global-set-key	  (kbd "s-p")	'ps-print-buffer-with-faces)

	(global-unset-key (kbd "<f10>"))
	(global-unset-key (kbd "C-<f10>"))
	(global-unset-key (kbd "S-<f10>"))
	(global-unset-key (kbd "C-z")) ))

;; window navigation
(when (fboundp 'windmove-default-keybindings)
	(global-set-key (kbd "ESC <up>")	'windmove-up)
	(global-set-key (kbd "ESC <down>")	'windmove-down)
	(global-set-key (kbd "ESC <right>")	'windmove-right)
	(global-set-key (kbd "ESC <left>")	'windmove-left) )


;; Diabled keys
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)	; C-x C-u
(put 'downcase-region 'disabled nil); C-x C-l


;; Shortcuts

(bind-key "<f6>"	'list-bookmarks)
(bind-key "M-Q"		'unfill-paragraph)
(bind-key "M-p"		'spool-to-enscript)
(bind-key "M-P"		'spool-to-enscript-region)

(bind-key "C-c ?"	'describe-personal-keybindings)

(bind-key "C-c a a"	'org-agenda) (when *mac*
(bind-key "C-c a d"	'daily-agenda) (defun daily-agenda() (interactive)(find-file org-agenda-file)))
(which-key-add-key-based-replacements "C-c a" "org agenda")

(bind-key "C-c b m" 'new-markdown-buffer)
(bind-key "C-c b o" 'new-org-buffer)
(bind-key "C-c b s" 'scratch-buffer)
(bind-key "C-c b t" 'new-empty-buffer)
(which-key-add-key-based-replacements "C-c b" "buffers")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC" 'display-current-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-add-key-based-replacements "C-c d" "dates")

(bind-key "C-c g"	'elpher-show-bookmarks) ; gopher / gemini

(bind-key "C-c i f" 'toggle-fill-column)
(bind-key "C-c i i" 'display-fill-column-indicator-mode)
(which-key-add-key-based-replacements "C-c i" "fill-column")

(bind-key "C-c l d" 'delete-duplicate-lines)
(bind-key "C-c l t" 'delete-trailing-whitespace)
(bind-key "C-c l w" 'delete-whitespace-rectangle)
(which-key-add-key-based-replacements "C-c l" "line functions")

(bind-key "C-c o a" 'org-archive-subtree-default)
(bind-key "C-c o c"	'org-capture)
(bind-key "C-c o k" 'org-cliplink)
(bind-key "C-c o l"	'org-store-link)
(which-key-add-key-based-replacements "C-c o" "org")

(bind-key "C-c p"	'markdown-preview-file)
(bind-key "C-c s"	'dictionary-search)
(bind-key "C-c w"	'eww-list-bookmarks) ; www

(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x l" 'lorem-ipsum-insert-paragraphs)
(bind-key "C-c x n"	'number-paragraphs)
(bind-key "C-c x q"	'replace-smart-quotes)
(which-key-add-key-based-replacements "C-c x" "text functions")

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

(which-key-add-key-based-replacements "C-x 8" "key translations")
(which-key-add-key-based-replacements "C-x 8 e" "emojis")
;; For 'C-x'... 'C-a', 'C-g', 'C-y' and 'C-z' are available


;; Aliases
(defalias 'er 'eval-region)
(defalias 'la 'list-abbrevs)
(defalias 'lc 'list-colors-display)
(defalias 'lp 'list-packages)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'flym 'flyspell-mode)
(defalias 'fm 'fundamental-mode)
(defalias 'hm 'html-mode)
(defalias 'jsm 'js-mode)
(defalias 'mm 'markdown-mode)
(defalias 'olv 'olivetti-mode)
(defalias 'om 'org-mode)
(defalias 'tm 'text-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wm 'whitespace-mode)

(defalias 'dr 'desktop-read)
(defalias 'ds 'desktop-save)

;; Work-specific
(when *w32*
	(setq default-directory "c:/Users/henrypa/OneDrive - City of Ottawa/")
	(bind-key "C-c x o"	'office.org)
	(defun office.org ()(interactive)(find-file (concat default-directory "!.org"))) )
