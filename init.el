;;; init.el --- Emacs configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; brew install emacs-plus

;; - `auto-insert' inserts code templates, including GPL notice.

;; Naming convention:
;;
;; - `cpj/' functions are my own utilities, helpers, commands, and
;;   glue. They are not pretending to be part of another package.
;;
;; - `my/' functions are local replacements or wrappers around
;;   existing package functions, usually preserving the original
;;   intent while changing behaviour, interactivity, or presentation.

;;; Code:
;; Initialize terminal
(blink-cursor-mode -1)
(delete-selection-mode t)
(electric-indent-mode -1)
(show-paren-mode -1)
(tooltip-mode -1)

;; Add directories to load-path
(dolist (dir '("etc" "opt" "usr" "var"))
  (let* ((path (expand-file-name dir user-emacs-directory)))
    (when (file-directory-p path)
      (add-to-list 'load-path (directory-file-name path)))))

;; Environmental constants
(message "→ Configuring environment.")
(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst system-short-name (car (split-string (system-name) "\\.")) "Hostname of local machine.")
(defconst *bullwinkle* (string-equal system-short-name "bullwinkle"))
(defconst *natasha* (string-equal system-short-name "natasha"))

(defconst my/emacs-29-p (>= emacs-major-version 29) "Non-nil when running Emacs 29 or newer.")
(defconst my/emacs-30-p (>= emacs-major-version 30) "Non-nil when running Emacs 30 or newer.")
(defconst my/emacs-31-p (>= emacs-major-version 31) "Non-nil when running Emacs 31 or newer.")
(defconst my/emacs-32-p (>= emacs-major-version 32) "Non-nil when running Emacs 32 or newer.")

(defvar cpj/init-loading-incomplete t
  "Non-nil while init.el is still loading.")

(when (bound-and-true-p ns-emacs-plus-version)
  (message "→ Running `Emacs Plus %s'." ns-emacs-plus-version))
(load "rc/me" 'noerror 'nomessage)
(eval-after-load "startup"
  '(fset 'display-startup-echo-area-message (lambda ())))

;; OAuth/GPG compatibility for org-gcal/oauth2-auto.
(setenv "GPG_AGENT_INFO" nil)
(setq epg-pinentry-mode 'loopback
      plstore-encrypt-to nil)
(defvar oauth2-auto-plstore)
(setq oauth2-auto-plstore (expand-file-name "oauth2-auto.plist"
			  (expand-file-name "var/" user-emacs-directory)))


;;; Customize
(when *mac*
  (setopt mac-function-modifier nil
	  mac-control-modifier 'control	; Control
	  mac-option-modifier 'meta	; Meta
	  mac-command-modifier 'super	; Super
	  mac-right-command-modifier 'alt ; Alt
	  mac-right-option-modifier nil); pass-thru

  ;; suppress mac frame refocus
  (setq ns-use-native-fullscreen nil)

  (keymap-global-set "s-c" 'ns-copy-including-secondary)	; ⌘-c = Copy
  (keymap-global-set "s-x" 'kill-region)			; ⌘-x = Cut
  (keymap-global-set "s-v" 'yank)				; ⌘-v = Paste
  (keymap-global-set "s-y" 'ns-paste-secondary)

  (keymap-global-set "s-a" 'mark-whole-buffer)
  (keymap-global-set "s-E" 'edit-abbrevs)
  (keymap-global-set "s-f" 'isearch-forward-regexp)
  (keymap-global-set "s-h" 'ns-do-hide-emacs)
  (keymap-global-set "s-k" 'kill-current-buffer)
  (keymap-global-set "s-l" 'goto-line)
  (keymap-global-set "s-o" 'find-file)
  (keymap-global-set "s-S" 'write-file)
  (keymap-global-set "s-s" 'save-buffer)
  (keymap-global-set "s-u" 'revert-buffer)
  (keymap-global-set "s-W" 'delete-frame)
  (keymap-global-set "s-w" 'kill-current-buffer)
  (keymap-global-set "s-z" 'undo)

  (keymap-global-set "s-1" "C-x 1")
  (keymap-global-set "s-3" "C-x 3")

  (dolist (key '("s-C" "s-D" "s-d" "s-e" "s-F" "s-f" "s-g" "s-j" "s-L"
		 "s-M" "s-m" "s-n" "s-p" "s-q" "s-t" "s-^" "s-&" "s-|"))
	  (keymap-global-unset key))

  ;; Disable suspend-frame
  (keymap-global-unset "C-z")

  ;; Line movement
  (keymap-global-set "<home>" nil) ; 'move-beginning-of-line
  (keymap-global-set "<end>"  nil) ; 'move-end-of-line

  ;; Alternates
  (keymap-global-set "C-<f11>" 'display-battery-mode)

  (keymap-global-set "A-<left>" "s-<left>")
  (keymap-global-set "A-<right>" "s-<right>")
  (keymap-global-set "A-k" "s-k")
  (keymap-global-set "A-=" "s-=")

  ;; Emojis
  (easy-menu-add-item global-map '(menu-bar edit) ["Emoji & Symbols"
	ns-do-show-character-palette
	:help "Show macOS Character Palette."
	:visible (eq window-system 'ns)])

  ;; Font
  (add-to-list 'default-frame-alist '(font . "Inconsolata 23")))

(when *gnu*
  (add-to-list 'default-frame-alist '(font . "Monospace 17"))
  (message "→ Running on GNU/Linux."))

(when *w32*
  (setopt w32-apps-modifier 'super)

  (keymap-global-set "<f11>" 'toggle-frame-maximized)
  (defalias 'restart-emacs 'save-buffers-kill-terminal)

  (add-to-list 'default-frame-alist '(font . "Consolas 12"))
  (menu-bar-mode 1)
  (message "→ Running on Windows."))


;;; Initialize package manager
(require 'package)
(setopt package-archive-column-width 1
	package-user-dir (expand-file-name "var/elpa/" user-emacs-directory)
	package-gnupghome-dir (expand-file-name "var/elpa/" user-emacs-directory))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))
(use-package use-package
  :ensure nil
  :custom (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (use-package-verbose t))


;;; settings
(set-language-environment 'utf-8)

(setopt	standard-indent 4
	tab-width 4
	;fill-column 70

	;indent-line-function 'indent-according-to-mode
	;tab-always-indent nil

	ad-redefinition-action 'accept
	async-shell-command-buffer 'new-buffer
	auth-sources '("~/.authinfo")
	case-fold-search t
	confirm-kill-processes nil ; quit Emacs directly even if there are running processes
	cursor-in-non-selected-windows nil
	custom-buffer-done-kill t
	delete-by-moving-to-trash t
	display-line-numbers-widen t
	enable-recursive-minibuffers t
	enable-remote-dir-locals t ; .dir-locals.el
	eval-expression-debug-on-error nil
	fill-nobreak-predicate '(fill-single-word-nobreak-p fill-single-char-nobreak-p fill-french-nobreak-p)
	find-file-visit-truename t
	goto-address-mail-face 'default
	help-clean-buttons t
	help-enable-variable-value-editing t
	help-window-select t
	history-delete-duplicates t
	indicate-empty-lines t
	inhibit-default-init t
	inhibit-startup-message t ; 'About Emacs'
	inhibit-startup-buffer-menu t ; Don't show *Buffer list*
	initial-scratch-message nil ; Makes *scratch* empty
	isearch-allow-scroll t
	kill-do-not-save-duplicates t
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
	visual-line-fringe-indicators '(nil right-curly-arrow)
	what-cursor-show-names t
	x-stretch-cursor t

	;; completion
	completion-auto-help 'always
	completion-auto-select 'second-tab
	completion-styles '(basic initials substring)
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t)

(if my/emacs-31-p
    (setopt quit-window-kill-buffer t)
  (defun my/quit-window ()
    "Quit the current window, killing its buffer."
    (interactive)
    (quit-window t))
  (define-key key-translation-map [remap quit-window] #'my/quit-window))

;; files
(setopt	custom-file			(concat user-emacs-directory "custom.el")
	nsm-settings-file		(concat user-emacs-directory "var/network-security.data")
	transient-history-file		(concat user-emacs-directory "var/transient/history.el")
	transient-levels-file		(concat user-emacs-directory "var/transient/levels.el")
	transient-values-file		(concat user-emacs-directory "var/transient/values.el")
	url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))
(setq	persist--directory-location	(concat user-emacs-directory "var/persist"))

(require 'multisession)
(setopt multisession-directory (concat user-emacs-directory "var/multisession/"))

(require 'request)
(setopt request-storage-directory (concat user-emacs-directory "var/request/"))

;; path
(use-package exec-path-from-shell
	:if	*mac*
	:custom	(shell-file-name "/usr/local/bin/bash")
		(exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
	:init (unless (bound-and-true-p ns-emacs-plus-injected-path)
		(exec-path-from-shell-initialize)))

;; garbage collection
(use-package gcmh :config (gcmh-mode 1))


;;; modeline
(message "→ Configuring modeline.")
(require 'battery)

(use-package doom-modeline
  :custom (doom-modeline-column-zero-based nil)
	(doom-modeline-enable-word-count t)
	(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
	(doom-modeline-icon nil)
	(doom-modeline-project-name nil)
	(doom-modeline-time-icon nil)
  :hook (after-init . doom-modeline-mode))

(setopt	battery-mode-line-format "%p%% "
	display-time-24hr-format t
	display-time-default-load-average nil
	mode-line-compact nil
	mode-line-position (list mode-line-percent-position " " "(%l,%C)")
	mode-line-right-align-edge 'right-fringe)
(if my/emacs-30-p (setopt project-mode-line t))

(column-number-mode)
;; (display-battery-mode)
;; (display-time-mode)
;; (load "rc/mm" 'noerror 'nomessage) ; memento-mori

(use-package ewth
  ;; https://github.com/chubin/wttr.in for deets
  ;; https://wttr.in/:help for options
  :disabled
  :if *natasha*
  :ensure nil
  :defer 2
  :config
  (setq ewth-url "http://wttr.in/Ottawa?format=2&d&T")
  (ewth-mode))

;; startup time
(defun efs/display-startup-time ()
  (message
   "GNU Emacs %s loaded in %s with %d garbage collection(s).%s"
   emacs-version
   (format "%.2f seconds"
           (float-time
            (time-subtract after-init-time before-init-time)))
   gcs-done
   (if cpj/init-loading-incomplete
       " [WARNING: 'init.el' did not complete]"
     "")))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;; buffers
(message "→ Configuring buffers.")

;;;; Libraries

(use-package s)
(use-package dash) ; for `-find', `-compose' and `-partial'

(load "filesandbuffers" nil 'nomessage)
(load "render-buffers" nil 'nomessage)
(load "skeletons" nil 'nomessage)

(use-package lean-emacs
  :ensure nil
  :demand t
  :bind (("M-j"     . join-line) ; default-indent-new-line, see C-M-j
         ("C-w"     . kill-region-or-backward-word)
         ("M-w"     . kill-region-or-thing-at-point)
         ("C-M-]"   . match-paren)
         ("C-x x s" . save-all-unsaved))
  :hook (find-file . large-find-file-hook)
  :config
  ;; Emacs suffers when you open large files.
  (dolist (key '("<home>" "s-<left>" "C-a"))
    (when (key-binding (kbd key))
      (global-set-key (kbd key)
                      #'back-to-indentation-or-beginning-of-line))))

;;;; Built-in packages

(use-package abbrev
  :ensure nil
  :custom
  (abbrev-file-name (expand-file-name "etc/abbrev_defs" user-emacs-directory))
  (abbrev-suggest t)
  (save-abbrevs 'silently))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1)
  (bookmark-set-fringe-mark nil)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "etc/bookmarks" user-emacs-directory)))

(use-package esh-mode
  :ensure nil
  :requires em-alias
  :custom
  (eshell-aliases-file
   (expand-file-name "etc/eshell/aliases" user-emacs-directory))
  (eshell-directory-name
   (expand-file-name "var/eshell/" user-emacs-directory)))

(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t))

(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'pushy))

(use-package prog-mode
  :ensure nil
  :config
  (global-prettify-symbols-mode)
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge))

(use-package net-utils
  :ensure nil
  :custom
  (whois-server-name "whois.ca.fury.ca"))

(use-package info
  :ensure nil
  :demand t
  :bind (:map Info-mode-map
              ("q" . kill-current-buffer)
              ("[" . Info-history-back)
              ("]" . Info-history-forward)
              ("{" . Info-backward-node)
              ("}" . Info-forward-node))
    :config
    (add-to-list 'Info-additional-directory-list
		 (expand-file-name "usr/info/" user-emacs-directory)))

(use-package calc
  :ensure nil
  :bind (:map calc-mode-map
              ("q" . kill-current-buffer))
  :config
  (defun cpj/quick-calc-cleanup (function &rest arguments)
    "Run FUNCTION with ARGUMENTS without leaving a new Calc buffer."
    (let ((calculator-buffer (get-buffer "*Calculator*")))
      (unwind-protect
          (apply function arguments)
        (unless calculator-buffer
          (when-let* ((buffer (get-buffer "*Calculator*")))
            (kill-buffer buffer))))))

  (advice-add 'quick-calc :around #'cpj/quick-calc-cleanup))

(use-package help-mode
  :ensure nil
  :hook
  (help-mode . cpj/help-mode-setup)
  :bind (:map help-mode-map
              ("["     . help-go-back)
              ("]"     . help-go-forward)
              ("M-RET" . goto-address-at-point))
  :config
  (defun cpj/help-mode-setup ()
    "Configure Help buffers."
    (setq-local font-lock-keywords-only t)
    (goto-address-mode 1)))

(use-package emacs-news-mode
  :ensure nil
  :bind (:map emacs-news-view-mode-map
              ("[" . my/outline-previous-heading)
              ("]" . my/outline-next-heading)
              ("{" . outline-backward-same-level)
              ("}" . outline-forward-same-level)))

(use-package view
  :ensure nil
  :bind (:map view-mode-map
              ("j" . View-scroll-line-forward)
              ("k" . my/View-scroll-line-backward)
              ("q" . View-kill-and-leave)))

;;;; Files and saving

(setopt auto-save-default nil
        auto-save-list-file-prefix
        (expand-file-name "var/auto-save/" user-emacs-directory)
        auto-save-no-message nil
        auto-save-visited-interval 60
        create-lockfiles nil

        backup-by-copying t
        delete-old-versions t
        backup-directory-alist '(("." . ".~"))

        make-backup-files t
        vc-make-backup-files nil ; don't make backups in git-controlled dirs
        version-control nil)

(add-hook 'before-save-hook #'time-stamp)

;; Give files +x permissions when saved if they contain a valid shebang.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Remove trailing whitespace on save.
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Diffs on save
(add-to-list
 'save-some-buffers-action-alist
 (list "d"
       (lambda (buffer)
         (diff-buffer-with-file
          (buffer-file-name buffer)))
       "show diff between the buffer and its file"))

;;;; Auto-save when changing buffers/windows

;; Save all unsaved files when changing focus
(setq after-focus-change-function #'save-all-unsaved)

(defun cpj/save-current-file-buffer (&rest _)
  "Save the current buffer when it is visiting a file."
  (when buffer-file-name
    (save-buffer)))

(advice-add 'switch-to-buffer  :before #'cpj/save-current-file-buffer)
(advice-add 'other-window      :before #'cpj/save-current-file-buffer)
(advice-add 'windmove-left     :before #'cpj/save-current-file-buffer)
(advice-add 'windmove-right    :before #'cpj/save-current-file-buffer)
(advice-add 'windmove-up       :before #'cpj/save-current-file-buffer)
(advice-add 'windmove-down     :before #'cpj/save-current-file-buffer)

;;;; Mode hooks

(add-hook 'doc-view-mode-hook #'auto-revert-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)
(add-hook 'pdf-view-mode-hook #'auto-revert-mode)

(remove-hook 'file-name-at-point-functions
             #'ffap-guess-file-name-at-point)

;;;; Special buffers

;; Modes derived from `special-mode' pick this up.
(keymap-set special-mode-map "q" #'kill-current-buffer)
(keymap-set messages-buffer-mode-map "q" #'bury-buffer)

(add-to-list
 'display-buffer-alist
 '("\\`\\*\$begin:math:text$Warnings\\\\\|Compile\-Log\\$end:math:text$\\*\\'"
   (display-buffer-no-window)
   (allow-no-window . t)))

;; Remove *Completions* buffer when minibuffer exits.
(defun cpj/kill-completions-buffer ()
  "Kill the *Completions* buffer, if present."
  (when-let* ((buffer (get-buffer "*Completions*")))
    (kill-buffer buffer)))

(add-hook 'minibuffer-exit-hook #'cpj/kill-completions-buffer)

;; Opening multiple files.
(add-hook 'window-setup-hook #'delete-other-windows)

;;;; Scratch buffer

(use-package autoscratch
  :custom
  (initial-major-mode 'autoscratch-mode))

;;;; Form feed

;; The form-feed ASCII character, 0x0C, historically marked the end of a page.
;; It is still useful in code for dividing a file into logical pages.
(use-package form-feed-st
  :config
  (global-form-feed-st-mode)
  (add-to-list 'form-feed-st-include-modes 'help-mode t))

;;;; Editing conveniences

;; Comment continuation.
(keymap-set emacs-lisp-mode-map "S-<return>" #'default-indent-new-line)

;; Comment out malfunctioning code, or, well, comment.
(defmacro comment (&rest _body)
  "Ignore BODY, just like `ignore', but as a macro."
  nil)

;;;; Which-key

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :bind (("C-h C-h" . nil))
  :config
  (which-key-mode)

  (defalias 'which-key-alias
    #'which-key-add-key-based-replacements)

  (defun cpj/which-key-abort-quietly (&optional _)
    "Abort which-key without signalling `keyboard-quit'."
    (interactive)
    (let ((which-key-inhibit t))
      (when (fboundp 'which-key--hide-popup-ignore-command)
        (which-key--hide-popup-ignore-command))
      (message nil)))

  (which-key-define-key-recursively
   global-map
   (kbd "C-g")
   #'cpj/which-key-abort-quietly)

  (push '((nil . "\\`cpj/which-key-abort-quietly\\'") . t)
        which-key-replacement-alist))

;;;; Search and narrowing

;; Search TERM in a web browser.
(keymap-set search-map "b" #'browser-search)

(use-package narrow-dwim
  :ensure nil
  :bind (("C-c n" . narrow-dwim)))

;;;; Deferred / disabled

;; Copies every file you save in Emacs to a backup directory tree.
;; (use-package backup-each-save
;;   :ensure nil
;;   :hook (after-save . backup-each-save))

;; Real auto-save.
;; (when (>= emacs-major-version 26)
;;   (auto-save-visited-mode 1))

;; whois
;; HACK: when executing command, resultant buffer needs local key set.
;; (advice-add 'whois :after ...)


;;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
;; HACK · replace with `fido-mode' (cf. http://xahlee.info/emacs/emacs/emacs_fido_mode.html)
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
	(add-to-list 'ido-ignore-buffers "^*debug ")
	(add-to-list 'ido-ignore-buffers "^*Compile-Log*")
	(add-to-list 'ido-ignore-buffers "^*Async-native-compile-log*")
	(add-to-list 'ido-ignore-buffers "^*Backtrace*")
	(add-to-list 'ido-ignore-buffers "^*Warnings*")
	(add-to-list 'ido-ignore-buffers "*Flymake log*")
	(add-to-list 'ido-ignore-files ".DS_Store")
	(add-to-list 'ido-ignore-files "ido.last")
	(add-to-list 'completion-ignored-extensions ".synctex.gz")
	(add-to-list 'completion-ignored-extensions ".tex")
	(add-to-list 'completion-ignored-extensions ".pdf")

	(use-package ido-sort-mtime :config (ido-sort-mtime-mode 1)))

;; M-x enhancement
(use-package smex
  :custom (smex-save-file (concat user-emacs-directory "var/smex.history"))
  :bind ( ("M-x" . smex))
  :config (smex-initialize))


;;; Ibuffer
;; https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :ensure nil
  :demand t
  :custom (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-expert t)
  (ibuffer-saved-filter-groups
   '(("home"
      ("Emacs" (or (name . "^\\*[^*]*scratch[^*]*\\*$")
                   (name . "^\\*Messages\\*$")
                   (name . "\\.el")))
      ("Dired" (mode . dired-mode))
      ("Shell" (or (mode . sh-mode)
                   (mode . mistty-mode)))
      ("Text" (or (name . "\\.txt")
                  (name . "\\.text")))
      ("Markdown" (or (name . "\\.md")
                      (name . "\\.ronn")))
      ("Org"  (name . "\\.org"))
      ("Planner" (or (mode . calendar-mode)
                     (mode . diary-mode)
                     (mode . diary-fancy-display-mode)
		     (mode . calfw-calendar-mode)
                     (name . "^\\*daily-info\\*")
                     (name . "^\\*Org Agenda\\*")
                     (name . "^\\*Virgo\\*")
                     (name . "^calendar@*")
		     (name . "^\\*Holidays\\*")
		     (name . "^\\*ind\\*")))
      ("TeX"  (or (name . "\\.tex")
		  (name . "\\.bib")))
      ("ePub" (mode . nov-mode))
      ;("erc" (mode . erc-mode))
      ("Eww"  (mode . eww-mode))
      ("gnus" (or (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "\\.bbdb$")
                  (name . "^\\.newsrc-dribble"))) )))
  :bind ( :map ibuffer-mode-map
          ("C-x C-f" . ibuffer-ido-find-file)
          ("<up>" . ibuffer-previous-line)
          ("<down>" . ibuffer-next-line)
          ("<left>" . ibuffer-previous-header)
          ("<right>" . ibuffer-next-header)
          ("<return>" . my/ibuffer-visit-buffer))
  :init   (defalias 'list-buffers 'ibuffer) ; always use Ibuffer
  :config (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "home")
	      (ibuffer-update nil t)))
  (require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*Messages\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Shell Command Output\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*tramp/")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Latex Preview Pane Welcome\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Flymake log\\*"))


;;; Dired
(use-package dired
  :ensure nil
  :demand t
  :config (set-face-attribute 'dired-ignored nil
			      :inherit 'dired-file-name))

(use-package dired-x
  :ensure nil
  :demand t
  :custom (dired-dwim-target t) ; suggest other visible Dired buffer
          (dired-listing-switches "-laGhv  --group-directories-first")
          (dired-garbage-files-regexp (concat dired-garbage-files-regexp
            "\\|\\.DS_Store\\|\\.old\\|\\.synctex\\.gz\\|\\.log\\|\\.tex"))
	  (dired-omit-verbose nil)
	  (image-dired-thumbnail-storage 'standard)
  :bind ( :map dired-mode-map
          ("q" . kill-dired-buffers)
          ("C-<home>" . dired-home)
          ("C-<end>" . dired-end))
  :hook   (dired-mode . dired-omit-mode)
  :config (unless *w32* (setq dired-kill-when-opening-new-dired-buffer t))
  (defalias 'dired-find-file 'dired-find-alternate-file)
  (advice-add 'dired-find-file-other-window :after
	      (lambda (&rest r) (delete-other-windows)))

  (if (keymap-lookup dired-mode-map "% s")
      (message "Error: %% s already defined in dired-mode-map")
    (define-key dired-mode-map "%s" 'my-dired-substspaces))

  (setopt dired-omit-files (concat dired-omit-files "\\|^.DS_Store\\|^.localized"))
  (add-to-list 'dired-omit-extensions ".synctex.gz")
  (delete "~" dired-omit-extensions)); show backup files

;; improve file sorting
(use-package ls-lisp
  :ensure nil
  :custom (ls-lisp-use-string-collate nil)
          (ls-lisp-ignore-case t)
  :config (unless *w32* (setopt ls-lisp-use-insert-directory-program nil)))

(use-package dired-narrow
  :after dired
  :demand t
  :bind ( :map dired-mode-map
	  ("/" . dired-narrow))
  :config (easy-menu-add-item dired-mode-map '(menu-bar immediate)
	    ["Narrow dired buffer" dired-narrow :help "Narrow to the files matching a string"]))

(use-package quick-preview
  :after dired
  :demand t
  :bind ( :map dired-mode-map
	  ("<SPC>" . quick-preview-at-point))
  :config
  (easy-menu-add-item dired-mode-map '(menu-bar immediate)
    ["Quick Preview" quick-preview-at-point :help "Preview file at point with quick preview tool"]))

(use-package reveal-in-osx-finder
  :after dired
  :demand t
  :bind ( :map dired-mode-map
	  ("r" . reveal-in-osx-finder))
  :config
  (easy-menu-add-item dired-mode-map '(menu-bar immediate)
    ["Reveal in Finder" reveal-in-osx-finder :help "Reveal the file in the OS X Finder"]))


;;; Tramp
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


;;; frames
(message "→ Establishing frame logic.")

(when *mac*

  ;; start Emacs server

  (defun cpj/kill-daemon-save-buffers-kill-terminal ()
    "Disable `mac-pseudo-daemon-mode', then save buffers and exit Emacs."
    (interactive)
    (when (bound-and-true-p mac-pseudo-daemon-mode)
      (mac-pseudo-daemon-mode -1))
    (save-buffers-kill-terminal))

  (use-package mac-pseudo-daemon
    :bind ( ("C-x C-c" . cpj/kill-daemon-save-buffers-kill-terminal))
    :config (mac-pseudo-daemon-mode 1))

  (require 'server)
  (unless (server-running-p) (server-start))
  (when (server-running-p) (message "→ Server running."))

  ;; Fix weird frame-size issue on Mac, when calling from `emacsclient'.

  ;; Finder's `Open With...' and `org-protocol' use the custom
  ;; `Emacs Client.app' in ~/Applications.
  ;;
  ;; Its `main.scpt' calls:
  ;;
  ;;   /usr/local/opt/emacs-plus@31/bin/emacsclient
  ;;
  ;; using the stable Homebrew `opt' path rather than a versioned
  ;; Cellar path.
  ;;
  ;; In the `on open' handler, the `-c' argument has been removed so
  ;; Finder-opened files use the existing Emacs frame instead of
  ;; creating a small client frame.
  ;;
  ;; The `on open location' handler passes `org-protocol' URLs directly
  ;; to `emacsclient -n', allowing browser capture to invoke
  ;; `org-capture' in the running Emacs server.
  ;;
  ;; After editing `main.scpt', re-sign and re-register the app with:
  ;;
  ;;   ~/Applications/.local/sign-emacs-client
  ;;
  ;; Ordinary Emacs 31 upgrades should not require changes.
  ;;
  ;; When moving to a new major Emacs version:
  ;;
  ;;   1. Copy the newly installed Emacs Client.app from the Homebrew
  ;;      Cellar to ~/Applications.
  ;;   2. Reapply or replace its `main.scpt' with the customized version.
  ;;   3. Update the Homebrew `opt' path for the new major version.
  ;;   4. Run `~/Applications/.local/sign-emacs-client'.
  ;;   5. Refresh the Dock entry or Launch Services registration if
  ;;      macOS continues to find the older application.
  ;;
  ;; Copying the fresh app bundle is preferable to replacing only
  ;; `main.scpt', since the bundle may contain updated metadata,
  ;; resources, or other upstream changes.
  ;;
  ;; With `-c' removed from the Finder handler, no client-frame geometry
  ;; repair should normally be needed.

  (defconst cpj/frame-workarea-height-fudge 4
    "Pixels to subtract from workarea height when resizing macOS frames.")

  ;; Finder/Emacs Client.app frames can report `fullscreen' as
  ;; `maximized' without being visually maximized. Resizing to the
  ;; full monitor workarea almost works, but on macOS the bottom of
  ;; the frame can clip the minibuffer. Subtracting a few pixels gives
  ;; the NS frame enough room to display it properly.

  (defun cpj/maximize-frame-by-geometry (&optional frame)
    "Resize FRAME to fill its monitor workarea."
    (let* ((frame (or frame (selected-frame)))
           (workarea (alist-get 'workarea
				(frame-monitor-attributes frame))))
      (when (and (frame-live-p frame)
		 (display-graphic-p frame)
		 workarea)
	(let ((left   (nth 0 workarea))
              (top    (nth 1 workarea))
              (width  (nth 2 workarea))
              (height (- (nth 3 workarea)
			 cpj/frame-workarea-height-fudge)))
          (modify-frame-parameters frame '((fullscreen . nil)))
          (set-frame-position frame left top)
          (set-frame-size frame width height t)))))

  ;; Uncomment hook ↓ ↓ ↓ when creating new frames. When calling
  ;; emacsclient without `-c', this should not be needed. You may call
  ;; `cpj/maximize-frame-by-geometry' manually.

  ;; (add-hook 'server-visit-hook
  ;;           #'cpj/maximize-frame-by-geometry)


  ;; Fix frame not-selected issue when closing secondary frame.
  ;; This is still a thing.

  (defun cpj/activate-emacs ()
    "Activate the Emacs application on macOS."
    (when (fboundp 'ns-do-applescript)
      (ns-do-applescript "tell application \"Emacs\" to activate")))

  (defun cpj/refocus-selected-frame ()
    "Focus the currently selected frame."
    (when (display-graphic-p)
      (cpj/activate-emacs)
      (raise-frame (selected-frame))
      (select-frame-set-input-focus (selected-frame))))

  (defun cpj/refocus-selected-frame-after-delete (&rest _)
    "Return focus to Emacs after deleting a frame."
    (run-at-time 0 nil #'cpj/refocus-selected-frame))

  (advice-add 'delete-frame
              :after #'cpj/refocus-selected-frame-after-delete))

(use-package mac-notch-tab-bar
  :ensure nil
  :when *mac*
  :config
  (mac-notch-tab-bar-mode 1))


;;; calendar
(message "→ Configuring calendar.")
(load "calendar-functions" nil 'nomessage)
(require 'moon-holidays) ; usr/ -- Buddhist
(require 'liturgical-year) ; usr/
(require 'discordian-calendar) ; usr/
(require 'mercury-retrograde) ; usr/
(use-package hindu-calendar)
(require 'hindu-diwali) ; usr/
(require 'local-holidays) ; etc/

(calendar-set-date-style 'iso)
(setopt calendar-mark-holidays-flag t
	calendar-mark-diary-entries-flag t
	calendar-month-header '(propertize
	  (format "%s %d" (calendar-month-name month) year)
	  'font-lock-face 'calendar-month-header)
	cal-tex-holidays t
	cal-tex-diary t)

(keymap-set calendar-mode-map "q" 'calendar-exit-kill)
(keymap-set calendar-mode-map "w" 'calendar-world-clock)
(keymap-set calendar-mode-map "y" 'list-holidays-this-year)

(easy-menu-add-item calendar-mode-map '(menu-bar goto)
  ["World clock" calendar-world-clock] "Beginning of Week")
(easy-menu-add-item calendar-mode-map '(menu-bar holidays)
  ["Yearly Holidays" list-holidays-this-year])

(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)

(require 'diary-lib)
(setopt diary-file (expand-file-name "~/Documents/diary")
        diary-list-include-blanks nil)
(keymap-set diary-mode-map "C-c C-q" 'kill-current-buffer)
(add-to-list 'auto-mode-alist '("diary$" . diary-mode))
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'diary-fancy-display-mode-hook 'alt-clean-equal-signs)

;;; Timekeeping
(message "→ Configuring timekeeping.")
(use-package roman-clock ; usr/
  :ensure nil
  :bind (("C-c d r" . roman-clock)
         ("C-c d R" . roman-clock-)))

(use-package roman-clock-period-notify-mode ; usr/
  :disabled
  :ensure nil
  :if (display-graphic-p)
  :config
  (roman-clock-period-notify-mode 1))

(use-package biorhythm ; usr/
  :ensure nil
  :demand t
  :commands (biorhythm
             biorhythm-string
             days-on-earth))

(use-package wwv ; usr/
  :ensure nil
  :demand t
  :commands (wwv
	     wwv-summary))

(use-package daily-info ; etc/
  :ensure nil
  :commands (di
             ind)
  :custom
  (daily-info-include-holidays nil)
  (daily-info-include-diary nil))


;;; Initialize packages
(message "→ Initializing packages.")

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

;; Transient keyboard user interfaces
(use-package casual
  :demand t
  :bind (("M-o" . casual-editkit-main-tmenu)
	 :map calendar-mode-map
	 ("M-o" . casual-calendar-tmenu)
	 :map dired-mode-map
	 ("M-o" . casual-dired-tmenu)
	 :map ibuffer-mode-map
	 ("M-o" . casual-ibuffer-tmenu)
	 :map isearch-mode-map
	 ("M-o" . casual-isearch-tmenu)
	 :map Info-mode-map
	 ("M-o" . casual-info-tmenu)
	 :map Man-mode-map
	 ("M-o" . casual-man-tmenu))
  :config (require 'casual-timezone-utils)
	  (setopt casual-timezone-datestamp-format "%a %e %b %Y %R")
	  (advice-add 'casual-timezone-planner :after (lambda (&rest _) (calendar-exit-kill)))
	  (keymap-set casual-timezone-planner-mode-map "q" 'kill-current-buffer))

;; Fast, friendly searching with ripgrep
(use-package deadgrep
  :bind (("<f5>" . 'deadgrep)
	 :map deadgrep-mode-map
	 ("f" . delete-other-windows))
  :config
  (defalias 'find-grep 'deadgrep)
  (advice-add 'deadgrep :after #'my/delete-other-windows)
  (add-hook 'deadgrep-mode-hook #'flymake-mode-off))

(use-package dictionary
  :ensure nil
  :defer  t
  :custom (dictionary-server "dict.org")
  :bind (("M-s d" . dictionary-search)))

(use-package dunnet
  :ensure nil
  :defer t
  :custom dun-log-file (concat user-emacs-directory "var/games/dunnet-scores"))

(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
	 :map dired-mode-map
	 ([remap dired-do-async-shell-command] . dwim-shell-command)
	 ([remap dired-do-shell-command] . dwim-shell-command)
	 ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  (defun dwim-shell-command-pandoc-org-to-docx ()
    "Convert an org file to docx using a template docx file."
    (interactive)
    (dwim-shell-command-on-marked-files
     "converting from org to docx"
     ;; HACK · use `concat' to render reference doc as variable
     "pandoc -s '<<f>>' -o '<<fne>>.docx' --reference-doc ~/Sync/Notes/Info/custom-reference.docx"
     :utils "pandoc")))

(use-package elpher
  :bind   (:map elpher-mode-map
	  ("[" . elpher-back))
  :hook	  (elpher-mode . (lambda ()
	  (setq-local left-margin-width 10)
	  (set-window-buffer nil (current-buffer))))
  :init	  (easy-menu-add-item global-map '(menu-bar tools)
	    ["Gopher" elpher :help "Browse Gopherspace"] 'browse-web)
  :config (advice-add 'eww-browse-url :around 'elpher:eww-browse-url))

(use-package eww
  :ensure nil
  :demand t
  :custom (browse-url-browser-function 'eww-browse-url)
          (eww-auto-rename-buffer t)
	  (eww-bookmarks-directory (concat user-emacs-directory "etc/"))
	  (eww-readable-adds-to-history nil)
	  (url-privacy-level '(email lastloc))

	  ;; look-and-feel
	  (shr-folding-mode t)
	  (shr-inhibit-images t)
	  (shr-use-colors nil)
	  (shr-use-fonts nil)
	  (shr-bullet "• ")
	  (shr-indentation 2)	; Left-side margin
	  (shr-width nil)	; Fold text for comfiness
	  (shr-max-width 94)	; Controls fold-column in web-derived pages (ie. Elfeed)
				; Useful especially when you increase text-scale.

  :bind (("C-x g" . browse-url-at-point)
	:map eww-mode-map
	("[" . eww-back-url)
	("]" . eww-forward-url)
	("y" . eww-copy-page-url)
	:map eww-bookmark-mode-map
	("w" . eww))
  ;:hook
  :config
  (url-setup-privacy-info)
  (use-package ace-link
    :config (ace-link-setup-default))) ;; alternative to tabbing

(use-package free-keys
  :defer t
  :config (add-to-list 'free-keys-modifiers "s" t)
  	  (add-to-list 'free-keys-modifiers "A" t))

(use-package go-mode
  :defer t)

(use-package google-translate
  :bind (("C-c t t" . google-translate-at-point)
	 ("C-c t <RET>" . google-translate-smooth-translate))
  :init	(which-key-alias "C-c t" "google-translate")
  (setq google-translate-translation-directions-alist
	'(("fr" . "en") ("en" . "fr"))))

(use-package goto-longest-line ; opt/
  :ensure nil
  :commands (goto-longest-line))

(use-package highlight-defined
  :hook (emacs-lisp-mode-hook . highlight-defined-mode))

(use-package hl-todo
  :custom (hl-todo-keyword-faces
	   `(("TODO"       warning bold)
	     ("FIXME"      error bold)
	     ("HACK"       font-lock-constant-face bold)
	     ("REVIEW"     font-lock-keyword-face bold)
	     ("NOTE"       success bold)
	     ("NB"         success bold)
	     ("DEPRECATED" shadow bold)))
  :hook	(prog-mode . hl-todo-mode)
	(emacs-lisp-mode . hl-todo-mode)
	(org-mode . hl-todo-mode))

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

(use-package pandoc-mode)

(use-package shortcuts-mode
  :bind (("<f9>" . shortcuts-mode)))

(use-package simple-httpd :ensure t)

(use-package sparkweather
  :after calendar
  :custom (sparkweather-add-footer nil)
  :bind (:map sparkweather-mode-map
	 ("q" . quit-window)))

(use-package ssh)

(use-package tmr)

(use-package visible-mark) ; make the mark visible


;;; Text, Prog, and Markdown modes
(message "→ Configuring modes.")
(require 'table)
(when (< emacs-major-version 28)
  (defalias 'show-paren-local-mode 'show-paren-mode))

(use-package smart-tab
  :config (global-smart-tab-mode))

(add-hook 'text-mode-hook
	  (lambda ()
	    (abbrev-mode)
	    (goto-address-mode)
	    ;; (table-recognize)
	    (visual-line-mode)))

(add-hook 'fill-nobreak-predicate #'fill-french-nobreak-p)

(use-package visual-fill-column
  :bind (("<f6>" . toggle-fill-column-center))
  :config
  (advice-add 'text-scale-adjust :after
              (lambda (&rest _)
                (when (bound-and-true-p visual-fill-column-mode)
                  (visual-fill-column-adjust)))))

(if my/emacs-30-p (global-visual-wrap-prefix-mode)
  (use-package adaptive-wrap :hook (visual-line-mode . adaptive-wrap-prefix-mode)))

(use-package hl-sentence) ; highlight current sentence

(use-package typo) ; minor mode for typographic editing

(use-package unfill
  :bind (("M-q" . unfill-toggle)))

;; prog-mode
(add-hook 'prog-mode-hook
	  (lambda ()
	    (setq show-trailing-whitespace t)  ; needs to be buffer local
	    (abbrev-mode)
	    (unless (eq major-mode 'lisp-interaction-mode) ; ie. *scratch*
	      (display-line-numbers-mode))
	    (electric-indent-local-mode)
	    (goto-address-prog-mode)
	    (show-paren-local-mode)
	    (when (featurep 'visual-fill-column)
	      (visual-fill-column-mode -1))))

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq tab-width 8
		  truncate-lines t)))

(with-eval-after-load 'elisp-mode
  (keymap-set emacs-lisp-mode-map "C-c C-e" #'cpj/elisp-eval-region-page-or-buffer)

  (keymap-unset emacs-lisp-mode-map "C-c C-f" t)
  (keymap-unset emacs-lisp-mode-map "C-c C-b" t))

(use-package flymake
  :ensure nil
  :preface
  (defun cpj/elisp-flymake-quiet ()
    "Use Flymake for byte-compiler diagnostics, not Checkdoc nagging."
    (remove-hook 'flymake-diagnostic-functions
                 #'elisp-flymake-checkdoc t))

  :hook
  (emacs-lisp-mode . cpj/elisp-flymake-quiet)
  (emacs-lisp-mode . flymake-mode)

  :bind
  (:map flymake-mode-map
        ("C-x ! n" . flymake-goto-next-error)
        ("C-x ! p" . flymake-goto-prev-error)
        ("C-x ! l" . flymake-show-buffer-diagnostics))

  :config
  (which-key-alias "C-x !" "flymake"))

(defun cpj/scratch-buffer-p ()
  "Return non-nil if current buffer is scratch-like."
  (string-match-p "\\`\\*.*scratch.*\\*\\'" (buffer-name)))

(defun cpj/disable-flymake-in-scratch-buffers ()
  "Disable Flymake in scratch-like buffers."
  (when (and (cpj/scratch-buffer-p)
             (bound-and-true-p flymake-mode))
    (flymake-mode -1)))

(add-hook 'after-change-major-mode-hook
          #'cpj/disable-flymake-in-scratch-buffers)

;; Emacs' new trusted content model is ridiculous.
;; Let's turn it off for all previously trusted sources
;; (Emacs itself, Elpa, etc.)
(use-package files
  :ensure nil
  :custom
  (trusted-content
   (cons (expand-file-name user-emacs-directory)
         load-path)))

;; Keep Checkdoc available for manual use, but do not let it drive
;; Flymake diagnostics.  The byte-compiler catches things I care about
;; while editing; Checkdoc is too chatty for continuous feedback.
(use-package checkdoc
  :ensure nil
  :config
  (setq checkdoc-column-zero-backslash-before-paren nil
        checkdoc-force-docstrings-flag nil
        checkdoc--argument-missing-flag nil)
  (add-to-list
   'display-buffer-alist
   '("\\*Checkdoc Status\\*"
     (display-buffer-reuse-window display-buffer-at-bottom)
     (window-height . 0.25)
     (dedicated . t))))

;; bash
(add-to-list 'auto-mode-alist '("\\.bash*" . sh-mode))
(with-eval-after-load 'shell
  (keymap-unset shell-mode-map "M-r" t)
  (keymap-unset shell-mode-map "M-p" t))

(add-hook 'shell-mode-hook #'goto-address-mode)

;; html
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))

;; XML
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.page$" . nxml-mode))

;; (add-hook 'nxml-mode-hook 'show-parens-local-mode)

;; do not mark long lines in whitespace-mode
(require 'whitespace)
(delete 'lines whitespace-style)

;; custom transforms
(use-package first-letter-only
  :ensure nil
  :bind ("C-c x f" . first-letter-only)
  :custom
  (first-letter-only-buffer-name "*FLO*"))

;; Markdown
(use-package markdown-mode
  :demand t
  :custom (markdown-command "multimarkdown")
  (markdown-enable-prefix-prompts nil)
  (markdown-italic-underscore t)
  (markdown-unordered-list-item-prefix "- ")
  :bind ( :map markdown-mode-map
	  ("M-p" . nil)
	  ("C-c p" . markdown-preview-file)
	  ("C-x x o" . markdown-convert-buffer-to-org))
  :mode	(("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.gmi\\'" . markdown-mode)
	 ("\\.ronn\\'" . markdown-mode))
  :commands (markdown-mode
	     gfm-mode
	     markdown-preview)
  :init	(setopt markdown-hide-urls t)
  :config (add-to-list 'markdown-uri-types "gemini"))

;; Text utilities
(load "text-functions" nil 'nomessage)
(require 'number-lines)
(require 'normalize-text)
(require 'replace-garbage-chars)


;;; Org-mode
(message "→ Configuring `org'.")
(setopt org-directory
        (expand-file-name "~/Documents/org/")

        org-default-notes-file
        (expand-file-name "notes.org" org-directory)

        org-generic-id-locations-file
        (expand-file-name "var/org-generic-id-locations"
                          user-emacs-directory)

        org-id-locations-file
        (expand-file-name "var/org-id-locations"
                          user-emacs-directory))

(defun cpj/org-edit-special-disable-visual-fill-column (&rest _)
  "Disable `visual-fill-column-mode' in Org special edit buffers."
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1)))

(defun cpj/org-entities-help-outline-cleanup (&rest _)
  "Make `org-entities-help' easier to browse with Outline folding."
  (let ((inhibit-read-only t))
    (flush-blank-lines (point-min) (point-max)))
  (outline-mode)
  (setq-local truncate-lines t)
  (outline-cycle-buffer)
  (view-mode 1))

(defun cpj/org-latex-export-as-latex-cleanup-windows (&rest _)
  "Clean up window layout after `org-latex-export-as-latex'."
  (when (called-interactively-p 'any)
    (delete-other-windows)))

(defun cpj/org-at-table-p-any-advice (oldfun &rest _args)
  "Call OLDFUN as though `org-at-table-p' had been given ANY."
  (funcall oldfun t))

(use-package org
  :ensure nil
  :demand t

  :custom
  ;; Editing and display.
  (org-ctrl-k-protect-subtree t)
  (org-element-use-cache nil)
  (org-ellipsis "·")
  (org-fold-catch-invisible-edits 'smart)
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline t)
  (org-hidden-keywords nil)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native entities))
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-log-done 'time)
  (org-log-repeat nil)
  (org-log-state-notes-into-drawer nil)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'content)
  (org-startup-indented nil)
  (org-startup-shrink-all-tables t)

  ;; Images.
  (org-startup-with-inline-images t)
  (org-cycle-inline-images-display t)
  (org-image-actual-width t)

  ;; Speed commands and markup.
  (org-use-speed-commands
   (lambda ()
     (and (looking-at org-outline-regexp)
          (looking-back "^\\**" (line-beginning-position)))))
  (org-use-sub-superscripts '{})

  ;; Tags.
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-tags-exclude-from-inheritance '("PROJECT"))

  ;; Export.
  (org-export-with-author t)
  (org-export-with-broken-links t)
  (org-export-with-date t)
  (org-export-with-section-numbers nil)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts t)
  (org-export-with-tables t)
  (org-export-with-toc nil)
  (org-export-with-timestamps t)
  (org-export-date-timestamp-format "%Y-%m-%d")
  (org-export-time-stamp-file t)

  ;; ASCII export.
  (org-ascii-text-width fill-column)
  (org-ascii-inner-margin 2)
  (org-ascii-quote-margin 4)
  (org-ascii-headline-spacing '(0 . 1))

  ;; LaTeX export.
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process
   (list (concat "latexmk -" org-latex-compiler " -recorder -synctex=1 -bibtex-cond %b")))

  ;; Markdown export.
  (org-md-headline-style 'atx)

  ;; TODOs.
  (org-todo-keywords '((sequence "TODO" "DONE")))
  (org-todo-keyword-faces
   '(("INPROGRESS" . (:foreground "blue" :weight bold))))

  ;; Emphasis.
  (org-emphasis-alist
   '(("*" bold)
     ("**" bold)
     ("/" italic)
     ("_" italic)
     ("=" (:background "maroon" :foreground "white"))
     ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
     ("+" (:strike-through t))))

  ;; Capture.
  (org-capture-templates
   '(("p" "Protocol" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%i\n")
     ("L" "Protocol Link" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")))

  :bind
  ( ("C-c k" . org-capture)
    ("C-c l" . org-store-link)

   :map org-mode-map
   ([remap backward-paragraph] . my/org-backward-paragraph)
   ([remap forward-paragraph]  . my/org-forward-paragraph)
   ("S-<return>" . org-open-link-at-point-external)
   ("M-<f4>"     . org-speed-command-help)
   ("M-["        . org-backward-heading-same-level)
   ("M-]"        . org-forward-heading-same-level)
   ("C-M-["      . org-back-to-top-level-heading)
   ("C-M-]"      . my/org-end-of-subtree)
   ("C-c o ^"    . my/org-sort)
   ("C-c o c"    . org-check-misformatted-subtree)
   ("C-c o r"    . org-mode-restart)
   ("C-c o t"    . org-toggle-link-display)
   ("A-b"        . cpj/org-emphasize-bold)
   ("A-i"        . cpj/org-emphasize-italic))

  :config
  (require 'org-tempo)
  (require 'org-capture)
  (require 'org-protocol)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'ox-texinfo)

  (load "org-functions" nil 'nomessage)
  (load "org-links" nil 'nomessage)

  (set-face-underline 'org-ellipsis nil)

  (which-key-alias "C-c o" "org")
  (which-key-alias "C-c o c" "misformatted subtree")

  ;; Org special-edit buffers inherit visual-fill-column from the parent
  ;; buffer, which makes source/example editing awkward. Disable it after
  ;; `org-edit-special' creates the edit buffer.
  (advice-remove 'org-edit-special
		 #'cpj/org-edit-special-disable-visual-fill-column)
  (advice-add 'org-edit-special
              :after #'cpj/org-edit-special-disable-visual-fill-column)

  ;; Alternative implementation of `org-support-shift-select'.
  (dolist (key '("S-<left>" "S-<right>" "S-<up>" "S-<down>"
                 "C-S-<left>" "C-S-<right>" "C-S-<up>" "C-S-<down>"))
    (keymap-set org-mode-map key nil))

  (keymap-set org-mode-map "S-<home>"  #'org-shiftleft)
  (keymap-set org-mode-map "S-<end>"   #'org-shiftright)
  (keymap-set org-mode-map "S-<prior>" #'org-shiftup)
  (keymap-set org-mode-map "S-<next>"  #'org-shiftdown)

  ;; Fix `C-a' binding in Org mode.
  (org-remap org-mode-map
             #'back-to-indentation-or-beginning-of-line
             #'org-beginning-of-line)

  ;; Tweak behaviour of M-up and M-down.
  (add-to-list 'org-metaup-hook
               (lambda () (interactive)
                 (org-transpose-paragraphs -1)))
  (add-to-list 'org-metadown-hook
               (lambda () (interactive)
                 (org-transpose-paragraphs 1)))

  ;; View mode helpers, useful for Org-ish read-only buffers.
  (with-eval-after-load 'view
    (keymap-set view-mode-map "["   #'org-previous-link)
    (keymap-set view-mode-map "]"   #'org-next-link)
    (keymap-set view-mode-map "RET" #'goto-address-at-point))

  ;; Ispell should not check Org drawers or code/example blocks.
  (dolist (region '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                    ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                    ("#\\+begin_src" . "#\\+end_src")
                    ("^#\\+begin_example " . "#\\+end_example$")
                    ("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$")))
    (add-to-list 'ispell-skip-region-alist region))

  ;; Custom entities.
  (add-to-list 'org-entities-user
               '("textnumero" "\\textnumero" nil "&numero;" "No." "No." "№"))

  ;; Better `org-entities-help'.
  (advice-remove 'org-entities-help
		 #'cpj/org-entities-help-outline-cleanup)
  (advice-add 'org-entities-help
              :after #'cpj/org-entities-help-outline-cleanup)

  ;; LaTeX classes.
  (add-to-list 'org-latex-classes
               '("letter" "\\documentclass{letter}")
               t)

  (add-to-list 'org-latex-classes
               '("memoir" "\\documentclass{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
               t)

  (advice-remove 'org-latex-export-as-latex
		 #'cpj/org-latex-export-as-latex-cleanup-windows)
  (advice-add 'org-latex-export-as-latex
              :after #'cpj/org-latex-export-as-latex-cleanup-windows)

  ;; Fix `org-table-convert-region' menu entry.
  (define-key org-tbl-menu [Convert\ Region]
              '(menu-item "Convert Region" org-table-convert-region
                :enable
                (or (org-region-active-p)
                    (not (org-at-table-p 'any)))))

  ;; Fix table.el error.
  ;; https://github.com/doomemacs/doomemacs/issues/6980
  ;; Treat table.el tables as tables when Org asks `org-at-table-p'.
  ;; This works around failures in table.el / Org integration, notably
  ;; commands that call `org-at-table-p' without passing ANY.
  ;;
  ;; Remove first so form-feed/page evaluation does not stack duplicate advice.
  (advice-remove 'org-at-table-p
		 #'cpj/org-at-table-p-any-advice)
  (advice-add 'org-at-table-p
              :around #'cpj/org-at-table-p-any-advice))

;;; Org-mode adjunct packages

(use-package org-return
  :ensure nil
  :after org
  :bind (:map org-mode-map
              ([remap org-return] . org-return-dwim)))

(use-package org-comment-placeholder
  :ensure nil
  :after org
  :hook (org-mode . org-comment-placeholder-mode))

(use-package org-hide-inline-footnotes
  :ensure nil
  :after org
  :hook (org-mode . org-hide-inline-footnotes-mode))

(use-package org-quote-indent
  :ensure nil
  :after org
  :hook (org-mode . org-quote-indent-mode))

(use-package org-macro-display
  :ensure nil
  :after org
  :hook (org-mode . org-macro-display-mode))

(use-package org-pretty-table
  ;; FIXME · breaking again
  :disabled
  :ensure nil
  :after org
  :hook (org-mode . org-pretty-table-mode))

(use-package org-rehearsal
  :ensure nil
  :demand t
  :after org
  :custom
  (org-rehearsal-auto-enable-directories
   (delq nil (list ritual-directory)))
  :bind (:map org-mode-map
              ("C-c o m" . org-rehearsal-report))
  :hook (org-mode . org-rehearsal-enable-maybe))

(use-package org-paragraph-preview
  :ensure nil
  :demand t
  :after org
  :custom
  (org-paragraph-preview-latex-header
   (expand-file-name "latexhdr.org" org-directory))
  (org-paragraph-preview-latex-directives
   '("\\ritual"
     "\\nopgnos"))
  :bind (:map org-mode-map
              ("C-c o p" . org-paragraph-preview)))

(use-package org-plain-latex-preview
  :ensure nil
  :after org)

;;; Optional Org packages

;; HACK · After upgrading org-chef, adjust spacing in
;; `org-chef-recipe-to-org-element' from `pre-' to `post-'.
(use-package org-chef
  :if *natasha*
  :demand t
  :after (org org-capture)
  :config
  (defvar org-chef-recipe-book "~/Documents/Recipes/Cookbook.org"
    "Default recipe book.")

  (add-to-list 'org-capture-templates
	       '("c" "Cookbook" entry
		 (file org-chef-recipe-book)
		 "%(org-chef-get-recipe-from-url)"
		 :empty-lines 0)
	       t)

  (add-to-list 'org-capture-templates
	       '("m" "Manual Cookbook" entry
		 (file org-chef-recipe-book)
		 "* %^{Recipe title: }
:PROPERTIES:
:provenance:
:source-url:
:servings:
:prep-time:
:cook-time:
:ready-in:
:END:
** Ingredients%?

** Directions

** Notes
")
	       t)

  (defconst my/org-fraction-replacements
    '(("1/8" . "{{{frac(1,8)}}}")
      ("1/4" . "{{{frac(1,4)}}}")
      ("1/3" . "{{{frac(1,3)}}}")
      ("3/8" . "{{{frac(3,8)}}}")
      ("1/2" . "{{{frac(1,2)}}}")
      ("5/8" . "{{{frac(5,8)}}}")
      ("2/3" . "{{{frac(2,3)}}}")
      ("3/4" . "{{{frac(3,4)}}}")
      ("7/8" . "{{{frac(7,8)}}}")
      ("⅛"   . "{{{frac(1,8)}}}")
      ("¼"   . "{{{frac(1,4)}}}")
      ("⅓"   . "{{{frac(1,3)}}}")
      ("⅜"   . "{{{frac(3,8)}}}")
      ("½"   . "{{{frac(1,2)}}}")
      ("⅝"   . "{{{frac(5,8)}}}")
      ("⅔"   . "{{{frac(2,3)}}}")
      ("¾"   . "{{{frac(3,4)}}}")
      ("⅞"   . "{{{frac(7,8)}}}"))
    "Fraction spellings normalized by `my/org-normalize-fractions'.")

  (defun my/org-normalize-fractions ()
    "Normalize common fractions in the accessible portion of the buffer."
    (interactive)
    (save-mark-and-excursion
      (dolist (replacement my/org-fraction-replacements)
	(goto-char (point-min))
	(while (search-forward (car replacement) nil t)
          (replace-match (cdr replacement) t t)))

      ;; Close up mixed numbers such as 1 1/2, 1-½, and 1–½.
      (goto-char (point-min))
      (while (re-search-forward
              "\\b\\([0-9]+\\)[[:space:]\u00A0\u2010\u2011\u2012\u2013-]*\
\\({{{frac([0-9]+,[0-9]+)}}}\\)"
              nil t)
	(replace-match "\\1\\2"))))

  (add-hook 'org-capture-before-finalize-hook
            #'my/org-normalize-fractions))

(use-package org-cliplink
  :after org
  :bind (:map org-mode-map
              ("C-c o k" . cpj/org-cliplink))
  :config
  (defun cpj/org-cliplink ()
    "Insert an Org link from the clipboard, using the page title."
    (interactive)
    (org-cliplink-insert-transformed-title
     (org-cliplink-clipboard-content)
     (lambda (url title)
       (let* ((parsed-url (url-generic-parse-url url))
              (clean-title
               (if (string= (url-host parsed-url) "github.com")
                   (replace-regexp-in-string
                    "GitHub - .*: \$begin:math:text$\.\*\\$end:math:text$" "\\1" title)
                 title)))
         (org-cliplink-org-mode-link-transformer url clean-title))))))

(use-package org-contrib ; use ':ignore:' tag to exclude heading (but not content) from export
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-download ; `org-download-yank'
  :if *natasha*
  :after org
  :custom
  (org-download-heading-lvl nil)
  (org-download-image-org-width 925))

(use-package org-ref ; setup bibliography, cite, ref, and label org-mode links
  :if *natasha*
  :disabled
  :after org
  :init
  (define-key org-mode-map (kbd "C-=") #'org-ref-insert-link-menu))

(use-package ox-epub
  :after org)

(use-package ox-gemini
  :after org)

;;; End Org-mode configuration

;;; TeX
(use-package tex
  :unless *w32*
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  (LaTeX-mode . (lambda () ;; Make the prettify addition buffer-local and avoid duplicates
    (setq-local prettify-symbols-alist (cons '("\\\\&" . ?＆) prettify-symbols-alist))))
  (tex-mode . (lambda () (setq ispell-parser 'tex)))
  :custom
  (font-latex-fontify-sectioning 'color)
  (LaTeX-babel-hyphen-after-hyphen nil)
  (latex-run-command "xelatex")
  (TeX-auto-save t)
  (TeX-master nil)         ; FIXME EMACS30 fail
  (TeX-parse-self nil)     ; FIXME EMACS30 fail

  ;; AUCTeX Preview (customizable vars)
  (preview-leave-open-previews-visible t)
  (preview-locating-previews-message nil)
  (preview-protect-point t))


;;; Calendar data and Org Agenda
;; Calendar data from macOS Calendar is projected into
;; `calendar-data.org', which is read by `org-agenda' as an ordinary
;; Org agenda source.
;;
;; macOS calendar access is granted to a specific Emacs application
;; bundle.  After installing, replacing, or moving Emacs.app, run:
;;
;;     patch-emacs-calendar-permission
;;
;; For example:
;;
;;     patch-emacs-calendar-permission /usr/local/opt/emacs-plus\@31/Emacs.app
;;
;; This restores Mac Calendar access used by `calendar-data' through
;; `maccalfw'.
(message "→ Configuring calendar dashboards.")

(defvar cpj/org-agenda-file
  (expand-file-name "daily.org" org-directory)
  "Default Org agenda file.")

(defvar cpj/calendar-data-file
  (expand-file-name "calendar-data.org" org-directory)
  "Generated Org file containing macOS Calendar data.")

(setopt org-agenda-files
        (list cpj/org-agenda-file
              cpj/calendar-data-file))

(use-package org-agenda
  :ensure nil
  :after org
  :bind (("C-c a" . cpj/org-agenda-list)
         :map org-agenda-mode-map
         ("q" . org-agenda-exit))
  :hook ((org-agenda-finalize . cpj/org-agenda-register-diary-buffer)
         (org-agenda-mode . hl-line-mode))

  :custom
  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday 1)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-time-leading-zero t)
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-use-time-grid nil)
  (org-agenda-window-setup 'only-window)

  (org-agenda-prefix-format
   '((agenda . " %i %?-12t")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))

  :config
  ;; Normalize the face for previously scheduled items.
  (set-face-attribute 'org-scheduled-previously nil
                      :inherit nil
                      :foreground (face-foreground 'default nil t)
                      :background (face-background 'default nil t)
                      :weight 'normal)

  (defun cpj/org-agenda-list ()
    "Refresh calendar data, then display the Org agenda."
    (interactive)
    (calendar-data-refresh-if-stale)
    (org-agenda-list))

  (defun cpj/org-agenda-register-diary-buffer ()
    "Register the diary buffer for cleanup when Org Agenda exits."
    (when-let* ((buf (get-buffer "diary")))
      (add-to-list 'org-agenda-new-buffers buf))))

(use-package calfw :defer t)
(use-package maccalfw :defer t)
(use-package calendar-data
  :ensure nil
  :commands (calendar-data-refresh
             calendar-data-refresh-if-stale)
  :custom
  (calendar-data-file cpj/calendar-data-file)
  (calendar-data-calendar-names
   '("Family"
     "Birthdays"
     "Ottawa District 1"
     "Home"
     "cpjhenry@gmail.com"))
  (calendar-data-past-days 30)
  (calendar-data-future-days 365))


;;; spell checking
(message "→ Configuring spellchecker.")
(bind-key "<f7>" 'my/ispell-buffer)

(use-package jinx
  :demand t
  :pin gnu ; source from 'gnu' package archives only
  :if (executable-find "aspell")
  :bind ( ([remap ispell-word] . jinx-correct)
	  ([remap my/ispell-buffer] . my/jinx-correct-all)
	  :map jinx-mode-map
	  ("M-$" . jinx-correct)
	  ("C-M-$" . jinx-languages)
	  ("<f7>" . my/jinx-correct-all)
	  ("M-n" . jinx-next))
  :hook	(emacs-startup . global-jinx-mode)
  :config
  (load "jinx-functions" nil 'nomessage)
  (add-hook 'jinx-mode-hook #'my/jinx-add-ispell-localwords)
  (setf (alist-get ?* jinx--save-keys) #'my/jinx-save-as-ispell-localword)
  (defun my/jinx-correct-all ()
    (interactive)
    (with-silent-modifications
      (let ((inhibit-read-only t))
	(jinx-correct-all)))))

;;; print
(message "→ Configuring print engine.")
(define-key global-map [menu-bar file print] nil)

(use-package print-text-latex
  :ensure nil
  :custom
  (print-text-latex-save-output nil)
  :bind (("M-p SPC" . print-text-a5)
         ("M-p c"   . print-text-card-3x5)))

(use-package print-text-card
  :ensure nil
  :custom
  (print-text-latex-save-output nil)
  :bind (("M-p 3" . print-text-card)))


;;; Configure specific machines
(message "→ Configuring specific machines.")
(when *natasha*
  (setopt browse-url-secondary-browser-function 'browse-url-generic
	  ;browse-url-generic-program "/Applications/Waterfox.app/Contents/MacOS/waterfox"
	  browse-url-generic-program "open"))

;; Mail / News
(use-package rmail
  :if	*natasha*
  :ensure nil
  :defer t
  :custom (rmail-secondary-file-directory (concat user-emacs-directory "var/"))
  (rmail-default-file (concat rmail-secondary-file-directory "XMAIL"))
  (rmail-file-name (concat rmail-secondary-file-directory "RMAIL"))

  (rmail-primary-inbox-list '("imaps://cn914@mail.ncf.ca"))
  (rmail-remote-password-required t)
  :hook	(rmail-show-message . goto-address-mode)
  (rmail-quit . kill-current-buffer)
  :config
  (setq
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
  :if *natasha*
  :ensure nil
  :custom (message-kill-buffer-on-exit t)
  :bind ( :map  message-mode-map ("A-<return>" . message-send-and-exit)))

;; RSS
(use-package elfeed
  :if	*natasha*
  :custom (elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/"))
  (elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/"))
  (elfeed-search-remain-on-entry t)
  (elfeed-show-truncate-long-urls nil)
  (elfeed-sort-order 'ascending)
  (elfeed-use-curl t)
  :bind (("C-c f" . elfeed)
	 :map elfeed-search-mode-map
	 ("/" . elfeed-search-live-filter)
	 ("[" . beginning-of-buffer) ; top
	 ("]" . end-of-buffer) ; bottom
	 ("B" . elfeed-search-beginning-to-point-as-read)
	 ("R" . elfeed-search-mark-all-as-read)
	 ("m" . elfeed-mail-todo)
	 ("s" . elfeed-toggle-star)
	 :map elfeed-show-mode-map
	 ("[" . beginning-of-buffer)
	 ("]" . end-of-buffer)
	 ("TAB" . shr-next-link)
	 ("SPC" . scroll-up-half)
	 ("B" . elfeed-show-visit-secondary-browser)
	 ("i" . elfeed-show-toggle-images))
  :init	(easy-menu-add-item global-map '(menu-bar tools)
	  ["Read RSS Feeds" elfeed :help "Read RSS Feeds"] "Read Mail")
  :config (setq
	   elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el")
	   elfeed-log-level 'error)

  (eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
  (advice-add 'elfeed-search-fetch :after
	      (lambda (&rest _) (goto-char (point-min))))
  ;(add-to-list 'global-jinx-modes 'elfeed-show-mode)

  (load "rc/feeds" 'noerror 'nomessage)
  (load "elfeed-functions" nil 'nomessage))

;; Web
(use-package w3m
  :defer t
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
  (load "w3m-functions" nil 'nomessage))

;; Others
(use-package chess
  :if	*natasha*
  :defer t
  :custom
  (chess-default-engine 'chess-gnuchess)
  (chess-images-default-size 80))

(use-package gnugo ; Game of Go
  :disabled
  :if *natasha*
  :defer t
  :init (easy-menu-add-item  global-map '("tools" "games")
	  ["Go" gnugo :help "Play Go"] "Gomoku"))

(use-package nov                         ; Read EPUB files
  :if *natasha*
  :defer t
  :custom
  (nov-save-place-file
   (expand-file-name "var/nov-places" user-emacs-directory))
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (when (featurep 'ibuf-ext)
    (add-to-list 'ibuffer-never-show-predicates
                 "^\\*nov unzip\\*$"))
  (when (featurep 'ido)
    (add-to-list 'ido-ignore-buffers
                 "^\\*nov unzip\\*$")))

(when *gnu*
	(setq browse-url-secondary-browser-function 'browse-url-generic
	      browse-url-generic-program "firefox-esr"))


;;; sundry
(message "→ Configuring sundry.")
(load "misc-functions" nil 'nomessage)
(load "scripts" 'noerror 'nomessage)

(require 'kf-library)
(load "help-cpj" nil 'nomessage)

(load "pdfexport" nil 'nomessage)
(with-eval-after-load 'latex-mode
  (define-key latex-mode-map
              (kbd "C-c r")
              #'latex-compile-and-update-other-buffer))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map
              (kbd "C-c r")
              #'md-compile-and-update-other-buffer))

(with-eval-after-load 'org
  (define-key org-mode-map
              (kbd "C-c o r")
              #'org-compile-latex-and-update-other-buffer))

;; https://jonathanabennett.github.io/blog/2019/05/29/writing-academic-papers-with-org-mode/
(use-package pdf-tools
  :if	*mac*
  :custom (pdf-annot-activate-created-annotations t)
  (pdf-view-display-size 'fit-width)
  :bind ( :map pdf-view-mode-map
	("C-s" . isearch-forward)
	("h" . pdf-annot-activate-created-annotations)
	("t" . pdf-annot-add-text-annotation)
	("D" . pdf-annot-delete))
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query))

(use-package org-pdftools
  :after org pdf-tools
  :hook (org-mode . org-pdftools-setup-link))


;;; UX
(message "→ Configuring UX.")

;;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up	<next> is fn-down

(global-set-key (kbd "C-<home>" ) 'beginning-of-buffer)
(global-set-key (kbd "C-<end>"  ) 'my/end-of-buffer)
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
(global-set-key [remap backward-paragraph] 'my/backward-paragraph)
(global-set-key [remap forward-paragraph] 'my/forward-paragraph)

;;; scroll settings
(setq auto-window-vscroll nil
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
;;	(package-vc-install '(ultra-scroll
;;		:vc-backend Git
;;		:url "https://github.com/jdtsmith/ultra-scroll")))
;; (use-package ultra-scroll
;;	:init (setq scroll-conservatively 101) ; important!
;;	:config (ultra-scroll-mode 1))

;;; mouse
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

;;; window navigation
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


;;; alternate keys
(global-set-key (kbd "C-s")	'isearch-forward-regexp)
(global-set-key (kbd "C-r")	'isearch-backward-regexp)
(global-set-key (kbd "M-s s")	'isearch-forward)
(global-set-key (kbd "M-s r")	'isearch-backward)
(global-set-key (kbd "M-z")	'zap-up-to-char)

(global-set-key (kbd "<f12>")	'list-buffers)
(global-set-key (kbd "M-<f11>")	'toggle-modeline)

(global-set-key (kbd "A-<return>") (kbd "M-<return>"))
(global-set-key (kbd "A-S-<return>") (kbd "M-S-<return>"))

(defun my/recenter-top-bottom ()
  "Scroll the window so that current line is at the top."
  (interactive)
  (recenter-top-bottom 0))
(keymap-global-set "C-l" 'my/recenter-top-bottom)

;; https://www.matem.unam.mx/~omar/apropos-emacs.html#writing-experience
(bind-key "C-d" 'delete-forward-char)      ; better replacement for delete-char
(bind-key "M-c" 'capitalize-dwim)          ; capitalize-word
(bind-key "M-K" 'kill-paragraph)           ; M-k capitalizes sentence
(bind-key "C-x M-t" 'transpose-paragraphs) ; C-x C-t transpose-lines
(global-set-key [remap mark-word] 'mark-whole-word)
(global-set-key [remap forward-word] 'forward-to-word) ; leaves point in better spot

;; Disable alternate suspend-frame
(global-unset-key (kbd "C-x C-z"))

;; Disable the "numeric argument". Prefer universal argument (C-u) prefix.
(dolist (prefix '("C-" "M-" "C-M-"))
  ;(keymap-global-unset (concat prefix "-")) ; negative-argument
  (dotimes (i 10) (keymap-global-unset (concat prefix (number-to-string i)))))

;; Disable <f10> options

;; <f10>	menu-bar-open
;; S-<f10>	context-menu-open
;; C-<f10>	buffer-menu-open
;; M-<f10>	toggle-frame-maximized

(dolist (key '("C-<f10>"))
  (global-unset-key (kbd key)))

;; Cleanup abbrev menu
(dolist (key '("C-a" "+" "-" "'"))
  (keymap-global-unset (concat "C-x a " key)))

;; Undo/redo cleanups
(keymap-global-unset "C-_")
(keymap-global-unset "C-M-_")

(keymap-global-unset "<undo>")
(keymap-global-unset "C-x u")


;;; Disabled functions
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
(add-to-list 'safe-local-variable-values '(truncate-lines . t))
(add-to-list 'safe-local-variable-values '(before-save-hook . (my/org-sort)))

(dolist (value '((flymake-mode . nil)
		 (org-comment-placeholder-mode . nil)
                 (org-hide-inline-footnotes-mode . nil)
                 (org-macro-display-mode . nil)
                 (org-quote-indent-mode . nil)))
  (add-to-list 'safe-local-variable-values value))


;;; Shortcuts
(bind-key "<f8>"	'list-bookmarks)

(bind-key "C-`"		'scratch-buffer)
(bind-key "C-<escape>"	'my/shell)

(bind-key "M-<f1>"	'my/emacs-help)
(bind-key "M-<f2>"	'describe-personal-keybindings)
(bind-key "M-<f3>"	'shortdoc)

(bind-key "C-M-;"	'my/eval-region)
(bind-key "C-M-y"	'undo-yank)


;;; Ctrl-c (personal keybindings)
(bind-key "C-c b"	'eww-list-bookmarks) ; WWW
(which-key-alias "C-c b" "eww-bookmarks")

(bind-key "C-c c"	'calendar)

(bind-key "C-c d SPC"	'display-current-date-and-time)
(bind-key "C-c d c"	'insert-date)
(bind-key "C-c d i"	'insert-iso-date)
(which-key-alias "C-c d" "dates")

(bind-key "C-c e"	'elpher) ; gopher / gemini
(bind-key "C-c i"	'my/init)

(bind-key "C-c m"	'menu-bar-read-mail)
(which-key-alias "C-c m" "read-mail")

(bind-key "C-c x #"	'number-lines-dwim)
(bind-key "C-c x b"	'flush-blank-lines)
(bind-key "C-c x g"	'replace-garbage-chars)
(bind-key "C-c x l"	'lorem-ipsum-insert-paragraphs)
(which-key-alias "C-c x l" "lorem-ipsum")
(bind-key "C-c x n"	'normalize-text-dwim)
(which-key-alias "C-c x" "text")

(bind-key "C-c z"	'my/agenda)

(global-set-key (kbd "C-c 8 c") (kbd "✓"))
(global-set-key (kbd "C-c 8 x") (kbd "⨯"))
(which-key-alias "C-c 8" "keys")


;;; Ctrl-x (buffer functions)
(bind-key "C-x c"	'kill-current-buffer)

(bind-key "C-x x SPC"	'toggle-cursor-off/on)
(bind-key "C-x x L"	'buf-to-LF)
(bind-key "C-x x V"	'view-text-file-as-info-manual)
(bind-key "C-x x a"	'align-regexp)
(bind-key "C-x x c"	'toggle-fill-column)
(bind-key "C-x x k"	'kill-other-buffers)
(bind-key "C-x x l"	'add-file-local-variable)
(bind-key "C-x x m"	'move-buffer-file)
(bind-key "C-x x r"	'rename-file-and-buffer)
(bind-key "C-x x v"	'view-mode)
(bind-key "C-x x w"	'preview-html)
(which-key-alias "C-x x" "buffers")

;; Additional which-key aliases
(which-key-alias "C-x a" "abbrev")
(which-key-alias "C-x a i" "inverse")
(which-key-alias "C-x n" "narrow")
(which-key-alias "C-x p" "project")
(which-key-alias "C-x r" "registers")
(which-key-alias "C-x t" "tabs")
(which-key-alias "C-x w" "windows")


;;; Ctrl-x 8 sequences
(defun my/insert-zero-width-space ()
  "Insert a zero-width space."
  (interactive)
  (insert #x200B))

(with-eval-after-load 'iso-transl
  (keymap-set iso-transl-ctl-x-8-map "0" #'my/insert-zero-width-space)
  (keymap-set iso-transl-ctl-x-8-map "a |" "↕"))

(which-key-alias "C-x 8" "keys")
(which-key-alias "C-x 8 0" "ZWS")
(which-key-alias "C-x 8 e" "emojis")


;;; Aliases
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
(when *w32* (load (expand-file-name ".work" user-emacs-directory) 'noerror nil))
(when *mac* (bind-key "C-c Z" (lambda () (interactive) (find-file "/db:/!.org")))
      (which-key-alias "C-c Z" "work-agenda"))

(setq cpj/init-loading-incomplete nil)
(message "✓ Init file loaded completely.")
;;; init.el ends here

;;=================================================================================
;; DEBUG TOOL --- stops processing of .el file.
;; https://emacs.stackexchange.com/questions/19385/how-to-exit-from-emacs-init-file
;(with-current-buffer " *load*" (goto-char (point-max)))
;;=================================================================================

; LocalWords:  canadian sug aspell memq eval RET kfhelppanels init FN
; LocalWords:  pdfexport melpa vers tls dg defs eshell multisession
; LocalWords:  persistency ido Ibuffer elfeed rc rmh elfeedroutines
; LocalWords:  esr md noindent nEntered shoppinglist Cliplink el kbd
; LocalWords:  INPROGRESS kfhelp setq xm readabilizing JS dev Lorem
; LocalWords:  Gopherspace filesandbuffers ipsum ePub epub xelatex kf
; LocalWords:  vcusepackage latexmk synctex bibtex cond xah dirs Ctrl
; LocalWords:  remotehost modeline mori featurep cbc smex vc ns
; LocalWords:  setq's setopt mailutils imagemagick usr dunnet Async
; LocalWords:  dir fullscreen dropbox keymap toc buddhist ewth ronn
; LocalWords:  enscript noerror formfeed hline erc bbdb newsrc laGhv
; LocalWords:  pandoc alphapapa unpackaged xml xsl xhtml nxml parens
; LocalWords:  MidnightBlue src numero documentclass subsubsection
; LocalWords:  github cliplink Waterfox waterfox nov backend fboundp
; LocalWords:  windmove goto ripgrep nomessage lorem OAuth authinfo
; LocalWords:  plist nopgnos flymake api todo paren docstrings ibuf
; LocalWords:  ibuffer ish minibuffer emacsclient Uncomment maccalfw
