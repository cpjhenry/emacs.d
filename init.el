;;; init.el --- Emacs configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; brew install emacs-plus

;; `auto-insert' inserts code templates, including GPL notice.

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
(when (bound-and-true-p user-full-name)
  (message "Hello, %s." user-full-name))
(eval-after-load "startup"
  '(fset 'display-startup-echo-area-message (lambda ())))


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
  (keymap-global-set "s-2" "C-x 2")
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
  :custom
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (use-package-verbose t))


;;; settings
(set-language-environment 'utf-8)

(setopt	standard-indent 4
	tab-width 4

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
	recenter-positions '(top middle bottom)
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
	time-stamp-start "[Tt]ime-stamp:[ \t]+\\\\?[\"<]+"
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
	completion-ignore-case t
	completion-styles '(basic initials substring)
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t)

;; quit-window / kill-buffer
(if my/emacs-31-p
    (setopt quit-window-kill-buffer t)
  (defun my/quit-window ()
    "Quit the current window, killing its buffer."
    (interactive)
    (quit-window t))
  (define-key key-translation-map [remap quit-window] #'my/quit-window))
(keymap-set messages-buffer-mode-map "q" #'bury-buffer)

;; kill-region
(if my/emacs-31-p
    (setopt kill-region-dwim 'emacs-word))

;; files --- move out of ~/.emacs.d
(setopt	custom-file			(concat user-emacs-directory "var/custom.el")
	nsm-settings-file		(concat user-emacs-directory "var/network-security.data")
	transient-history-file		(concat user-emacs-directory "var/transient/history.el")
	transient-levels-file		(concat user-emacs-directory "var/transient/levels.el")
	transient-values-file		(concat user-emacs-directory "var/transient/values.el")
	url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))

(require 'persist)
(setq persist--directory-location (concat user-emacs-directory "var/persist"))

(require 'multisession)
(setopt multisession-directory (concat user-emacs-directory "var/multisession/"))

(require 'request)
(setopt request-storage-directory (concat user-emacs-directory "var/request/"))

;; path
(use-package exec-path-from-shell
	:if *mac*
	:custom
	(shell-file-name (getenv "SHELL"))
	(exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
	:init
	(unless (bound-and-true-p ns-emacs-plus-injected-path)
	  (exec-path-from-shell-initialize)))

;; garbage collection
(use-package gcmh :config (gcmh-mode 1))


;;; buffers
(message "→ Configuring buffers.")

;; Libraries

(use-package s)
(use-package dash) ; for `-find', `-compose' and `-partial'

(load "filesandbuffers" nil 'nomessage)
(load "render-buffers" nil 'nomessage)
(load "skeletons" nil 'nomessage)

(use-package lean-emacs
  :ensure nil
  :demand t
  :bind
  ("C-M-m" . match-paren)
  :hook
  (find-file . large-find-file-hook)
  :config
  (dolist (key '("<home>" "s-<left>" "C-a"))
    (when (key-binding (kbd key))
      (keymap-global-set key 'back-to-indentation-or-beginning-of-line))))

;; Built-in packages

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
  (bookmark-fringe-mark nil)
  (bookmark-sort-flag nil)
  (bookmark-default-file (expand-file-name "etc/bookmarks" user-emacs-directory)))

(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name
   (expand-file-name "var/eshell/" user-emacs-directory)))

(use-package grep
  :ensure nil
  :custom
  (grep-use-headings t))

(use-package man
  :ensure nil
  :defer t
  :custom
  (Man-notify-method 'pushy))

(use-package net-utils
  :ensure nil
  :custom
  (whois-server-name "whois.ca.fury.ca")
  :config
  (defun cpj/whois-use-net-utils-mode (&rest _)
    "Put the Whois results buffer in `net-utils-mode'."
    (when-let* ((buffer (get-buffer "*Whois*")))
      (with-current-buffer buffer
	(net-utils-mode))))
  (advice-add #'whois :after #'cpj/whois-use-net-utils-mode))

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
  (dolist (face '(info-title-1
                  info-title-2
                  info-title-3
                  info-title-4))
    (set-face-attribute face nil :family "Inconsolata"))

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
  (help-mode . goto-address-mode)
  :bind
  (("C-h C-s" . cpj/find-symbol-source)
   :map help-mode-map
   ("["       . help-go-back)
   ("]"       . help-go-forward)
   ("M-RET"   . goto-address-at-point)))

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

;; Files and saving
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
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Diffs on save
(add-to-list
 'save-some-buffers-action-alist
 (list "d"
       (lambda (buffer)
         (diff-buffer-with-file
          (buffer-file-name buffer)))
       "show diff between the buffer and its file"))

;; Auto-save when changing buffers/windows

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

;; Mode hooks

(add-hook 'doc-view-mode-hook #'auto-revert-mode)
(add-hook 'package-menu-mode-hook #'hl-line-mode)
(add-hook 'pdf-view-mode-hook #'auto-revert-mode)

(remove-hook 'file-name-at-point-functions
             #'ffap-guess-file-name-at-point)

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

;; Scratch buffer

(use-package autoscratch
  :custom
  (initial-major-mode 'autoscratch-mode)
  :config
  (setf (alist-get "#" autoscratch-triggers-alist nil nil #'string=)
	'(autoscratch-select
          '(("org"    . (org-mode))
            ("perl"   . (cperl-mode))
            ("ruby"   . (ruby-mode))
            ("python" . (python-mode))
            ("conf"   . (conf-unix-mode))
            ("shell"  . (shell-script-mode)))))
  (setf (alist-get "*" autoscratch-triggers-alist nil nil #'string=)
	'(org-mode)))

;; Form-feed

;; The form-feed ASCII character, 0x0C, historically marked the end of a page.
;; It is still useful in code for dividing a file into logical pages.
(use-package form-feed-st
  :config
  (defun my/kf-display-with-form-feed-st (fn &rest args)
    "Run FN with ARGS, then enable `form-feed-st-mode' in its result buffer."
    (let ((buf (apply fn args)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (form-feed-st-mode 1)))
      buf))

  (global-form-feed-st-mode)

  (with-eval-after-load 'kf-library
    (advice-add 'kf-display-something-maybe-big
                :around #'my/kf-display-with-form-feed-st)))

;; Editing conveniences

;; Comment continuation.
(keymap-set emacs-lisp-mode-map "S-<return>" #'default-indent-new-line)

;; Which-key

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

;; Search and narrowing

;; Search TERM in a web browser.
(keymap-set search-map "b" #'browser-search)

(use-package narrow-dwim
  :ensure nil
  :bind (("C-c n" . narrow-dwim)))

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


;;; IDO
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
;; HACK · replace with `fido-mode' (cf. http://xahlee.info/emacs/emacs/emacs_fido_mode.html)
(use-package ido
	:ensure nil
	:custom	(ido-save-directory-list-file (concat user-emacs-directory "var/ido.last"))
		(ido-enable-flex-matching t)
		(ido-show-dot-for-dired nil)
	:bind (	("C-<tab>" . ido-switch-buffer)
		("C-x C-d" . ido-dired)
		:map ido-common-completion-map
		("M-TAB" . ido-switch-to-completions))
	:init	(ido-mode t)
	:config
	(define-key
	 (cdr ido-minor-mode-map-entry)
	 [remap write-file] nil)); C-x C-w remapping

(use-package ido-sort-mtime
  :config (ido-sort-mtime-mode 1))

;; M-x enhancement
(use-package smex
  :custom (smex-save-file (concat user-emacs-directory "var/smex.history"))
  :bind ( ("M-x" . smex))
  :config (smex-initialize))


;;; Dired
(use-package dired
  :ensure nil
  :demand t
  :custom
  (dired-free-space nil)
  (dired-movement-style 'bounded-files)
  (dired-dwim-target t) ; suggest other visible Dired buffer
  (dired-listing-switches "-laGhv  --group-directories-first")
  (dired-garbage-files-regexp
   (concat dired-garbage-files-regexp
	   "\\|\\.DS_Store\\|\\.old\\|\\.synctex\\.gz\\|\\.log\\|\\.tex"))
  :bind ( :map dired-mode-map
          ("q" . kill-dired-buffers)
	  ("C-<home>" . dired-home)
	  ("C-<end>" . dired-end))
  :config
  (set-face-attribute 'dired-ignored nil
		      :inherit 'dired-file-name)
  (unless *w32* (setq dired-kill-when-opening-new-dired-buffer t))
  (defalias 'dired-find-file 'dired-find-alternate-file)
  (advice-add 'dired-find-file-other-window :after
	      (lambda (&rest r) (delete-other-windows)))
  (if (keymap-lookup dired-mode-map "% s")
      (message "Error: %% s already defined in dired-mode-map")
    (define-key dired-mode-map "%s" 'my-dired-substspaces)))

(use-package dired-x
  :ensure nil
  :demand t
  :custom
  (dired-omit-verbose nil)
  :hook
  (dired-mode . dired-omit-mode))

(use-package image-dired
  :ensure nil
  :custom
  (image-dired-thumbnail-storage 'standard))

;; Completion and Dired visibility

(dolist (pattern '("\\`\\*Messages"
                   "\\`\\*Shell Command Output"
                   "\\`\\*tramp/"
                   "\\`\\*debug"
                   "\\`\\*Compile-Log"
                   "\\`\\*Async-native-compile-log"
                   "\\`\\*Backtrace"
                   "\\`\\*Warnings"
                   "\\`\\*Flymake log"
		   "\\`\\*Ido Completions"
		   "\\`\\*scratch"))
  (add-to-list 'ido-ignore-buffers pattern))

(dolist (pattern '("\\.~"
                   "\\.DS_Store"
                   "ido\\.last"))
  (add-to-list 'ido-ignore-files pattern))

(add-to-list 'ido-ignore-directories "\\.~")

(dolist (extension '(".synctex.gz"
                     ".tex"
                     ".pdf"
                     ".pages"))
  (add-to-list 'completion-ignored-extensions extension))

(setopt dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store"
                "\\|^.localized"))

(dolist (ext '(".synctex.gz" ".tex"))
  (add-to-list 'dired-omit-extensions ext))

(setopt dired-omit-extensions
        (delete "~" dired-omit-extensions))

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
		     (name . "^\\*ind\\*")
		     (name . "^\\*Buddhist Observation\\*")))
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

  ;; Finder's `Open With...' and `org-protocol' use the Emacs Plus
  ;; `Emacs Client.app' installed in /Applications.
  ;;
  ;; In `Contents/Resources/Scripts/main.scpt', the `-c' argument is
  ;; removed from the `on open' handler so Finder-opened files use the
  ;; existing Emacs frame instead of creating a separate client frame.
  ;;
  ;; The `on open location' handler passes `org-protocol' URLs directly
  ;; to `emacsclient -n', allowing browser capture to invoke
  ;; `org-capture' in the running Emacs server.
  ;;
  ;; After editing `main.scpt', re-sign and re-register the app with:
  ;;
  ;;   sign-emacs-client
  ;;
  ;; A Homebrew upgrade may replace `Emacs Client.app'.  If so, remove
  ;; `-c' from the new `main.scpt' and run `sign-emacs-client' again.
  ;;
  ;; No client-frame geometry repair should normally be needed.

  ;;; start Emacs server

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

  ;; Fix frame not-selected issue when closing secondary frame.

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
              :after #'cpj/refocus-selected-frame-after-delete)

  ;; Fix org-capture capture frame not selected

  (defun cpj/refocus-org-capture-frame ()
    "Raise and focus the frame displaying an Org capture buffer."
    (let ((frame (selected-frame)))
      (run-at-time
       0 nil
       (lambda (frame)
	 (when (frame-live-p frame)
           (cpj/activate-emacs)
           (raise-frame frame)
           (select-frame-set-input-focus frame)))
       frame)))

  (add-hook 'org-capture-mode-hook
            #'cpj/refocus-org-capture-frame)

  (use-package mac-notch-tab-bar
    :ensure nil
    :when *mac*
    :config (mac-notch-tab-bar-mode 1)))


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

(keymap-set calendar-mode-map "d" #'cpj/calendar-view-diary)
(keymap-set calendar-mode-map "q" 'calendar-exit-kill)
(keymap-set calendar-mode-map "w" 'calendar-world-clock)
(keymap-set calendar-mode-map "y" 'list-holidays-this-year)

(easy-menu-add-item calendar-mode-map '(menu-bar goto)
  ["World clock" calendar-world-clock] "Beginning of Week")
(easy-menu-add-item calendar-mode-map '(menu-bar holidays)
  ["Yearly Holidays" list-holidays-this-year])

(advice-add 'calendar-exit :before #'save-diary-before-calendar-exit)

(use-package year-calendar
  :ensure nil
  :commands year-calendar)

(message "→ Configuring diary.")
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
         ("C-c d d" . roman-date)))

(use-package roman-clock-period-notify-mode ; usr/
  :disabled
  :ensure nil
  :if (display-graphic-p)
  :config
  (roman-clock-period-notify-mode 1))

;;; Buddhist observances
(use-package buddhist-observation; usr/
  :ensure nil
  :demand t
  :commands (buddhist-observation-display
             buddhist-observation-today
             buddhist-observation-stop-audio))

;;; Weather
(use-package weather-alert
  :ensure nil
  :commands (wx wx-alert))

(use-package sparkweather
  :after calendar
  :custom (sparkweather-add-footer nil)
  :bind (:map sparkweather-mode-map
	 ("q" . quit-window)))


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
	 ("M-o" . casual-info-tmenu))
  :config
  (require 'casual-timezone-utils)
  (setopt casual-timezone-datestamp-format "%a %e %b %Y %R")
  (advice-add 'casual-timezone-planner :after (lambda (&rest _) (calendar-exit-kill)))
  (keymap-set casual-timezone-planner-mode-map "q" #'kill-current-buffer)
  (with-eval-after-load 'man
    (keymap-set Man-mode-map "M-o" #'casual-man-tmenu)))

;; Fast, friendly searching with ripgrep
(use-package deadgrep
  :bind
  (("<f5>" . 'deadgrep)
   :map deadgrep-mode-map
   ("[" . beginning-of-buffer)
   ("]" . end-of-buffer)
   ("f" . delete-other-windows))
  :config
  (defalias 'find-grep 'deadgrep)
  (add-to-list 'deadgrep-extra-arguments "--glob=!*~" t)
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
  :custom
  (browse-url-browser-function 'eww-browse-url)
  (eww-auto-rename-buffer t)
  (eww-bookmarks-directory (concat user-emacs-directory "etc/"))
  (eww-readable-adds-to-history nil)
  (eww-search-confirm-send-region nil)
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
  :bind
  (("C-x g" . browse-url-at-point)
   ("M-s M-w" . cpj/eww-search-words)
   :map eww-mode-map
   ("[" . eww-back-url)
   ("]" . eww-forward-url)
   ("y" . eww-copy-page-url)
   ("M-p" . nil)
   ("M-n" . nil)
   :map eww-bookmark-mode-map
   ("w" . eww))
  :config
  (url-setup-privacy-info))

(use-package ace-link ; alternative to tabbing
  :after eww
  :config (ace-link-setup-default))

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
	   '(("TODO"       warning bold)
	     ("FIXME"      error bold)
	     ("HACK"       highlight)
	     ("NOTE"       success bold)
	     ("DEPRECATED" shadow bold)))
  :hook	(prog-mode . hl-todo-mode)
	(org-mode . hl-todo-mode))

(use-package list-projects)

(use-package lorem-ipsum
  :bind
  ("C-c x l" . lorem-ipsum-insert-paragraphs)
  :init
  (easy-menu-add-item global-map '(menu-bar edit)
		      ["Lorem-ipsum" lorem-ipsum-insert-paragraphs :help "Insert..."])
  :config
  (setq-default lorem-ipsum-sentence-separator " ")
  (which-key-alias "C-c x l" "lorem-ipsum"))

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

(use-package ssh)

(use-package tmr)

(use-package visible-mark) ; make the mark visible

(use-package wiki-summary
  :bind ("M-s M-s" . cpj/wiki-summary)
  :config
  (defun cpj/wiki-summary-clean-term (term)
    "Clean TERM for use as a Wikipedia search phrase."
    (when term
      (setq term
            (replace-regexp-in-string
             "[ \t]*(.*?)[ \t]*" " " term))
      (setq term
            (replace-regexp-in-string
             "[ \t]*\"[^\"]*\"[ \t]*" " " term))
      (setq term (string-trim term))
      (setq term
            (replace-regexp-in-string
             "[ \t]+Day\\'" "" term))
      (string-trim term)))

  (defun cpj/wiki-summary (&optional prompt)
    "Look up the region, agenda item, or word at point in Wikipedia.

With prefix argument PROMPT, confirm or edit the search term first."
    (interactive "P")
    (let* ((term
            (cond
             ((use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end)))
             ((derived-mode-p 'org-agenda-mode)
              (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))
             (t
              (thing-at-point 'word t))))
           (term (cpj/wiki-summary-clean-term term)))
      (wiki-summary
       (if prompt
           (read-string
            (concat "Wikipedia Article"
                    (if term (format " (%s)" term) "")
                    ": ")
            nil nil term)
	 term)))))


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

(use-package unfill
  :bind (("M-q" . unfill-toggle)))

;; prog-mode
(global-prettify-symbols-mode)
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
(use-package elisp-tools
  :ensure nil
  :demand t
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-e" . elisp-eval-page-region-or-buffer)))

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq tab-width 8
                             truncate-lines t)))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-f" . nil)
        ("C-c C-b" . nil)))

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

(use-package org
  :ensure nil

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
  (org-startup-folded 'content); overview, content, showall, showeverything
  (org-startup-indented nil)
  (org-startup-shrink-all-tables t)
  (org-use-sub-superscripts '{})

  ;; Images.
  (org-startup-with-inline-images t)
  (org-cycle-inline-images-display t)
  (org-image-actual-width t)

  ;; Speed commands and markup.
  (org-use-speed-commands
   (lambda ()
     (and (looking-at org-outline-regexp)
          (looking-back "^\\**" (line-beginning-position)))))

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
   '(("i" "Idea" entry
      (file+headline org-default-notes-file "Ideas")
      "* %?\n")
     ("p" "Protocol" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%i\n")
     ("L" "Protocol Link" entry
      (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")))

  ;; Refile.
  (org-refile-targets
   `((,(expand-file-name "Emacs nix and Homebrew.org" org-directory)
      :maxlevel . 1)
     (org-agenda-files
      :maxlevel . 1) ))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)

  :bind
  (("C-c k" . org-capture)
   ("C-c l" . org-store-link)

   :map org-mode-map
   ([remap backward-paragraph] . my/org-backward-paragraph)
   ([remap forward-paragraph]  . my/org-forward-paragraph)
   ("S-<return>" . cpj/org-open-link-at-point-external)
   ("M-["        . org-previous-visible-heading)
   ("M-]"        . my/org-end-of-subtree)
   ("C-c o ^"    . my/org-sort)
   ("C-c o c"    . org-check-misformatted-subtree)
   ("C-c o r"    . org-mode-restart)
   ("C-c o t"    . org-toggle-link-display)
   ("A-b"        . cpj/org-emphasize-bold)
   ("A-i"        . cpj/org-emphasize-italic))

  :hook
  (org-capture-after-finalize . cpj/org-sort-capture-target)

  :config
  (require 'org-tempo)
  (require 'org-capture)
  (require 'org-protocol)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'ox-texinfo)

  (load "org-functions" nil 'nomessage)

  (set-face-underline 'org-ellipsis nil)

  (which-key-alias "C-c o" "org")
  (which-key-alias "C-c o c" "misformatted subtree")

  ;; add custom speed commands
  (add-to-list 'org-speed-commands
               '("P" . ded/org-show-previous-heading-tidily))
  (add-to-list 'org-speed-commands
               '("N" . ded/org-show-next-heading-tidily))

  ;; Org special-edit buffers inherit visual-fill-column from the parent
  ;; buffer, which makes source/example editing awkward. Disable it after
  ;; `org-edit-special' creates the edit buffer.
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
  (defun my/org-transpose-paragraph-up ()
    "Transpose the current Org paragraph upward."
    (interactive)
    (org-transpose-paragraphs -1))

  (defun my/org-transpose-paragraph-down ()
    "Transpose the current Org paragraph downward."
    (interactive)
    (org-transpose-paragraphs 1))

  (add-to-list 'org-metaup-hook #'my/org-transpose-paragraph-up)
  (add-to-list 'org-metadown-hook #'my/org-transpose-paragraph-down)

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
  (advice-add 'org-at-table-p
              :around #'cpj/org-at-table-p-any-advice))

;;; Org-mode adjunct packages

(use-package org-hide-inline-footnotes
  :ensure nil
  :after org
  :hook (org-mode . org-hide-inline-footnotes-mode))

(use-package org-macro-display
  :ensure nil
  :after org
  :hook (org-mode . org-macro-display-mode))

(use-package org-prose
  :ensure nil
  :commands org-prose-count)

(use-package org-rehearsal
  :ensure nil
  :after org
  :custom
  (org-rehearsal-auto-enable-directories
   (delq nil (list ritual-directory)))
  :bind (:map org-mode-map
              ("C-c o m" . org-rehearsal-report))
  :hook (org-mode . org-rehearsal-enable-maybe))

(use-package org-return
  :ensure nil
  :after org
  :bind (:map org-mode-map
              ([remap org-return] . org-return-dwim)))

(use-package org-paragraph-preview
  ;; Org export buffer with paragraphs shortened to LIMIT characters.
  :ensure nil
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
  :after (org org-capture)
  :config
  (defvar org-chef-recipe-book "~/Documents/Recipes/Cookbook.org"
    "Default recipe book.")

  (add-to-list 'org-capture-templates
	       '("c" "Cookbook" entry
		 (file org-chef-recipe-book)
		 "%(org-chef-get-recipe-from-url)"
		 :empty-lines 1)
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

** Notes"
		 :empty-lines 2)
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

(use-package org-mindmap
  :vc (:url "https://github.com/krvkir/org-mindmap.git" :rev :newest)
  :after org
  :bind
  (:map org-mindmap-mode-map
        ("C-c m c" . org-mindmap-insert-child)
        ("C-c m s" . org-mindmap-insert-sibling)
        ("C-c m d" . org-mindmap-delete-node)
        ("C-c m v" . org-mindmap-switch-layout)
        ("C-c m p" . org-mindmap-switch-compaction)
        ("C-c m m" . org-mindmap-list-to-mindmap)
        ("C-c m l" . org-mindmap-to-list))
  :config
  (add-hook 'org-mode-hook #'org-mindmap-mode))

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

(use-package ox-rapport
  :ensure nil
  :after ox-latex)

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
  :bind (("C-c a" . my/org-agenda-list)
         :map org-agenda-mode-map
         ("q" . org-agenda-exit)
	 ("RET" . cpj/org-agenda-return))
  :hook ((org-agenda-finalize . cpj/org-agenda-register-diary-buffer)
	 (org-agenda-finalize . cpj/org-agenda-set-header)
	 (org-agenda-mode . hl-line-mode))
  :custom
  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-span 'fortnight)
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
                      :weight 'normal))

(use-package calfw :defer t)

(use-package maccalfw
  :ensure nil
  :load-path "opt/maccalfw"
  :defer t)

(use-package calendar-data
  :ensure nil
  :commands (calendar-data-refresh
             calendar-data-refresh-if-stale)
  :custom
  (calendar-data-file cpj/calendar-data-file)
  (calendar-data-calendar-names
   (list user-gmail
	 "Birthdays"
	 "Home"
	 "Family"))
  (calendar-data-past-days 30)
  (calendar-data-future-days 365))

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

(use-package ind ; usr/
  :ensure nil
  :commands (ind
             ind-extended
             ind-diagnostics)
  :custom
  (ind-show-lodge-dates t)
  :config
  (dolist (lodge user-lodges)
    (add-to-list 'ind-lodge-dates lodge t)))

(use-package daily-info ; etc/
  :ensure nil
  :commands (di
             cpj/org-agenda-birthdays)
  :custom
  (daily-info-include-holidays nil)
  (daily-info-include-diary nil)
  :init
  (add-to-list
   'org-agenda-custom-commands
   '("b" "Birthdays"
     agenda ""
     ((org-agenda-files (list cpj/calendar-data-file))
      (org-agenda-include-diary nil)
      (org-agenda-span calendar-data-future-days)
      (org-agenda-start-on-weekday nil)
      (org-agenda-start-day "0d")
      (org-agenda-show-all-dates nil)
      (org-agenda-overriding-header "Birthdays")
      (org-agenda-skip-function
       '(org-agenda-skip-entry-if
         'notregexp
         "Birthday"))))))

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


;;; spell checking
(message "→ Configuring spellchecker.")
(keymap-global-set "<f7>" 'my/ispell-buffer)

(use-package jinx
  :demand t
  :pin gnu ; source from 'gnu' package archives only
  :if (executable-find "aspell")
  :bind
  (([remap ispell-word] . jinx-correct)
   ([remap my/ispell-buffer] . my/jinx-correct-all)
   :map jinx-mode-map
   ("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)
   ("<f7>" . my/jinx-correct-all)
   ("M-n" . jinx-next))
  :hook
  (emacs-startup . global-jinx-mode)
  (jinx-mode . my/jinx-add-ispell-localwords)
  :config
  (load "jinx-functions" nil 'nomessage)
  (setf (alist-get ?* jinx--save-keys) #'my/jinx-save-as-ispell-localword)
  (defun my/jinx-correct-all ()
    "Correct all Jinx misspellings in the current buffer."
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
	  browse-url-generic-program "open"))

;; Mail / News
(use-package gnus
  :if *natasha*
  :ensure nil
  :defer t
  :bind
  (:map gnus-summary-mode-map
	("A-<backspace>" . gnus-summary-delete-article))
  :custom
  (gnus-startup-file "~/.newsrc")
  (gnus-interactive-catchup nil)
  (gnus-interactive-exit nil)
  (gnus-message-archive-group nil)
  (gnus-novice-user nil)
  (gnus-permanently-visible-groups
   (regexp-opt '("Drafts" "INBOX" "Sent") 'symbols))
  (gnus-select-method user-gnus-select-method)
  (gnus-secondary-select-methods user-gnus-secondary-select-methods)
  (gnus-read-newsrc-file nil)
  (gnus-show-threads nil)
  (gnus-summary-line-format "%U%R%z %16&user-date; / %s\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %H:%M")
     ((+ 86400 (gnus-seconds-today)) . "Yesterday %H:%M")
     (604800 . "%A %H:%M")
     ((gnus-seconds-month) . "%A %d")
     ((gnus-seconds-year) . "%B %d")
     (t . "%b %d %Y")))
  (gnus-use-cache nil)
  (read-mail-command #'gnus)
  :hook
  (gnus-exit-gnus . cpj/gnus-export-newsrc)
  :config
  (load "gnus-functions" nil 'nomessage)
  (gnus-add-configuration
   '(only-article (vertical 1.0 (article 1.0 point))))

  (easy-menu-remove-item global-map '(menu-bar tools) 'gnus)
  (easy-menu-remove-item global-map '(menu-bar tools) 'rmail)
  (easy-menu-add-item global-map '(menu-bar tools)
		      ["Read Mail and Net News" menu-bar-read-mail
		       :help "Read your mail and network news groups"]
		      "Compose New Mail"))

(use-package nndraft
  :if *natasha*
  :ensure nil
  :defer t
  :config
  (defun cpj/nndraft-expire-without-backup-directory (orig &rest args)
    "Expire nndraft articles without using custom backup directories."
    (let ((backup-directory-alist nil))
      (apply orig args)))

  (advice-add 'nndraft-request-expire-articles
              :around #'cpj/nndraft-expire-without-backup-directory))

(use-package smtpmail
  :if *natasha*
  :ensure nil
  :custom
  (smtpmail-smtp-server user-mail-server)
  (smtpmail-smtp-service 587)
  (smtpmail-stream-type 'starttls)
  (send-mail-function #'smtpmail-send-it)
  (message-send-mail-function #'smtpmail-send-it))

(use-package ecomplete
  :ensure nil
  :custom
  (ecomplete-database-file
   (concat user-emacs-directory "var/ecompleterc")))

(use-package message
  :if *natasha*
  :ensure nil
  :custom
  (message-kill-buffer-on-exit t)
  (message-kill-buffer-query nil)
  (message-mail-alias-type 'ecomplete)
  (message-expand-name-standard-ui t)
  (message-self-insert-commands nil)
  :hook
  (message-mode . cpj/message-disable-smart-tab)
  :bind
  (:map message-mode-map
        ("A-<return>" . message-send-and-exit))
  :config
  (defun cpj/message-disable-smart-tab ()
    "Disable `smart-tab-mode' in Message buffers."
    (smart-tab-mode -1)))

;; RSS
(use-package elfeed
  :if *natasha*
  :custom
  (elfeed-db-directory (expand-file-name "var/elfeed/db/" user-emacs-directory))
  (elfeed-enclosure-default-dir
   (expand-file-name "var/elfeed/enclosures/" user-emacs-directory))
  (elfeed-log-level 'error)
  (elfeed-search-confirm-tag nil)
  (elfeed-search-remain-on-entry t)
  (elfeed-search-sort-order 'ascending)
  (elfeed-use-curl t)
  :custom-face
  (elfeed-show-title-face  ((t (:inherit elfeed-show-header-face :weight bold))))
  (elfeed-show-author-face ((t (:inherit elfeed-show-header-face))))
  (elfeed-show-date-face   ((t (:inherit elfeed-show-header-face))))
  (elfeed-show-feed-face   ((t (:inherit elfeed-show-header-face))))
  :bind
  (("C-c f" . elfeed)
   :map elfeed-search-mode-map
   ("/" . elfeed-search-live-filter)
   ("[" . beginning-of-buffer)
   ("]" . end-of-buffer)
   ("B" . cpj/elfeed-search-beginning-to-point-as-read)
   ("R" . cpj/elfeed-search-mark-all-as-read)
   ("m" . elfeed-mail-todo)
   ("s" . elfeed-toggle-star)
   :map elfeed-show-mode-map
   ("[" . beginning-of-buffer)
   ("]" . end-of-buffer)
   ("TAB" . shr-next-link)
   ("B" . cpj/elfeed-show-visit-secondary-browser)
   ("i" . cpj/elfeed-show-toggle-images)

   ([remap elfeed-show-scroll-up-or-next]
    . cpj/elfeed-show-scroll-up-half-or-next)
   ([remap elfeed-show-scroll-down-or-prev]
    . cpj/elfeed-show-scroll-down-half-or-prev))
  :hook
  (elfeed-search-update . cpj/elfeed-search-goto-top)
  (elfeed-show-update . cpj/elfeed-show-tidy-buffer)
  (elfeed-show-update . replace-garbage-chars)
  (elfeed-show-update . my/text-scale-increase)
  (elfeed-show-update . my/truncate-lines)
  :init
  (make-directory
   (expand-file-name "var/elfeed/" user-emacs-directory) t)
  (easy-menu-add-item
   global-map '(menu-bar tools)
   ["Read RSS Feeds" elfeed :help "Read RSS and Atom feeds"]
   "Directory Servers")
  :config
  (require 'elfeed-functions)
  (load "rc/feeds" 'noerror 'nomessage))

(use-package elfeed-daily
  :ensure nil
  :if *natasha*
  :after elfeed
  :commands cpj/elfeed-daily
  :custom
  (elfeed-search-filter "@6months +unread -daily")
  :bind
  (:map elfeed-search-mode-map
        ("c" . cpj/elfeed-search-clear-filter))
  :init
  (defalias 'db #'cpj/elfeed-daily)
  :config
  (setq elfeed-show-refresh-function #'cpj/elfeed-show-refresh))

;; Others
(use-package chess
  :if	*natasha*
  :defer t
  :custom
  (chess-images-default-size 90)     ; comfortable board scale
  (chess-images-separate-frame nil) ; obey existing frame geometry
  :config
  (defvar cpj/chess-movetime 1000
    "UCI chess engine thinking time per move, in milliseconds."))

(use-package gnugo ; Game of Go
  :if *natasha*
  :defer t
  :init
  (easy-menu-add-item  global-map '(menu-bar tools games)
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
(load "cpj-help-functions" nil 'nomessage)

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

;; Work-specific
(when *w32*
  (load (expand-file-name ".work" user-emacs-directory) 'noerror nil))

(when *mac*
  (defun my/find-work-agenda ()
    "Open the work agenda."
    (interactive)
    (find-file "/db:/!.org"))

  (keymap-global-set "C-c Z" #'my/find-work-agenda)
  (which-key-alias "C-c Z" "work-agenda"))


;;; UX
(message "→ Configuring UX.")

;;; arrow keys (Darwin)
;; <home>  is fn-left	<end>  is fn-right
;; <prior> is fn-up	<next> is fn-down

(keymap-global-set "C-<home>"  #'beginning-of-buffer)
(keymap-global-set "C-<end>"   #'my/end-of-buffer)
(keymap-global-set "C-<prior>" #'scroll-down-line)
(keymap-global-set "C-<next>"  #'scroll-up-line)

;; M-<home>		'beginning-of-buffer-other-window
;; M-<end>		'end-of-buffer-other-window
;; M-<prior>		'scroll-other-window-down
;; M-<next>		'scroll-other-window

(keymap-global-unset "M-<left>")
(keymap-global-unset "M-<right>")

(keymap-global-set "M-[" #'my/backward-page)
(keymap-global-set "M-]" #'my/forward-page)

(keymap-global-set "<remap> <backward-paragraph>" #'my/backward-paragraph)
(keymap-global-set "<remap> <forward-paragraph>" #'my/forward-paragraph)

;;; scroll settings
(setq auto-window-vscroll nil
      next-screen-context-lines 0
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      scroll-step 0)

(keymap-global-set "C-<" #'scroll-left)
(keymap-global-set "C->" #'scroll-right)

;; half-scroll
(keymap-global-set "<remap> <scroll-up-command>" #'cpj/scroll-up-half)
(keymap-global-set "<remap> <scroll-down-command>" #'cpj/scroll-down-half)

(keymap-global-set "A-<up>" [prior])
(keymap-global-set "A-<down>" [next])

(when *mac*
  (keymap-global-set "s-<up>" [prior])
  (keymap-global-set "s-<down>" [next]))

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
  (keymap-global-set "<pinch>" #'ignore))

;;; window navigation
(use-package windmove
  :ensure nil
  :bind
  (("C-M-<left>". windmove-left)
   ("C-M-<right>". windmove-right)
   ("C-M-<up>". windmove-up)
   ("C-M-<down>". windmove-down)))


;;; alternate keys
(message "→ Configuring alternate keys.")

(keymap-global-set "C-s" 'isearch-forward-regexp)
(keymap-global-set "C-r" 'isearch-backward-regexp)
(keymap-global-set "M-s s" 'isearch-forward)
(keymap-global-set "M-s r" 'isearch-backward)

(keymap-global-set "C-l" 'my/recenter-top-bottom)
(keymap-global-set "C-x x s" 'save-all-unsaved)

(keymap-global-set "M-d" 'kill-word-dwim)
(keymap-global-set "M-j" 'join-line) ; default is `default-indent-new-line', see C-M-j
(keymap-global-set "M-z" 'zap-up-to-char)

(keymap-global-set "A-<return>" "M-<return>")
(keymap-global-set "A-S-<return>" "M-S-<return>")

;; https://www.matem.unam.mx/~omar/apropos-emacs.html#writing-experience
(keymap-global-set "C-d" 'delete-forward-char)      ; better replacement for delete-char
(keymap-global-set "M-c" 'capitalize-dwim)          ; capitalize-word
(keymap-global-set "M-K" 'kill-paragraph)           ; M-k capitalizes sentence
(keymap-global-set "C-x M-t" 'transpose-paragraphs) ; C-x C-t transpose-lines

(keymap-global-set "<remap> <mark-word>" 'mark-whole-word)
(keymap-global-set "<remap> <forward-word>" 'forward-to-word)

;; Disable alternate suspend-frame
(keymap-global-unset "C-x C-z")

;; Disable the "numeric argument". Prefer universal argument (C-u) prefix.
(dolist (prefix '("C-" "M-" "C-M-"))
  ;(keymap-global-unset (concat prefix "-")) ; negative-argument
  (dotimes (i 10) (keymap-global-unset (concat prefix (number-to-string i)))))

;; Disable <f10> options

;; <f10>	menu-bar-open
;; S-<f10>	context-menu-open
;; C-<f10>	buffer-menu-open
;; M-<f10>	toggle-frame-maximized

(dolist (key '("C-<f10>" "M-<f10>"))
  (keymap-global-unset key))

;; Cleanup abbrev menu
(dolist (key '("C-a" "+" "-" "'"))
  (keymap-global-unset (concat "C-x a " key)))

;; Undo/redo cleanups
(keymap-global-unset "C-_")
(keymap-global-unset "C-M-_")

(keymap-global-unset "<undo>")
(keymap-global-unset "C-x u")


;;; Shortcuts
(keymap-global-set "<f8>"	'list-bookmarks)
(keymap-global-set "<f12>" 	'list-buffers)

(keymap-global-set "C-`"	'scratch-buffer)
(keymap-global-set "C-<escape>"	'my/shell)

(keymap-global-set "S-<f1>"	'my/emacs-help)
(keymap-global-set "S-<f2>"	'shortdoc)

(keymap-global-set "C-M-;"	'my/eval-region)
(keymap-global-set "C-M-y"	'undo-yank)


;;; Ctrl-c (personal keybindings)
(keymap-global-set "C-c b"	'eww-list-bookmarks) ; WWW
(which-key-alias "C-c b" "eww-bookmarks")

(keymap-global-set "C-c c"	'calendar)

(keymap-global-set "C-c d SPC"	'display-current-date-and-time)
(keymap-global-set "C-c d e"	'insert-euro-date)
(keymap-global-set "C-c d i"	'insert-iso-date)
(which-key-alias "C-c d" "dates")
(which-key-alias "C-c d SPC" "current date/time")

(keymap-global-set "C-c e"	'elpher) ; gopher / gemini
(keymap-global-set "C-c i"	'my/init)

(keymap-global-set "C-c m"	'menu-bar-read-mail)
(which-key-alias "C-c m" "read-mail")

(keymap-global-set "C-c x #"	'number-lines-dwim)
(keymap-global-set "C-c x b"	'flush-blank-lines)
(keymap-global-set "C-c x g"	'replace-garbage-chars)
(keymap-global-set "C-c x n"	'normalize-text-dwim)
(which-key-alias "C-c x" "text")

(keymap-global-set "C-c z"	'my/agenda)


;;; Ctrl-x (buffer functions)
(keymap-global-set "C-x c"	'kill-current-buffer)

(keymap-global-set "C-x x SPC"	'toggle-cursor-off/on)
(keymap-global-set "C-x x L"	'buf-to-LF)
(keymap-global-set "C-x x V"	'view-text-file-as-info-manual)
(keymap-global-set "C-x x a"	'align-regexp)
(keymap-global-set "C-x x c"	'toggle-fill-column)
(keymap-global-set "C-x x k"	'kill-other-buffers)
(keymap-global-set "C-x x l"	'add-file-local-variable)
(keymap-global-set "C-x x m"	'move-buffer-file)
(keymap-global-set "C-x x r"	'rename-file-and-buffer)
(keymap-global-set "C-x x v"	'view-mode)
(keymap-global-set "C-x x w"	'preview-html)
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
(with-eval-after-load 'iso-transl
  (keymap-set iso-transl-ctl-x-8-map "0" "\u200B")
  (keymap-set iso-transl-ctl-x-8-map "a |" "↕")
  (keymap-set iso-transl-ctl-x-8-map "y" "✓"))

(which-key-alias "C-x 8"   "keys")
(which-key-alias "C-x 8 0" "ZWS")
(which-key-alias "C-x 8 a" "arrows")
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
(defalias 'otbl 'turn-on-orgtbl)
(defalias 'tm 'text-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'vfc 'visual-fill-column-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wm 'whitespace-mode)

(defalias 'dr 'desktop-read)
(defalias 'ds 'desktop-save)


;;; Disabled functions & safe local variables
;(setq disabled-command-function 'enable-me)
(message "→ Disabling unused functions.")

(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil) ; C-x C-l
(put 'narrow-to-region 'disabled nil) ; C-x n n
(put 'suspend-frame 'disabled nil) ; C-z
(put 'upcase-region 'disabled nil) ; C-x C-u

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-02/msg01410.html
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; Safe local variables
(dolist (value
         '((org-log-done)
           (truncate-lines . t)
           (cpj/org-sort-after-capture . t)
           (before-save-hook . (my/org-sort))
           (before-save-hook . (elisp-check-literal-form-feeds))
           (flymake-mode . nil)
           (org-comment-placeholder-mode . nil)
           (org-hide-inline-footnotes-mode . nil)
           (org-macro-display-mode . nil)
           (org-quote-indent-mode . nil)))
  (add-to-list 'safe-local-variable-values value))

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
; LocalWords:  remotehost modeline mori featurep cbc smex vc ns ime
; LocalWords:  setq's setopt mailutils imagemagick usr dunnet Async
; LocalWords:  dir fullscreen dropbox keymap toc buddhist ewth ronn
; LocalWords:  noerror formfeed hline erc bbdb newsrc laGhv
; LocalWords:  pandoc alphapapa unpackaged xml xsl xhtml nxml parens
; LocalWords:  MidnightBlue src numero documentclass subsubsection Tt
; LocalWords:  github cliplink Waterfox waterfox nov backend fboundp
; LocalWords:  windmove goto ripgrep nomessage lorem OAuth authinfo
; LocalWords:  plist nopgnos flymake api todo paren docstrings ibuf
; LocalWords:  ibuffer ish minibuffer emacsclient Uncomment maccalfw
; LocalWords:  kMDItemCFBundleIdentifier mdfind nndraft defvar nnimap
; LocalWords:  funcall ecompleterc nntp
