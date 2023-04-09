;; Emacs configuration / pjh
(defconst *mac* (eq system-type 'darwin))
(defconst *gnu* (eq system-type 'gnu/linux))
(defconst *w32* (eq system-type 'windows-nt))

(defconst *bullwinkle* (string-equal (system-name) "bullwinkle.local"))
(defconst *natasha* (string-equal (system-name) "natasha.local"))

;; Initialize terminal
(set-language-environment 'utf-8)

(when *mac*
	(set-frame-font "Inconsolata 21")
	; Mac command key is Super
	; Mac option  key is Meta
	; Mac control key is Control
	(setq mac-function-modifier 'hyper) 	; Hyper
	(setq mac-right-command-modifier 'alt)	; Alt
	(setq mac-right-option-modifier nil)
	(define-key key-translation-map (kbd "<C-mouse-1>") (kbd "<mouse-2>"))
	(message "[Darwin]"))
(when *gnu*
	(set-frame-font "Monospace 17")
	(message "[gnu]"))
(when *w32*
	(set-frame-font "Cascadia Mono 17")
	(setq w32-lwindow-modifier 'super)
	(setq w32-pass-lwindow-to-system nil)
	(message "[w32]"))
(set-background-color "Ivory")

(setq user-mail-address "cpjhenry@gmail.com")
(setq calendar-latitude 45.3)
(setq calendar-longitude -75.8)
(setq calendar-location-name "Ottawa")

;; Initialize package manager
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(setq package-enable-at-startup nil)
(when (>= emacs-major-version 24)
	(require 'package)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) )
	(unless (package-installed-p 'use-package) (package-install 'use-package) )
	(require 'use-package)
	(setf use-package-always-ensure t)

;; Add directories to load-path
(add-to-list 'load-path (expand-file-name "etc" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "opt" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "var" user-emacs-directory))

(when *mac*
	(setq default-directory "~/")
	(setq exec-path '(".local/" "/Users/cpjh/bin/" "/Library/TeX/texbin/" "/usr/local/opt/qt@5/bin/"
					"/usr/local/opt/python@3/libexec/bin/" "/usr/local/MacGPG2/bin/" "/usr/libexec/" 
					"/usr/local/opt/gnu-sed/libexec/gnubin/" "/usr/local/opt/coreutils/libexec/gnubin/" 
					"/usr/local/bin/" "/usr/local/sbin/" "/usr/bin/" "/usr/sbin/" "/bin/" "/sbin/" 
					"/Applications/Emacs.app/Contents/MacOS/libexec/")) )

(package-initialize t) ; instead of (package-initialize)
;(package-refresh-contents) ; Fetch the archive contents on startup

;; settings
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq-default tab-width 4)
(setq-default fill-column 55)
(setq-default help-window-select t)

(setq backup-by-copying t)
(setq bookmark-save-flag 1)
(setq bookmark-set-fringe-mark nil)
(setq delete-by-moving-to-trash t)
(setq dictionary-server "dict.org")
(setq flyspell-issue-message-flag nil)
(setq ispell-list-command "--list") ; correct command
(setq ispell-program-name "aspell") ; spell checker
(setq ring-bell-function 'ignore)
(setq save-abbrevs 'silent)
(setq sentence-end-double-space nil)
(setq show-paren-style 'mixed)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)		; C-x C-f /remotehost:filename
(setq trash-directory "~/.Trash")
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; frames
(setq frame-title-format nil)
(setq ns-use-native-fullscreen t)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq use-short-answers t)
(setq pop-up-windows nil)
(setq ns-pop-up-frames nil)

(when *mac* (setq mac-use-title-bar nil))
(when (display-graphic-p)(tool-bar-mode -1))
(scroll-bar-mode -1)
(toggle-frame-maximized)
(electric-indent-mode -1)

;; files
(setq abbrev-file-name				(concat user-emacs-directory "etc/abbrev_defs"))
(setq auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/"))
(setq bookmark-default-file			(concat user-emacs-directory "etc/bookmarks"))
(setq eshell-aliases-file			(concat user-emacs-directory "etc/eshell/aliases"))
(setq eshell-directory-name			(concat user-emacs-directory "var/eshell/"))
(setq eww-bookmarks-directory 		(concat user-emacs-directory "etc/"))
(setq request-storage-directory		(concat user-emacs-directory "var/request/storage/"))
(setq tramp-auto-save-directory		(concat user-emacs-directory "var/tramp/auto-save/"))
(setq tramp-persistency-file-name	(concat user-emacs-directory "var/tramp/persistency"))
(setq url-cache-directory			(concat user-emacs-directory "var/url/cache/"))
(setq url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))

;; calendar
(setq diary-file "~/Documents/diary")
(setq diary-show-holidays-flag nil)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(setq lunar-phase-names '(
	"● New Moon"
	"☽ First Quarter Moon"
	"○ Full Moon"
	"☾ Last Quarter Moon"))

(setq calendar-christian-all-holidays-flag t)
(setq calendar-chinese-all-holidays-flag t)
(setq holiday-general-holidays nil)
(setq holiday-bahai-holidays nil)
;(setq holiday-hebrew-holidays nil)
;(setq holiday-islamic-holidays nil)
(setq holiday-local-holidays '( ; National / Provincial Holidays and Commemorations
	(holiday-fixed 01 01  "New Year's Day")
	(holiday-fixed 02 02  "Groundhog Day")
	(holiday-fixed 02 14  "Valentine's Day")
	(holiday-fixed 04 01  "April Fools' Day")
	(holiday-float 05 0 2 "Mother's Day")
	(holiday-float 06 0 3 "Father's Day")
	(holiday-fixed 07 01  "Canada Day")
	(holiday-float 08 1 1 "Civic Holiday")
	(holiday-float 09 1 1 "Labour Day")
	(holiday-float 10 1 2 "Thanksgiving")
	(holiday-fixed 10 31  "Halloween")
	(holiday-fixed 11 11  "Remembrance/Memorial Day")

	(holiday-fixed 01 21  "Lincoln Alexander Day")
	(holiday-float 02 1 3 "Family Day")
	(holiday-fixed 02 15  "National Flag Day")
	(holiday-float 03 1 2 "Commonwealth Day")
	(holiday-fixed 04 06  "Tartan Day")
	(holiday-fixed 04 09  "Vimy Ridge Day")
	(holiday-fixed 06 21  "Indigenous Peoples Day")
	(holiday-fixed 06 24  "Midsummer Day")
	(holiday-fixed 09 30  "Truth and Reconciliation")
	(holiday-fixed 12 11  "Statute of Westminster")))

(setq calendar-mark-holidays-flag t)
(setq calendar-view-holidays-initially-flag t)
(defun list-hols () (interactive) (list-holidays (string-to-number (format-time-string "%Y"))))

(setq zoneinfo-style-world-list '(
	("America/Vancouver" "Vancouver")
	("America/Edmonton" "Edmonton")
	("America/Toronto" "Ottawa")
	("America/Halifax" "Halifax")
	("America/St_Johns" "St. John's")
	("America/Marigot" "St. Martin")
	("Europe/London" "Edinburgh")
	("Europe/Lisbon" "Lisbon")
	("Europe/Paris" "Paris")
	("Europe/Istanbul" "Ankara")
	("Asia/Calcutta" "Bangalore")
	("Asia/Shanghai" "Beijing")
	("Asia/Tokyo" "Tokyo")))

;; backups
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)


;; buffers
(use-package nswbuff) ; buffer switching
(setq nswbuff-clear-delay 1.5)
(setq nswbuff-display-intermediate-buffers t)
(setq nswbuff-exclude-buffer-regexps '(
	"^ .*"
	"^\\*Help\\*"
	"^\\*Messages\\*"
	"^\\*Shell Command Output\\*"
	"from-mobile.org"
	"^\\*tramp/.*"))

(use-package persistent-scratch :config (persistent-scratch-setup-default))
(use-package unkillable-scratch :ensure t :config (unkillable-scratch t)
	:init (setq unkillable-scratch-do-not-reset-scratch-buffer t))

(easy-menu-add-item  nil '("Buffers") ["Increase text size" text-scale-increase])
(easy-menu-add-item  nil '("Buffers") ["Decrease text size" text-scale-decrease])

;; remove unneeded messages and buffers
(setq inhibit-startup-message t) 	; 'About Emacs'
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil) 	; Makes *scratch* empty
(add-hook 'minibuffer-exit-hook 	; Removes *Completions* buffer when done
	(lambda () (let ((buffer "*Completions*")) (and (get-buffer buffer) (kill-buffer buffer)))))

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ; Don't show *Buffer list*
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

;; don't load default init file
(setq inhibit-default-init t)

;; file and buffer functions
(load "init/filesandbuffers")
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)
(easy-menu-add-item  nil '("file" "print") ["Enscript" spool-to-enscript t])
(easy-menu-add-item  nil '("file" "print") ["Enscript (region)" spool-to-enscript-region t])
(define-key menu-bar-print-menu [print-buffer] nil)
(define-key menu-bar-print-menu [print-region] nil)
(define-key menu-bar-print-menu [ps-print-buffer] nil)
(define-key menu-bar-print-menu [ps-print-region] nil)

;; print functions
(load "init/page-dimensions")
(defun toggle-fill-column ()
    "Toggle fill-column values between 32 and 55"
    (interactive)
    (setq fill-column (if (= fill-column 55) 32 55))
	(message "fill-column set to: %s" fill-column))

(when *mac*
	(setq printer-name "Brother_HL_L2370DW")
	(setq ps-paper-type 'a5)
	(setq ps-lpr-switches '("-o media=a5"))
	(setq ps-font-size 11)
	(setq ps-print-color-p nil)
	(setq ps-print-header nil)
	(setq ps-print-footer nil) 
	(setq ps-left-margin 28)
	(setq ps-right-margin 28)
	(setq ps-top-margin 28)
	(setq ps-bottom-margin 28)
	)

;; Custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Mode Line
(use-package smart-mode-line :config (sml/setup))
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Notes/" ":Notes:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:org/" ":org:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/" ":Proj:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Reference/" ":Ref:") t)
(add-to-list 'sml/replacer-regexp-list '("^.*/gemini/" ":gem:") t)

(when *natasha*
(add-to-list 'sml/replacer-regexp-list '("^.*/Work/" ":Work:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Work:Operations/" ":Ops:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Work:PDG/" ":PDG:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:1-.*/" ":PDG-1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:2-.*/" ":PDG-2:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:3-.*/" ":PDG-3:") t) )

(setq sml/col-number-format "%2C")
(setq battery-mode-line-format " %p ")
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(column-number-mode)
(display-battery-mode)
(display-time-mode)

;; Startup time
(defun efs/display-startup-time ()
	(message "Emacs loaded in %s with %d garbage collections."
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)

;; Today's cookie
(when *mac*
	(setq cookie-file "/usr/local/share/games/fortunes/fortunes")
	(setq fortune-dir "/usr/local/share/games/fortunes/")
	(defun todayscookie () (message (cookie cookie-file)))
	(add-hook 'window-setup-hook 'todayscookie))

;; Disable ISO-Translat
;(load "init/iso-transl")


;; Initialize packages
(use-package elpher)
(setq elpher-bookmarks-file (concat user-emacs-directory "var/elpher-bookmarks"))
(add-hook 'elpher-mode-hook (lambda () 
	(local-set-key (kbd "A-<left>") 'elpher-back)
	(local-set-key (kbd "A-<up>")   'scroll-down-command)
	(local-set-key (kbd "A-<down>") 'scroll-up-command)
	(setq-local left-margin-width 10)
	(setq-local gnutls-verify-error nil)
	(set-window-buffer nil (current-buffer)) ))
(easy-menu-add-item  nil '("tools") ["Gopher" elpher t])

(advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
(defun elpher:eww-browse-url (original url &optional new-window)
	"Handle gemini links."
	(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
	(use-package elpher) ; r
	(elpher-go url))
	(t (funcall original url new-window))))
(add-hook 'eww-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>") 'eww-back-url) ))

(use-package google-this
	:config (google-this-mode))
(use-package lorem-ipsum)
(use-package page-break-lines
	:init	(setq page-break-lines-max-width 80)
	:config	(global-page-break-lines-mode))
(use-package smooth-scrolling
	:config (smooth-scrolling-mode))
(use-package ssh)
(use-package wc-mode)
(use-package which-key
	:config (which-key-mode));(which-key-setup-side-window-right-bottom)


;; Lisp & Help modes
(add-hook 'emacs-lisp-mode-hook (lambda ()
	(prettify-symbols-mode)
	(show-paren-mode)
	))
(add-hook 'help-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>" ) 'help-go-back)
	(local-set-key (kbd "A-<right>") 'help-go-forward)
	))
(add-hook 'Info-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>" ) 'Info-history-back)
	(local-set-key (kbd "A-<right>") 'Info-history-forward)
	))


;; Org-mode
(use-package org)
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (list (concat org-directory "daily.org")))
(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-startup-folded 'content)		; folded children content all
(setq org-catch-invisible-edits 'smart)
(setq org-ctrl-k-protect-subtree t)
(setq org-ellipsis "…")
(setq org-enable-priority-commands nil)
(setq org-export-preserve-breaks t)
(setq org-export-with-toc nil)
(setq org-footnote-auto-adjust t)
(setq org-log-done t) 					; 'CLOSED' logging
(setq org-log-state-notes-into-drawer nil)			
(setq org-log-repeat nil) 				; don't log repeating
(setq org-special-ctrl-a/e t)
(setq org-support-shift-select t)
(setq org-tags-exclude-from-inheritance '("PROJECT"))

(setq org-agenda-include-diary nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-start-on-weekday nil)
(add-hook 'org-agenda-finalize-hook 'delete-other-windows)

;(setq org-odt-convert-process "unoconv")
;(setq org-odt-preferred-output-format "docx")

(use-package org-autolist)
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)) )
(use-package org-chef :ensure t)
(add-hook 'org-mode-hook 'org-indent-mode)

(load "init/org-mode")	; org-mode functions
(load "org-phscroll")	; org-table fix


;; Emacs Text and Markdown modes
(add-hook 'text-mode-hook (lambda ()
	(abbrev-mode)
	(flyspell-mode)
	(visual-line-mode)
	(wc-mode) ))
(eval-after-load "flyspell" '(progn 
	(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)))

(use-package olivetti
	:init	(setq olivetti-body-width 80) )

(use-package markdown-mode
	:commands (markdown-mode gfm-mode)
	:init	(setq markdown-command "multimarkdown")
			(setq markdown-enable-prefix-prompts nil)
			(setq markdown-hide-urls t)
	:config	(add-to-list 'markdown-uri-types "gemini")		
	:mode	(("README\\.md\\'" . gfm-mode)
			 ("\\.md\\'" . markdown-mode)
			 ("\\.markdown\\'" . markdown-mode)) )
;(add-hook 'markdown-mode-hook (lambda ()
;	(setq-local left-margin-width 15) )) ;(setq-local right-margin-width 15)

(load "init/text") ; text functions
(load "init/pdfexport") ; pdf functions
(create-scratch-buffer)


;; Select Load
(when *natasha*
	(setq browse-url-browser-function 'browse-url-generic ; eww
	browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox")

	(use-package elfeed)
	(load "rc/elfeed" 'noerror) ; feeds config
	(eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
	(setq elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/"))
	(setq elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/"))
	(setq elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el"))
	(setq elfeed-use-curl t)
	(easy-menu-add-item  nil '("tools") ["Read web feeds" elfeed t])
	(defun elfeed-mark-all-as-read ()
		(interactive)
		(mark-whole-buffer)
		(elfeed-search-untag-all-unread) )
	(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)
	(global-set-key (kbd "C-c f") 'elfeed)

	(load "rc/erc" 'noerror) ; irc config
	(easy-menu-add-item  nil '("tools")	["IRC with ERC" erc t])
	(global-set-key (kbd "C-c e") 'erc)
	)

(when *mac*
	(load "init/deft") ; note functions (bound to <f8>

	(use-package gnugo) ; Game of Go
	(setq gnugo-program "/usr/local/bin/gnugo")
	(easy-menu-add-item  nil '("tools" "games") ["Go" gnugo t])
	)


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

;; buffer movement (option + arrows switches between visible buffers)
(require 'windmove)
(windmove-default-keybindings 'meta)


;; alternate keys
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-S-k")     'kill-whole-line)
(global-set-key (kbd "C-x k")     'kill-current-buffer)
(global-set-key (kbd "C-x M-k")	  'nuke-all-buffers)
(global-set-key (kbd "C-x x r")   'rename-file-and-buffer)

(global-set-key (kbd "C-s")       'isearch-forward-regexp)
(global-set-key (kbd "C-r")       'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")     'isearch-forward)
(global-set-key (kbd "C-M-r")     'isearch-backward)

(global-set-key (kbd "C-z")       'undo)
(global-set-key (kbd "C-S-z")     'undo-redo)

(global-set-key (kbd "A-<return>") (kbd "M-<return>"))

;; Darwin overrides
(global-set-key   (kbd "s-o")     'find-file)
(global-set-key   (kbd "s-S")     'write-file)
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-p"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))


;; Diabled keys
(put 'upcase-region 'disabled nil)						; C-x C-u
(put 'downcase-region 'disabled nil)					; C-x C-l


;; Shortcuts

(global-set-key (kbd "TAB")       'self-insert-command)	; 'tab-to-tab-stop
(global-set-key (kbd "C-<tab>")   'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'nswbuff-switch-to-previous-buffer)
(bind-key "<f7>" 'list-bookmarks)

(global-set-key (kbd "M-Q")       'unfill-paragraph)
(global-set-key (kbd "M-p")       'spool-to-enscript)
(global-set-key (kbd "M-P")       'spool-to-enscript-region)

(global-set-key (kbd "C-c a")     'org-agenda)
(global-set-key (kbd "C-c c")     'org-capture)
(global-set-key (kbd "C-c D")     'insert-iso-date)
(global-set-key (kbd "C-c d")     'insert-date)
(global-set-key (kbd "C-c g")     'elpher)				; gopher / gemini
(global-set-key (kbd "C-c l")     'org-store-link)
(global-set-key (kbd "C-c o")     'markdown-preview-file) 
(global-set-key (kbd "C-c q")     'replace-smart-quotes)
(global-set-key (kbd "C-c w")     'eww-list-bookmarks)	; www
(bind-key "C-c ?" 'describe-personal-keybindings)

(global-set-key (kbd "C-c b m")   'new-markdown-buffer)
(global-set-key (kbd "C-c b n")   'new-empty-buffer)
(global-set-key (kbd "C-c b s")   'create-scratch-buffer)

(global-set-key (kbd "C-c x a")   '("archive-subtree" . org-archive-subtree-default))
(global-set-key (kbd "C-c x c")   (kbd "✓"))
(global-set-key (kbd "C-c x l")   'dictionary-search)

(when *mac*
(global-set-key (kbd "C-c x d")   '("daily"  . (lambda()(interactive)(find-file "~/Documents/org/daily.org"))))
(global-set-key (kbd "C-c x e")   '("init"   . (lambda()(interactive)(find-file "~/.emacs.d/init.el"))))
(bind-key "C-c i" 'display-fill-column-indicator-mode)
(bind-key "<f6>"  'toggle-fill-column)
)
(when *natasha*
(global-set-key (kbd "C-c x o")   '("office" . (lambda()(interactive)(find-file "~/OD/Work/work.org"))))
(bind-key "<f9>" 'load-simplenote)(defun load-simplenote()(interactive)(load "init/sn"))
)


;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(defalias 'cal 'calendar)
(defalias 'clock 'world-clock)
(defalias 'ds 'desktop-save)
(defalias 'dsm 'desktop-save-mode)
(defalias 'dfc 'display-fill-column-indicator-mode)
(defalias 'er 'eval-region)
(defalias 'fbl 'flush-blank-lines)
(defalias 'lcd 'list-colors-display)
(defalias 'lh 'list-hols)
(defalias 'li 'lorem-ipsum-insert-paragraphs)
(defalias 'ppc 'ps-print-customize)
(defalias 'rs 'replace-string)
(defalias 'rsq 'replace-smart-quotes)

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
