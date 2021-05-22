;; Emacs configuration / pjh

;; Load theme
(add-to-list 'load-path (concat user-emacs-directory "var/nano-emacs") )
(load "nano-faces")

;; Initialize terminal
(set-language-environment 'utf-8)
(set-frame-font "Inconsolata 21")
(set-background-color "Ivory")

(setq user-mail-address "cpjhenry@gmail.com")
(setq calendar-latitude 45.3)
(setq calendar-longitude -75.8)
(setq calendar-location-name "Ottawa")

; Mac command key is Super (by default)
; Mac option key is Meta (by default)
(setq ns-function-modifier 'hyper) ; Mac function key is Hyper
(setq ns-right-alternate-modifier 'alt) ; Mac right option key is Alt
(define-key key-translation-map (kbd "<H-mouse-1>") (kbd "<mouse-2>"))

(tool-bar-mode -1) 	; turn off tool bar
(scroll-bar-mode -1); turn off scrollbar
;(menu-bar-mode -1)
(toggle-frame-maximized)
(setq frame-title-format nil)
(setq ns-use-native-fullscreen t)
(setq mac-use-title-bar nil)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq pop-up-windows nil)
(setq ns-pop-up-frames nil)

;; Initialize package manager
(setq gnutls-algorithm-priority "normal:-vers-tls1.3")
(eval-and-compile
	(require 'package)
	(setq package-archives '(("org" . "https://orgmode.org/elpa/")
							("melpa" . "https://melpa.org/packages/")
							("gnu" . "http://elpa.gnu.org/packages/")))
	(setq package-archive-priorities '(("org" . 3)("melpa" . 2)("gnu" . 1)))
	(package-initialize)
	;(package-refresh-contents) ; Fetch the archive contents on startup and during compilation
	(unless (package-installed-p 'use-package)
		(package-install 'use-package))
	(require 'use-package)
	(setf use-package-always-ensure t))

;; Add directories to load-path
(eval-and-compile
	(mapc #'(lambda (path)
	(add-to-list 'load-path (expand-file-name path user-emacs-directory))) 
		'("etc" "var") ))
(setq default-directory "~/")
(use-package exec-path-from-shell :init (exec-path-from-shell-initialize) )

;; settings
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq-default tab-width 4)
(setq-default fill-column 52)
(setq-default help-window-select t)

(setq cookie-file "/usr/local/share/games/fortunes/fortunes")
(setq delete-by-moving-to-trash t)
(setq flyspell-issue-message-flag nil)
(setq ispell-list-command "--list") ; correct command
(setq ispell-program-name "aspell") ; spell checker
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq ring-bell-function 'ignore)
(setq save-abbrevs 'silent)
(setq sentence-end-double-space nil)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)
(setq trash-directory "~/.Trash")
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(setq abbrev-file-name				(concat user-emacs-directory "etc/abbrev_defs"))
(setq auto-save-list-file-prefix	(concat user-emacs-directory "var/auto-save/sessions/"))
(setq elfeed-db-directory			(concat user-emacs-directory "var/elfeed/db/"))
(setq elfeed-enclosure-default-dir	(concat user-emacs-directory "var/elfeed/enclosures/"))
(setq elfeed-score-score-file		(concat user-emacs-directory "etc/elfeed/score/score.el"))
(setq eshell-aliases-file			(concat user-emacs-directory "etc/eshell/aliases"))
(setq eshell-directory-name			(concat user-emacs-directory "var/eshell/"))
(setq eww-bookmarks-directory		(concat user-emacs-directory "var/eww") )
(setq recentf-save-file				(concat user-emacs-directory "var/recentf"))
(setq request-storage-directory		(concat user-emacs-directory "var/request/storage/"))
(setq simplenote2-directory			(concat user-emacs-directory "var/simplenote2/"))
(setq tramp-auto-save-directory		(concat user-emacs-directory "var/tramp/auto-save/"))
(setq tramp-persistency-file-name	(concat user-emacs-directory "var/tramp/persistency"))
(setq url-cache-directory			(concat user-emacs-directory "var/url/cache/"))
(setq url-configuration-directory	(concat user-emacs-directory "var/url/configuration/"))

(setq diary-file "~/Documents/diary")
;(calendar-set-date-style ‘iso)
(setq diary-show-holidays-flag nil)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(setq lunar-phase-names '(
	"● New Moon"
	"☽ First Quarter Moon"
	"○ Full Moon"
	"☾ Last Quarter Moon"))

;; backups
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; buffers
(use-package nswbuff) ; buffer switching
(setq nswbuff-clear-delay 1.5)
(setq nswbuff-display-intermediate-buffers t)
(setq nswbuff-exclude-buffer-regexps'("^ .*" "^\\*Messages\\*"))

(use-package persistent-scratch :config (persistent-scratch-setup-default) )

;; remove unneeded messages and buffers
(setq inhibit-startup-message t) 	; 'About Emacs'
(setq inhibit-startup-echo-area-message t)
(eval-after-load "startup" '(fset 'display-startup-echo-area-message (lambda ())))

(setq initial-scratch-message nil) 	; Makes *scratch* empty
(setq-default message-log-max nil) 	; Removes *Messages* from the buffer
(add-hook 'after-init-hook (kill-buffer "*Messages*"))
(add-hook 'minibuffer-exit-hook 	; Removes *Completions* buffer when done
	'(lambda () (let ((buffer "*Completions*"))
		(and (get-buffer buffer)
		(kill-buffer buffer)))))

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ; Don't show *Buffer list*
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

;; file and buffer functions
(load "init-filesandbuffers")
(recentf-mode t)
(run-at-time nil (* 5 60) 'recentf-save-list)
(add-to-list 'recentf-exclude ".emacs.d/elpa/")
(add-to-list 'recentf-exclude ".emacs.d/etc/")
(add-to-list 'recentf-exclude ".emacs.d/var/")

;; print functions
(load "init-page-dimensions")
(setq printer-name "Brother_HL_L2370DW_series")
(setq ps-paper-type 'a5)
(setq ps-lpr-switches '("-o media=a5"))
(setq ps-font-size 11)

(setq ps-left-margin 36)
(setq ps-right-margin 36)
(setq ps-top-margin 36)
(setq ps-bottom-margin 36)

(setq ps-print-header nil)
(setq ps-print-header-frame nil)
(setq ps-header-title-font-size 9)
(setq ps-header-offset 9)
(setq ps-header-lines 1)

;; Custom variables
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Mode Line
(use-package smart-mode-line :config (sml/setup))
(add-to-list 'sml/replacer-regexp-list '("^:Doc:org/" ":org:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/" ":DocProj:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Reference/" ":DocRef:") t)
(add-to-list 'sml/replacer-regexp-list '("^.*/gemini/" ":gem:") t)
(add-to-list 'sml/replacer-regexp-list '("^.*City of Ottawa/" ":CoO:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CoO:Operations/" ":Ops:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CoO:PDG/" ":PDG:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:1-.*/" ":PDG-1:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:2-.*/" ":PDG-2:") t)
(add-to-list 'sml/replacer-regexp-list '("^:PDG:3-.*/" ":PDG-3:") t)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode)
(display-battery-mode)

;; Tabs
;(load "init-tabs")

;; Emacs server
(defun server-shutdown ()
	"Save buffers, Quit, and Shutdown (kill) server."
	(interactive)
	(save-some-buffers)
	(kill-emacs))

;; Startup time
(defun efs/display-startup-time ()
	(message "Emacs loaded in %s with %d garbage collections."
	(format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
	gcs-done))
(add-hook 'emacs-startup-hook 'efs/display-startup-time)

;; Today's cookie
(defun todayscookie () (message (cookie "/usr/local/share/games/fortunes/fortunes")))
(add-hook 'window-setup-hook 'todayscookie)


;; Initialize packages
(use-package elfeed)
(load "rc-elfeed") ; feeds config
(eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
(setq elfeed-use-curl t)
(easy-menu-add-item  nil '("tools") ["Read web feeds" elfeed t])
(defun elfeed-mark-all-as-read ()
	(interactive)
	(mark-whole-buffer)
	(elfeed-search-untag-all-unread) )
(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

(use-package elpher)
(setq elpher-bookmarks-file (concat user-emacs-directory "var/elpher-bookmarks") )
(add-hook 'elpher-mode-hook (lambda () 
	(local-set-key (kbd "A-<left>") 'elpher-back)
	(local-set-key (kbd "A-<up>")   'scroll-down-command)
	(local-set-key (kbd "A-<down>") 'scroll-up-command)
	(setq-local left-margin-width 15)
	(setq-local gnutls-verify-error nil)
	(set-window-buffer nil (current-buffer)) ))
(easy-menu-add-item  nil '("tools") ["Gopher" elpher t])

(load "rc-erc") ; irc config
(easy-menu-add-item  nil '("tools")	["IRC with ERC" erc t])

(setq browse-url-browser-function 'browse-url-generic ; eww
	browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox")

(advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
(defun elpher:eww-browse-url (original url &optional new-window)
	"Handle gemini links."
	(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
	(use-package elpher) ; r
	(elpher-go url))
	(t (funcall original url new-window))))
(add-hook 'eww-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>") 'eww-back-url) ))

(use-package gnugo) ; Game of Go
(setq gnugo-program "/usr/local/bin/gnugo")
(easy-menu-add-item  nil '("tools" "games") ["Go" gnugo t])

(use-package simplenote2)
(load "rc-sn")
(simplenote2-setup)
(setq simplenote2-markdown-notes-mode 'markdown-mode)
(add-hook 'simplenote2-create-note-hook (lambda () (simplenote2-set-markdown) ))

(use-package smooth-scrolling :config (smooth-scrolling-mode))
(use-package ssh)
(use-package wc-mode)
(use-package which-key :config (which-key-mode))


;; Lisp & Help modes
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(setq show-paren-style 'mixed)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

(use-package form-feed) ; navigate using ^L
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'help-mode-hook 'form-feed-mode)
(add-hook 'Info-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>" ) 'Info-history-back)
	(local-set-key (kbd "A-<right>") 'Info-history-forward) ))


;; Org-mode
(require 'org)
(setq org-directory "~/Documents/org")
(setq org-agenda-files (list org-directory))
(setq org-agenda-diary-file (concat org-directory "/diary.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents")
(setq org-mobile-inbox-for-pull "from-mobile.org")
;(use-package org-mobile-sync :config (org-mobile-sync-mode 1))

(setq org-startup-folded 'content) ; folded children content all
(setq org-startup-truncated nil) ; fix org-mode table wrapping
(setq org-catch-invisible-edits 'smart)
(setq org-ctrl-k-protect-subtree t)
(setq org-enable-priority-commands nil)
(setq org-export-with-toc nil)
(setq org-footnote-auto-adjust t)
(setq org-log-done t)
(setq org-special-ctrl-a/e t)

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
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
(use-package org-chef :ensure t)
(add-hook 'org-mode-hook 'org-indent-mode)

(load "org-phscroll.el") ; org-table fix
(load "init-org-mode") ; org-mode functions


;; Emacs Text and Markdown modes
(add-hook 'text-mode-hook (lambda ()
	(abbrev-mode)
	(flyspell-mode)
	(visual-line-mode)
	(wc-mode) ))
(eval-after-load "flyspell" '(progn 
	(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)))

(use-package olivetti
	:init (setq olivetti-body-width 80) )

(use-package markdown-mode
	:commands (markdown-mode gfm-mode)
	:init (setq markdown-command "multimarkdown")
		(setq markdown-enable-prefix-prompts nil)
		(setq markdown-hide-urls t)
	:config	(add-to-list 'markdown-uri-types "gemini")		
	:mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode)) )
(add-hook 'markdown-mode-hook (lambda ()
	(setq-local left-margin-width 15) )) ;(setq-local right-margin-width 15)

(load "init-text") ; text functions
(create-scratch-buffer)

;; buffer movement
(require 'windmove) ; use alt + arrow keys to switch between visible buffers
(windmove-default-keybindings 'meta) ; ‘M-left’ and ‘M-right’ to switch windows

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)  

;; arrow keys (Darwin)
;; <home>  is fn-left	<end> is fn-right
;; <prior> is fn-up		<next> is fn-down
(put 'scroll-left 'disabled nil)
(global-set-key (kbd "<home>") 'scroll-right)
(global-set-key (kbd "<end>" ) 'scroll-left)
; <prior>	'scroll-down-command
; <next>	'scroll-up-command

(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-M-<left>") 'backward-sentence)
(global-set-key (kbd "s-M-<right>") 'forward-sentence)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)
(global-set-key (kbd "s-<prior>") 'backward-page) ; s-H-up
(global-set-key (kbd "s-<next>") 'forward-page) ; s-H-down

;; alternate keys
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-X C-g") 'recentf-open-files-compl)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-z") 'undo)

;; Shortcuts
(global-set-key (kbd "s-1") (kbd "C-x 1"))
(global-set-key (kbd "s-2") (kbd "C-x o C-x 1"))
(global-set-key (kbd "s-0") (kbd "C-x 0"))
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "s-o") 'find-file)				; override file-dialogs
(global-set-key (kbd "s-S") 'write-file)
(global-unset-key (kbd "s-m"))

(global-set-key (kbd "s-K") 'nuke-all-buffers)
(global-set-key (kbd "s-b") 'create-scratch-buffer)
(global-set-key (kbd "s-n") 'new-empty-buffer)
(global-set-key (kbd "s-w") 'kill-current-buffer)	; "s-k"
(global-set-key (kbd "C-<tab>") 'nswbuff-switch-to-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'nswbuff-switch-to-previous-buffer)

(global-set-key (kbd "C-c #") 'toggle-truncate-lines)
(global-set-key (kbd "C-c D") 'insert-iso-date)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c e") 'erc)					; IRC
(global-set-key (kbd "C-c f") 'elfeed)
(global-set-key (kbd "C-c g") 'elpher)				; gopher / gemini
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c m") 'markdown-mode)
(global-set-key (kbd "C-c o") 'markdown-preview-file) 
(global-set-key (kbd "C-c t") 'olivetti-mode)
(global-set-key (kbd "C-c w") 'eww-list-bookmarks)	; www

(global-set-key (kbd "TAB") 'insert-tab-char)		; same as Ctrl+i
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "M-p") 'lpr-buffer)
(global-set-key (kbd "s-p") 'ps-print-buffer)

(global-set-key (kbd "H-a") (kbd "C-c a a"))
(global-set-key (kbd "H-c") 'calendar)
(global-set-key (kbd "H-d") (lambda() (interactive) (find-file "~/Documents/org/daily.org")) )
(global-set-key (kbd "H-w") (lambda() (interactive) (find-file "~/Documents/!dbin/words.org")) )

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer

(defalias 'ds 'desktop-save)
(defalias 'dsm 'desktop-save-mode)
(defalias 'lcd 'list-colors-display)
(defalias 'li 'lorem-ipsum-insert-paragraphs)
(defalias 'ppc 'ps-print-customize)
(defalias 'rs 'replace-string)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'flym 'flyspell-mode)
(defalias 'fm 'fundamental-mode)
(defalias 'hm 'html-mode)
(defalias 'jsm 'js-mode)
(defalias 'mm 'markdown-mode)
(defalias 'olv 'olivetti-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'vlm 'visual-line-mode)

(defalias 'sn 'simplenote2-list)
(defalias 'snsm 'simplenote2-set-markdown)
