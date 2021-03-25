;; Emacs configuration / pjh

;; Initialize terminal
(set-language-environment 'utf-8)
(set-frame-font "Inconsolata 21")
(set-background-color "ivory")

(setq user-mail-address "cpjhenry@gmail.com")
(setq calendar-latitude 45.3)
(setq calendar-longitude -75.8)
(setq calendar-location-name "Ottawa")

; Mac option key is Meta (by default)
; Mac command key is Super (by default)
(setq ns-function-modifier 'hyper) ; Mac function key is Hyper
(setq ns-right-alternate-modifier 'alt) ; Mac right option key is Alt
;(mouse-avoidance-mode 'banish) ; jump to corner when typing

(tool-bar-mode -1) 	; turn off tool bar
(scroll-bar-mode -1); turn off scrollbar
;(menu-bar-mode -1)
(toggle-frame-maximized)
(set-default 'frame-title-format "")

(setq initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)

;; Add directories to load-path
(eval-and-compile
	(mapc #'(lambda (path)(add-to-list 'load-path (expand-file-name path user-emacs-directory))) '(
		"init"
		"site-lisp"
		"site-lisp/sunrise-commander" )))
(setq default-directory "~/")
(setenv "PATH" (concat "/usr/local/bin/" ":" (getenv "PATH")))

(setq diary-file "~/.emacs.d/diary")
(setq diary-show-holidays-flag nil)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(setq lunar-phase-names '(
	"● New Moon"
	"☽ First Quarter Moon"
	"○ Full Moon"
	"☾ Last Quarter Moon"))

;; Initialize package manager
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

;; settings
(setq-default tab-width 4)
(setq-default fill-column 52)

(setq ispell-list-command "--list") ; correct command
(setq ispell-program-name "/usr/local/bin/aspell") ; spell checker
(setq ring-bell-function 'ignore)
(setq sentence-end-double-space nil)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'simplified)
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;(setq shell-file-name "/usr/local/bin/bash") ; force full subshell
;(setq shell-command-switch "-ic")

;; backups
(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'backup-each-save)
(add-hook 'after-save-hook 'backup-each-save)

;; remove unneeded buffers
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t) 	; 'About Emacs'
(setq initial-scratch-message nil) 	; Makes *scratch* empty
(setq-default message-log-max nil) 	; Removes *messages* from the buffer
(kill-buffer "*Messages*")
(add-hook 'minibuffer-exit-hook 	; Removes *Completions* buffer when done
	'(lambda () (let ((buffer "*Completions*"))
		(and (get-buffer buffer)
		(kill-buffer buffer)))))

;; opening multiple files
(setq inhibit-startup-buffer-menu t) ; Don't show *Buffer list*
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

;; file and buffer functions
(load "filesandbuffers.el")
(load "print2pdf.el")

;; print functions
(load "page-dimensions.el")
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
(if (file-exists-p custom-file) (load custom-file))

;; Mode Line
(use-package smart-mode-line
	:config (sml/setup))
(add-to-list 'sml/replacer-regexp-list '("^:Doc:org/" ":org:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/" ":DocProj:") t)
(add-to-list 'sml/replacer-regexp-list '("^.*/gemini/" ":gem:") t)
(add-to-list 'sml/replacer-regexp-list '("^.*City of Ottawa/" ":CoO:") t)
(add-to-list 'sml/replacer-regexp-list '("^:CoO:Operations/" ":Ops:") t)
(add-to-list 'sml/replacer-regexp-list '("^:Ops:!PDG/" ":PDG:") t)

(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode)
(display-battery-mode)

;; Tabs
;(load "centaur.el")

;; Emacs server
(defun server-shutdown ()
	"Save buffers, Quit, and Shutdown (kill) server."
	(interactive)
	(save-some-buffers)
	(kill-emacs))

;; Initialize packages
(use-package elfeed)
(load "elfeedrc.el") ; feeds config
(setq elfeed-use-curl t)
(easy-menu-add-item  nil '("tools") ["Read web feeds" elfeed t])
(defun elfeed-mark-all-as-read ()
	(interactive)
	(mark-whole-buffer)
	(elfeed-search-untag-all-unread) )
(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

(use-package elpher)
(add-hook 'elpher-mode-hook (lambda () 
	(local-set-key (kbd "A-<left>") 'elpher-back)
	(local-set-key (kbd "A-<up>")   'scroll-down-command)
	(local-set-key (kbd "A-<down>") 'scroll-up-command)
	(setq left-margin-width 15)
	(set-window-buffer nil (current-buffer)) ))
(easy-menu-add-item  nil '("tools") ["Gopher" elpher t])

(advice-add 'eww-browse-url :around 'elpher:eww-browse-url)
(defun elpher:eww-browse-url (original url &optional new-window)
	"Handle gemini links."
	(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url)
	(require 'elpher)
	(elpher-go url))
	(t (funcall original url new-window))))
(add-hook 'eww-mode-hook (lambda ()
	(local-set-key (kbd "A-<left>") 'eww-back-url)))

(load "ercrc.el") ; irc config
(easy-menu-add-item  nil '("tools")	["IRC with ERC" erc t])

(use-package gnugo) ; Game of Go
(setq gnugo-program "/usr/local/bin/gnugo")
(easy-menu-add-item  nil '("tools" "games") ["Go" gnugo t])

(setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program (concat user-emacs-directory "g-c") )

(use-package nswbuff) ; buffer switching
(use-package ssh)
(use-package vterm)

(use-package which-key)
(which-key-mode)

;; Lisp & Help modes
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(setq show-paren-style 'mixed)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)

(use-package form-feed) ; navigate using ^L
(add-hook 'emacs-lisp-mode-hook 'form-feed-mode)
(add-hook 'help-mode-hook 'form-feed-mode)

;; Org-mode
(setq org-directory "~/Documents/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-startup-folded t)

(add-hook 'org-mode-hook (lambda () (org-indent-mode) ))

(setq org-agenda-files (list (concat org-directory "/daily.org")
							 (concat org-directory "/work.org") ))
(setq org-agenda-include-diary t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-start-on-weekday nil)
(add-hook 'org-agenda-finalize-hook (lambda () (delete-other-windows)))

(setq org-todo-keywords
      '((sequence "TODO" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold)))) ; add inprogress keyword
(setq org-log-done t)

(org-link-set-parameters ; link type: gemini://host/index.gmi
	"gemini"
	:follow (lambda (path) (elpher-go (concat "gemini:" path)))
	:face '(:foreground "turquoise" :weight bold)
	:display 'full)

(org-link-set-parameters ; link type: gopher
	"gopher"
	:follow (lambda (path) (elpher-go (concat "gopher:" path)))
	:face '(:foreground "blue" :weight bold)
	:display 'full)

(load "ol-man.el") ; link type: man

(defun org-toggle-iimage-in-org ()
	"display images in your org file"
	(interactive)
	(if (face-underline-p 'org-link)
		(set-face-underline-p 'org-link nil)
		(set-face-underline-p 'org-link t))
	(iimage-mode ‘toggle))

;; see https://orgmode.org/list/87r5718ytv.fsf@sputnik.localhost
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))
(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

(add-hook 'visual-line-mode-hook
	(lambda () (when (derived-mode-p 'org-mode)
		(local-set-key (kbd "C-a") #'org-beginning-of-line)
		(local-set-key (kbd "C-e") #'org-end-of-line)
		(local-set-key (kbd "C-k") #'org-kill-line))))

;; Emacs Text mode
(use-package olivetti
	:config (progn (text-mode)
		(setf olivetti-body-width 80)
		(visual-line-mode))
	:mode ("\\.txt\\'" . olivetti-mode))
(add-hook 'markdown-mode-hook 'olivetti-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(eval-after-load "flyspell"
	'(progn
		(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
		(define-key flyspell-mouse-map [mouse-3] #'undefined)
		(setq flyspell-issue-message-flag nil)))

(use-package smooth-scrolling)
(use-package wc-mode)
(add-hook 'text-mode-hook 'wc-mode)
(load "text.el") ; text functions

;; Markdown
(use-package markdown-mode
	:commands (markdown-mode gfm-mode)
	:mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:init (setq markdown-command "/usr/local/bin/multimarkdown"))

(defun markdown-preview-file ()
	"Run Marked on the current file and revert the buffer"
	(interactive)
	(shell-command
	(format "open -a /Applications/Marked\\ 2.app %s"
		(shell-quote-argument (buffer-file-name)))) )  

;; buffers
(load "savebuffers.el") ; automatically save buffers
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

(global-set-key (kbd "s-K") 'nuke-all-buffers)
(global-set-key (kbd "s-b") 'create-scratch-buffer)
(global-set-key (kbd "s-n") 'xah-new-empty-buffer)
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
(global-set-key (kbd "C-c w") 'eww)					; www
(global-set-key (kbd "H-c") 'calendar)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
(global-set-key (kbd "TAB") 'insert-tab-char)		; same as Ctrl+i
(global-set-key (kbd "M-p") 'lpr-buffer)
(global-set-key (kbd "s-p") 'ps-print-buffer)

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

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
(defalias 'om 'org-mode)
(defalias 'obm 'org-bullets-mode)
(defalias 'ssm 'shell-script-mode)
