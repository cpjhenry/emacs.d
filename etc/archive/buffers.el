;;; buffers
(message "→ Configuring buffers.")
(use-package s)
(use-package dash) ; for `-find', `-compose' and `-partial'
(load "filesandbuffers" nil 'nomessage)
(load "render-buffers" nil 'nomessage)
(load "skeletons" nil 'nomessage)

(use-package lean-emacs
  :ensure nil
  :demand t
  :bind ( ("M-j"	. join-line)			 ; default-indent-new-line (see 'C-M-j')
	  ("C-w"	. kill-region-or-backward-word)	 ; kill-region
	  ("M-w"	. kill-region-or-thing-at-point) ; kill-ring-save
	  ("C-M-]"	. match-paren)
	  ("C-x x s"	. save-all-unsaved))
  :hook   (find-file . large-find-file-hook)		 ; Emacs suffers when you open large files
  :config
  (dolist (key '("<home>" "s-<left>" "C-a"))
    (when (key-binding (kbd key))
      (global-set-key (kbd key) #'back-to-indentation-or-beginning-of-line))))

(require 'abbrev)
(setopt	abbrev-file-name (concat user-emacs-directory "etc/abbrev_defs")
	abbrev-suggest t
	save-abbrevs 'silently)

(require 'bookmark)
(setopt	bookmark-save-flag 1
	bookmark-set-fringe-mark nil
	bookmark-sort-flag nil
	bookmark-default-file (concat user-emacs-directory "etc/bookmarks"))

(require 'em-alias)
(require 'esh-mode)
(setopt	eshell-aliases-file (concat user-emacs-directory "etc/eshell/aliases")
	eshell-directory-name (concat user-emacs-directory "var/eshell/"))

;;; FIXME - collides with `org-ellipsis'
;; (require 'formfeed-hline)
;; (if (featurep 'formfeed-hline) (formfeed-hline-mode))

(require 'grep)
(setopt grep-use-headings t)

(require 'man)
(setopt	Man-notify-method 'pushy)

(require 'prog-mode)
(global-prettify-symbols-mode)
(setopt	prettify-symbols-unprettify-at-point 'right-edge)

(require 'net-utils)
(setopt whois-server-name "whois.ca.fury.ca")

(add-hook 'before-save-hook 'time-stamp)

;; files are given +x permissions when they're saved, if they contain a valid shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; remove trailing whitespace on-save
;(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Mode hooks
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'help-mode-hook
	  (lambda () (setq-local font-lock-keywords-only t)
	    (goto-address-mode)))
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(remove-hook 'file-name-at-point-functions 'ffap-guess-file-name-at-point)

;; Modes derived from special-mode will pick-up this directive
;; HACK keymap-set
(define-key special-mode-map (kbd "q") 'kill-current-buffer)
(define-key messages-buffer-mode-map (kbd "q")	'bury-buffer) ; 'messages-buffer-mode

;; eval-after-loads are run once, before mode hooks
;; mode-hooks execute once for every buffer in which the mode is enabled

(with-eval-after-load 'calc
  (define-key calc-mode-map (kbd "q") 'kill-current-buffer))

(with-eval-after-load 'emacs-news-mode
  (define-key emacs-news-view-mode-map (kbd "[") 'my/outline-previous-heading)
  (define-key emacs-news-view-mode-map (kbd "]") 'my/outline-next-heading)
  (define-key emacs-news-view-mode-map (kbd "{") 'outline-backward-same-level)
  (define-key emacs-news-view-mode-map (kbd "}") 'outline-forward-same-level))

(with-eval-after-load 'help-mode
  (define-key help-mode-map (kbd "[")	'help-go-back)
  (define-key help-mode-map (kbd "]")	'help-go-forward)
  (define-key help-mode-map (kbd "M-RET") 'goto-address-at-point))

(require 'info)
(define-key Info-mode-map (kbd "q")	'kill-current-buffer)
(define-key Info-mode-map (kbd "[" )	'Info-history-back)
(define-key Info-mode-map (kbd "]")	'Info-history-forward)
(define-key Info-mode-map (kbd "{")	'Info-backward-node)
(define-key Info-mode-map (kbd "}")	'Info-forward-node)

(with-eval-after-load 'view
  (define-key view-mode-map (kbd "j")	'View-scroll-line-forward)
  (define-key view-mode-map (kbd "k")	'my/View-scroll-line-backward)
  (define-key view-mode-map (kbd "q")	'View-kill-and-leave))

;; removes *Completions* buffer when done
(add-hook 'minibuffer-exit-hook
	  (lambda ()
	    (let ((buffer "*Completions*"))
	      (and (get-buffer buffer) (kill-buffer buffer)))))

;; opening multiple files
(add-hook 'window-setup-hook 'delete-other-windows) ; Show only one active window

;; *scratch*
(use-package autoscratch
  :custom (initial-major-mode 'autoscratch-mode))

;; The form-feed ASCII character (0x0C or 12) was used to signal the
;; end of the page. It's still used (albeit not that frequently) in
;; code to divide a file into logical "pages".
(use-package form-feed-st
  :config (global-form-feed-st-mode)
  	  (add-to-list 'form-feed-st-include-modes 'help-mode t))

;; backups / auto-save
(setopt	auto-save-default nil
	auto-save-list-file-prefix (concat user-emacs-directory "var/auto-save/")
	auto-save-no-message nil
	auto-save-visited-interval 60
	create-lockfiles nil

	backup-by-copying t
	delete-old-versions t
	backup-directory-alist '(("." . ".~"))

	make-backup-files t
	vc-make-backup-files nil ; don't make back-ups in git-controlled dirs
	version-control nil)

;; copies every file you save in Emacs to a backup directory tree
;; (require 'backup-each-save)
;; (add-hook 'after-save-hook 'backup-each-save)

;; http://xahlee.info/emacs/emacs/emacs_auto_save.html
;; (when (>= emacs-major-version 26)
;;	;; real auto save
;;	(auto-save-visited-mode t))

(if (version< emacs-version "27.1")
    (add-hook 'focus-out-hook 'save-all-unsaved)
  (setq after-focus-change-function 'save-all-unsaved))
;; to undo this, run: (setq after-focus-change-function 'ignore)

;; https://protesilaos.com/codelog/2024-12-11-emacs-diff-save-some-buffers/
(add-to-list 'save-some-buffers-action-alist
	     (list "d"
		   (lambda (buffer)
		     (diff-buffer-with-file
		      (buffer-file-name buffer)))
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
(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
	       (display-buffer-no-window)
	       (allow-no-window . t)))

;; comment continuation
(keymap-set emacs-lisp-mode-map "S-<return>" 'default-indent-new-line)

;; comment out malfunctioning code, or well, comment
(defmacro comment (&rest _body)
  "Ignore BODY, just like `ignore', but this is a macro."
  '())

;; display available keybindings in popup
(use-package which-key
  :custom (which-key-idle-delay 0.5)
  :bind (("C-h C-h" . nil))
  :config (which-key-mode)
  (defalias 'which-key-alias 'which-key-add-key-based-replacements)

  (defun cpj/which-key-abort-quietly (&optional _)
    "Abort which-key without signalling `keyboard-quit'."
    (interactive)
    (let ((which-key-inhibit t))
      (when (fboundp 'which-key--hide-popup-ignore-command)
	(which-key--hide-popup-ignore-command))
      (message nil)))

  (which-key-define-key-recursively global-map
   (kbd "C-g") #'cpj/which-key-abort-quietly)

  (push '((nil . "\\`cpj/which-key-abort-quietly\\'") . t)
   which-key-replacement-alist))

;; search TERM in a web browser
(keymap-set search-map "b" #'browser-search)

;; intelligent narrowing
(require 'narrow-dwim)
(keymap-set global-map "C-c n" #'narrow-dwim)

;; whois
;; HACK · when executing command, resultant buffer needs local-key set.
;(advice-add 'whois :after (keymap-local-set "q" 'kill-current-buffer))
