(use-package elfeed
	:init	(setq elfeed-db-directory (concat user-emacs-directory "var/elfeed/db/"))
			(setq elfeed-enclosure-default-dir (concat user-emacs-directory "var/elfeed/enclosures/"))
			(setq elfeed-score-score-file (concat user-emacs-directory "etc/elfeed/score/score.el"))
			(setq elfeed-sort-order 'ascending)
			(setq elfeed-use-curl t)
	:config	(load "rc/elfeed" 'noerror 'nomessage)
			(eval-after-load 'elfeed `(make-directory ,(concat user-emacs-directory "var/elfeed/") t))
			(easy-menu-add-item  nil '("tools") ["Read web feeds" elfeed t])

			(bind-key "C-c f" 'elfeed)
			(define-key elfeed-search-mode-map (kbd "q") (lambda()(interactive) (kill-current-buffer)
			(let ((buffer "*elfeed-log*")) (and (get-buffer buffer) (kill-buffer buffer))) )) )

(defun elfeed-mark-all-as-read () (interactive)
	(mark-whole-buffer)
	(elfeed-search-untag-all-unread))
	(define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

(defun elfeed-mail-todo (&optional use-generic-p)
	"Mail this to myself for later reading"
	(interactive "P")
	(let ((entries (elfeed-search-selected)))
		(cl-loop for entry in entries
			do (elfeed-untag entry 'unread)
			when (elfeed-entry-title entry)
			do (todo it (elfeed-entry-link entry)))
		(mapc #'elfeed-search-update-entry entries)
		(unless (use-region-p) (forward-line))))
	;(define-key elfeed-search-mode-map (kbd "m") 'elfeed-mail-todo)
