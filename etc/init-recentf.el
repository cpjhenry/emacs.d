;; recentf
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 50)

(recentf-mode t)
(setq recentf-arrange-by-dir t)
(setq recentf-save-file	(concat user-emacs-directory "var/recentf"))
;(run-at-time nil (* 5 60) 'recentf-save-list)
(add-to-list 'recentf-exclude ".emacs.d/elpa/")
(add-to-list 'recentf-exclude ".emacs.d/var/")
(add-to-list 'recentf-exclude "Applications/")
(add-to-list 'recentf-exclude "Library/")

;(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;(global-set-key (kbd "C-x C-g") 'recentf-open-files-compl)

(defun recentf-open-files-compl ()
	(interactive)
	(let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
			recentf-list))
			(fname (completing-read "File name: " tocpl nil nil)))
		(when fname
		(find-file (cdr (assoc-string fname tocpl))))))
