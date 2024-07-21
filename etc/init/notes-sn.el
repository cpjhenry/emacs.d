(use-package simplenote2
		:bind (("<f8>" . sn)) )
(load "rc/sn" 'noerror)
(simplenote2-setup)
(setq simplenote2-directory	(concat user-emacs-directory "var/simplenote2/"))
(setq simplenote2-markdown-notes-mode 'markdown-mode)
(add-hook 'simplenote2-create-note-hook (lambda () (simplenote2-set-markdown)))

(defalias 'sn 'simplenote2-browse)
(defalias 'snsm 'simplenote2-set-markdown)
