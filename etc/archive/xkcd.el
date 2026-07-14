(use-package xkcd
  :disabled
  :if *natasha*
  :defer t
  :hook	(xkcd-mode . turn-off-cursor)
  :init	(setq
	 xkcd-cache-dir    (concat user-emacs-directory "var/xkcd/")
	 xkcd-cache-latest (concat user-emacs-directory "var/xkcd/latest"))
  :config
  (defun xkcd-add-alt (&rest r)
    (interactive)
    (read-only-mode -1)
    (setq-local fill-column (window-width))
    (visual-line-mode 1)
    (insert "\n\n" xkcd-alt "\n")
    (read-only-mode t)
    (goto-char (point-min)))
  (advice-add 'xkcd-alt-text :override #'xkcd-add-alt)
  (advice-add 'xkcd-get :after #'xkcd-add-alt))
