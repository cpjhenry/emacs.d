(use-package chatgpt-shell
  :if	*natasha*
  :defer t
  :config (setq chatgpt-shell-root-path (concat user-emacs-directory "var/")))
