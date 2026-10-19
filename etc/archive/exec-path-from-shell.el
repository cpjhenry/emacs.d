(use-package exec-path-from-shell
  :disabled
  :if *mac*
  :custom
  (shell-file-name (getenv "SHELL"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "PKG_CONFIG_PATH"))
  :init
  (unless (bound-and-true-p ns-emacs-plus-injected-path)
    (exec-path-from-shell-initialize)))
