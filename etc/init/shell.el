;; shell
(setenv "BASH_ENV" (expand-file-name "~/.bashrc"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
