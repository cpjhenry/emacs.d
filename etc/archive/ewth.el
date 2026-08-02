(use-package ewth
  ;; https://github.com/chubin/wttr.in for deets
  ;; https://wttr.in/:help for options
  :disabled
  :if *natasha*
  :ensure nil
  :defer 2
  :config
  (setq ewth-url "http://wttr.in/Ottawa?format=2&d&T")
  (ewth-mode))
