(use-package latex-preview-pane
  :defer
  ;:hook (LaTeX-mode . latex-preview-pane-mode)
  :bind (:map latex-preview-pane-mode-map
	      ("M-p" . nil)
              ("M-P" . nil))
  :config
  (setq message-latex-preview-pane-welcome ""))

(use-package cdlatex
  :defer t
  :hook (LaTeX-mode . turn-on-cdlatex))

(use-package reftex
  :ensure nil
  :defer t
  :hook (LaTeX-mode . turn-on-reftex))
