(use-package flycheck ; on-the-fly syntax checking
  :unless *w32*
  :custom (flycheck-keymap-prefix "!")
  :hook	  (emacs-lisp-mode . flycheck-mode)
  :init	  (require 'checkdoc)
	  (setq checkdoc-column-zero-backslash-before-paren nil
		checkdoc-force-docstrings-flag nil
		checkdoc--argument-missing-flag nil)
  :config (which-key-alias "C-x !" "flycheck")
	  (if (featurep 'ibuf-ext)
	    (add-to-list 'ibuffer-never-show-predicates "^\\*Flycheck error messages\\*"))
	  (if (featurep 'ido)
	    (add-to-list 'ido-ignore-buffers "*Flycheck error messages*")))

