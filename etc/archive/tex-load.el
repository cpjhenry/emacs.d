;; TeX
(use-package tex
	:unless *w32*
	:ensure auctex
	:custom (font-latex-fontify-sectioning 'color)
		(LaTeX-babel-hyphen-after-hyphen nil)
		(latex-run-command "xelatex")
		(TeX-auto-save t)
		(TeX-master nil)	; FIXME EMACS30 fail
		(TeX-parse-self nil)	; FIXME EMACS30 fail
	:hook	(LaTeX-mode . (lambda () (push '("\\&" . ?＆) prettify-symbols-alist)))
	:config (setq
		ispell-parser 'tex
		preview-leave-open-previews-visible t
		preview-locating-previews-message nil
		preview-protect-point t)

	(use-package latex-extra
		:hook (LaTeX-mode . latex-extra-mode))

	(use-package latex-pretty-symbols)

	(use-package latex-preview-pane
		:bind (	:map latex-preview-pane-mode-map ("M-p" . nil) ("M-P" . nil))
		:config	(setq message-latex-preview-pane-welcome ""))

	(use-package cdlatex)
	(use-package reftex
		:ensure nil
		:hook (	(LaTeX-mode . turn-on-reftex)
			(LaTeX-mode . turn-on-cdlatex))))
