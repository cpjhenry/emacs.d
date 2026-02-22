;; smart-mode-line
(use-package smart-mode-line
:init	(setq sml/col-number-format "%2C")
:config (sml/setup)
		(add-to-list 'sml/replacer-regexp-list '("^:Doc:Notes/" ":Notes:") t)
		(add-to-list 'sml/replacer-regexp-list '("^:Doc:org/" ":org:") t)
		(add-to-list 'sml/replacer-regexp-list '("^:Doc:Projects/" ":Proj:") t)
		(add-to-list 'sml/replacer-regexp-list '("^:Doc:Reference/" ":Ref:") t)
		(add-to-list 'sml/replacer-regexp-list '("^.*/gemini/" ":gem:") t))

;; work-specific
	(add-to-list 'sml/replacer-regexp-list '("^.*City of Ottawa/" ":Work:") t)
	(add-to-list 'sml/replacer-regexp-list '("^.*/Work/" ":Work:") t)
	(add-to-list 'sml/replacer-regexp-list '("^:Work:Operations/" ":Ops:") t)
	(add-to-list 'sml/replacer-regexp-list '("^:Work:PDG/" ":PDG:") t)
	(add-to-list 'sml/replacer-regexp-list '("^:PDG:1-.*/" ":PDG-1:") t)
	(add-to-list 'sml/replacer-regexp-list '("^:PDG:2-.*/" ":PDG-2:") t)
	(add-to-list 'sml/replacer-regexp-list '("^:PDG:3-.*/" ":PDG-3:") t)
