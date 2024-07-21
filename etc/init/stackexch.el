;;; Stack Exchange
(use-package sx
	:config
		(bind-keys
		:prefix "C-c S"
		:prefix-map my-sx-map
		:prefix-docstring "Global keymap for SX."
			("q" . sx-tab-all-questions)
			("i" . sx-inbox)
			("o" . sx-open-link)
			("u" . sx-tab-unanswered-my-tags)
			("a" . sx-ask)
			("s" . sx-search))
		(setq sx-cache-directory (concat user-emacs-directory "var/sx")))
