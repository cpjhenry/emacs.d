(use-package flyspell
	:if (executable-find "aspell")
	:ensure nil
	:bind (	:map flyspell-mouse-map
		([down-mouse-3] . flyspell-correct-word)
		:map flyspell-mode-map
		("C-M-i" . nil)) ; use 'C-.' instead
	:config	(setq
		flyspell-doublon-as-error-flag nil
		flyspell-issue-welcome-flag nil
		flyspell-issue-message-flag nil
		flyspell-use-meta-tab nil
		ispell-dictionary "canadian"
		ispell-extra-args '("--sug-mode=ultra")
		ispell-list-command "--list"	; correct command
		ispell-program-name "aspell"	; spell checker
		ispell-silently-savep t)	; save personal list automatically

	;; turn-on flyspell-mode for these modes
	(dolist (hook '(text-mode-hook markdown-mode-hook))
		(add-hook hook (lambda() (flyspell-mode 1))))
		;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)

	;; turn-off flyspell-mode for these modes
	(dolist (hook '(change-log-mode-hook emacs-news-mode-hook log-edit-mode-hook))
		(add-hook hook (lambda() (flyspell-mode -1))))

	(use-package flyspell-lazy ; reduces lag on Darwin systems
		:after flyspell
		:config (setq
			flyspell-lazy-idle-seconds 1
			flyspell-lazy-window-idle-seconds 3)
			(flyspell-lazy-mode 1))

	(use-package flyspell-correct
		:after flyspell
		:bind (	:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))))
