(use-package olivetti
	:mode ("\\.txt\\'" . olivetti-mode) )
(add-hook 'olivetti-mode-hook (lambda ()
	(setq-local olivetti-body-width 80)
	(flyspell-mode)
	(visual-line-mode)
	(wc-mode) ))

