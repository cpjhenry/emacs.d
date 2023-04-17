(use-package nswbuff ; buffer switching
	:init	(setq nswbuff-clear-delay 1.5)
			(setq nswbuff-display-intermediate-buffers t)
			(setq nswbuff-exclude-buffer-regexps '(
				"^ .*"
				"^\\*Help\\*"
				"^\\*Messages\\*"
				"^\\*Shell Command Output\\*"
				"from-mobile.org"
				"^\\*tramp/.*"))
	:config	(global-set-key (kbd "C-<tab>")   'nswbuff-switch-to-next-buffer)
			(global-set-key (kbd "C-S-<tab>") 'nswbuff-switch-to-previous-buffer))
