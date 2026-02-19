(use-package centaur-tabs
;	:config
;	(centaur-tabs-mode t)
	)
(centaur-tabs-headline-match)
(centaur-tabs-change-fonts "Inconsolata" 160)
(setq centaur-tabs--buffer-show-groups t)
(global-set-key (kbd "C-<home>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<end>") 'centaur-tabs-forward)

