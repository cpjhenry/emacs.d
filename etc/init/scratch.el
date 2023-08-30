(use-package persistent-scratch
	:config	(persistent-scratch-setup-default))
(use-package unkillable-scratch :ensure t
	:init	(setq unkillable-scratch-do-not-reset-scratch-buffer t))
	:config	(unkillable-scratch t)
