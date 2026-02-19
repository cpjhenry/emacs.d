(defun nuke-all-buffers ()
	"Kill all buffers, leaving *scratch* only."
	(interactive)
	(mapcar (lambda (x) (kill-buffer x))
		(buffer-list))
	(delete-other-windows))

(use-package persistent-scratch
	:config	(persistent-scratch-setup-default))

(use-package unkillable-scratch
	:ensure t
	:init	(setq unkillable-scratch-do-not-reset-scratch-buffer t)
	:config	(unkillable-scratch t))

(unless *w32* (setq initial-buffer-choice "~/"))
