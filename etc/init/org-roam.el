(use-package org-roam
	:ensure t
	:custom	(org-roam-db-location (concat user-emacs-directory "var/org-roam.db"))
		(org-roam-directory (file-truename (concat org-directory "/Roam/")))
	:bind (	("C-c r l" . org-roam-buffer-toggle)
		("C-c r f" . org-roam-node-find)
		("C-c r g" . org-roam-graph)
		("C-c r i" . org-roam-node-insert)
		("C-c r c" . org-roam-capture)
		("C-c r j" . org-roam-dailies-capture-today))
	:config	(org-roam-setup)
		(org-roam-db-autosync-mode)
		(which-key-alias "C-c r" "org-roam"))
