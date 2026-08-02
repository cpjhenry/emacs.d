;; OAuth/GPG compatibility for org-gcal/oauth2-auto.
(setenv "GPG_AGENT_INFO" nil)
(setq epg-pinentry-mode 'loopback
      plstore-encrypt-to nil)
(defvar oauth2-auto-plstore)
(setq oauth2-auto-plstore (expand-file-name "oauth2-auto.plist"
			  (expand-file-name "var/" user-emacs-directory)))

(use-package org-gcal
  :after org
  :custom
  (org-gcal-fetch-file-alist `((,user-gmail . ,org-gcal-file)))
  (org-gcal-notify-p nil)
  (org-gcal-recurring-events-mode 'top-level)
  (plstore-cache-passphrase-for-symmetric-encryption t)
  (org-gcal-remove-api-cancelled-events nil)
  (org-gcal-update-cancelled-events-with-todo nil)
  :bind (("C-c C" . cpj/calfw-gcal))
  :config
  (add-to-list 'org-agenda-files org-gcal-file t)

  (defun cpj/org-gcal-sync-buffer-around (oldfun &rest args)
    "Run `org-gcal-sync-buffer' from `org-gcal-file' to shush warnings."
    (let ((buf (find-file-noselect org-gcal-file)))
      (with-current-buffer buf
	(unless (derived-mode-p 'org-mode)
          (org-mode))
	(apply oldfun args))))

  (advice-add 'org-gcal-sync-buffer
              :around #'cpj/org-gcal-sync-buffer-around)

  (defun cpj/calfw-gcal ()
    "Fetch Google Calendar events, then display calfw."
    (interactive)
    (with-current-buffer (find-file-noselect org-gcal-file)
      (ignore (org-gcal-fetch)))
    (cpj/calfw)))
