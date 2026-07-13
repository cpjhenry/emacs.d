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
