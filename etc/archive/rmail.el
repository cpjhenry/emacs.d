(use-package rmail
  :if *natasha*
  :ensure nil
  :defer t
  :custom
  (rmail-secondary-file-directory "~/Mail/")
  (rmail-default-file "~/Mail/XMAIL")
  (rmail-file-name "~/Mail/RMAIL")

  (rmail-primary-inbox-list '("imaps://cn914@mail.ncf.ca"))
  (rmail-remote-password-required t)
  :hook
  (rmail-show-message . goto-address-mode)
  (rmail-quit . kill-current-buffer)
  :config
  (setq
   smtpmail-smtp-server "mail.ncf.ca"
   send-mail-function   'smtpmail-send-it
   smtpmail-smtp-service 587

   rmail-delete-after-output t
   rmail-mail-new-frame t
   rmail-mime-prefer-html nil
   rmail-movemail-variant-in-use 'mailutils
   rmail-preserve-inbox nil

   rmail-highlighted-headers "^Subject:"
   rmail-ignored-headers (concat rmail-ignored-headers
				 "\\|^In-Reply-To:\\|^Content-Type:\\|^DKIM-Filter:")
   rmail-nonignored-headers nil))
