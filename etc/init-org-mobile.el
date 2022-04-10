;; org-mobile

(setq org-mobile-directory "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents")
(setq org-mobile-inbox-for-pull (concat org-directory "from-mobile.org"))
(use-package org-mobile-sync :config (org-mobile-sync-mode 1))
