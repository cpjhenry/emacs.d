;;;; Deferred / disabled

;; Copies every file you save in Emacs to a backup directory tree.
;; (use-package backup-each-save
;;   :ensure nil
;;   :hook (after-save . backup-each-save))

;; Real auto-save.
;; (when (>= emacs-major-version 26)
;;   (auto-save-visited-mode 1))
