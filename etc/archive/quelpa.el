(use-package quelpa
  :custom (quelpa-packages-dir (expand-file-name "var/quelpa/" user-emacs-directory))
	  (quelpa-melpa-dir (expand-file-name "var/quelpa/melpa/" user-emacs-directory)))
(use-package quelpa-use-package
  :after quelpa)

;; https://github.com/alphapapa/unpackaged.el
(use-package unpackaged
  :disabled
  :quelpa (unpackaged :fetcher github :repo "alphapapa/unpackaged.el"))
