;; See https://github.com/slotThe/vc-use-package
;; TODO: remove when vc-use-package is merged.
(unless *w32* (unless (>= emacs-major-version 30)
	(when (and (not (package-installed-p 'vc-use-package)) (fboundp 'package-vc-install))
	(package-vc-install "https://github.com/slotThe/vc-use-package"))
	(message "vc-use-package: disable once running Emacs 30+.")))
