;; Install straight.el (2024-02-04)
;; Installed to enable maccalfw configuration.
;; see https://iosexample.com/maccalfw-el-calendar-view-for-mac-calendars/

(defvar bootstrap-version)
(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
			(url-retrieve-synchronously
			"https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
			'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))
(if (>= emacs-major-version 29) (setq straight-repository-branch "develop"))

; LocalWords:  maccalfw el
