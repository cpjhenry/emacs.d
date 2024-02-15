(use-package exec-path-from-shell)
;; A known problem with GUI Emacs on MacOS: it runs in an isolated
;; environment, so envvars will be wrong. That includes the PATH
;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
;; and benefits greatly from compilation.
(setq exec-path
	(or (eval-when-compile
   	(when (require 'exec-path-from-shell nil t)
		(setq exec-path-from-shell-check-startup-files nil)
		(nconc exec-path-from-shell-variables '("PATH" "MANPATH" "LC_TYPE" "LC_ALL" "LANG" ))
		(exec-path-from-shell-initialize) exec-path))
		exec-path))

;; -or-
;(setq exec-path '("/Users/cpjh/bin/" "/Library/TeX/texbin/" "/usr/local/opt/qt@5/bin/"
;				"/usr/local/opt/python@3/libexec/bin/" "/usr/libexec/"
;				"/usr/local/opt/gnu-sed/libexec/gnubin/" "/usr/local/opt/coreutils/libexec/gnubin/"
;				"/usr/local/bin/" "/usr/local/sbin/" "/usr/bin/" "/usr/sbin/" "/bin/" "/sbin/"
;				"/Applications/Emacs.app/Contents/MacOS/libexec/" ))
