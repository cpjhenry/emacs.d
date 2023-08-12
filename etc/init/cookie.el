;; Today's cookie
(when *mac*
	(setq cookie-file "/usr/local/share/games/fortunes/fortunes")
	(setq fortune-dir "/usr/local/share/games/fortunes/")
	(defun todayscookie () (message (cookie cookie-file)))
	(add-hook 'window-setup-hook 'todayscookie) )
