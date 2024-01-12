;; daily info
(defun daily-info () "Daily information."
	(interactive)
	(setq xbuff (generate-new-buffer "*daily-info*"))
	(shell-command "di-mode" xbuff))

; LocalWords:  di
