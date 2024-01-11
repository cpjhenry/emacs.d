;; daily info
(defun daily-info () (interactive)
	(setq xbuff (generate-new-buffer "*daily-info*"))
	(shell-command "bash -ic di-mode" xbuff)
	(switch-to-buffer xbuff)
	(kill-line 2))
