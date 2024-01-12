;; daily info
(defun daily-info () (interactive)
	(setq xbuff (generate-new-buffer "*daily-info*"))
	;(setq shell-command-default-error-buffer "*Messages*")
	(shell-command "di-mode" xbuff)
	(switch-to-buffer xbuff)
	;(kill-line 2)
)
