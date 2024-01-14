;; execute bash scripts

(defun daily-info () "Daily information."
	(interactive)
	(let ((buf (generate-new-buffer "*daily-info*")))
	(shell-command "di-mode" buf)
	(switch-to-buffer buf)
	(help-mode)))
	
(defun wx ()
	(interactive)
	(setq xbuff (generate-new-buffer "*wx*"))
	(shell-command "wx-mode" xbuff))

; LocalWords:  di buf
