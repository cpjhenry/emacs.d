;; execute bash scripts

(defun daily-info () "Daily information."
	(interactive)
	(let ((buf (generate-new-buffer "*daily-info*")))
	(shell-command "di-mode" buf)
	(switch-to-buffer buf)
	(form-feed-mode)))

(defun az () "Monthly Forecast"
	(interactive)
	(let ((buf (generate-new-buffer "*az*")))
	(shell-command "az -u" buf)
	(switch-to-buffer buf)
	(markdown-preview)
	(and (get-buffer buf)
		(kill-buffer buf)
		(kill-buffer "*markdown-output*"))))

(defun fw () "Weekly Forecast"
	(interactive)
	(setq-local buf (generate-new-buffer "*fw*"))
	(shell-command "fw" buf))

(defun wx () "Weather"
	(interactive)
	(setq-local buf (generate-new-buffer "*wx*"))
	(shell-command "wx-mode" buf))

; LocalWords:  di buf az
