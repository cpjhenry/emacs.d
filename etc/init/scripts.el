;; execute bash scripts

(defun di () "Daily information."
	(interactive)
	(let ((buf (generate-new-buffer "*daily-info*")))
	(shell-command "di-mode" buf)
	(switch-to-buffer buf)
	(form-feed-mode)))

(defun gc () "Daily tasks"
	(interactive)
	(setq-local buf (generate-new-buffer "*gcal*"))
	(shell-command "gcal-mode" buf))

(defun calm () "Print version of monthly calendar"
	(interactive)
	(setq-local buf (generate-new-buffer "*calm(p)*"))
	(shell-command "calm-mode" buf))

(defun wx () "Weather"
	(interactive)
	(setq-local buf (generate-new-buffer "*wx*"))
	(shell-command "wx-mode" buf))

(defun fw () "Weekly Forecast"
	(interactive)
	(let ((buf (generate-new-buffer "*Virgo*")))
	(shell-command "fw -u" buf)
	(switch-to-buffer buf)
	(text-mode)
	(end-of-buffer))

	(let ((buf (generate-new-buffer "*Aries*")))
	(shell-command "fw -uf aries" buf)
	(switch-to-buffer buf)
	(text-mode)
	(ispell-buffer)
	(kill-ring-save (point-min) (point-max))
	(kill-buffer buf)
	(message "Forecast saved to clipboard.")))

(defun az () "Monthly Forecast"
	(interactive)
	(let ((buf (generate-new-buffer "*az*")))
	(shell-command "az -u" buf)
	(switch-to-buffer buf)
	(markdown-preview)
	(kill-buffer buf)
	(kill-buffer "*markdown-output*")))

; LocalWords:  buf di az fw wx gcal uf aries
