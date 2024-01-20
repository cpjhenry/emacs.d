;; execute bash scripts

(defun daily-info () "Daily information."
	(interactive)
	(let ((buf (generate-new-buffer "*daily-info*")))
	(shell-command "di-mode" buf)))

(defun fw () "Forecast"
	(interactive)
	(setq-local buf (generate-new-buffer "*fw*"))
	(shell-command "fw" buf))

(defun wx () "Weather"
	(interactive)
	(setq-local buf (generate-new-buffer "*wx*"))
	(shell-command "wx-mode" buf))

; LocalWords:  di buf
