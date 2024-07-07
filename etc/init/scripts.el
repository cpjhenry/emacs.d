;; execute bash scripts

(defun di () "Daily information."
	(interactive)
	(let ((buf (generate-new-buffer "*daily-info*")))
	(shell-command "di-mode&" buf)
	(switch-to-buffer buf)
	(view-mode)
	(diary-list-entries (calendar-current-date) diary-number-of-entries)))

(defun cm () "Print version of monthly calendar."
	(interactive)
	(let ((buf (generate-new-buffer "*calm(p)*")))
	(shell-command "cm-mode" buf)
	(switch-to-buffer buf)
	(view-mode)))

(defun wx () "Local weather."
	(interactive)
	(let ((buf (generate-new-buffer "*wx*")))
	(shell-command "wx-mode" buf)
	(kill-buffer buf)))

(defun fw () "Weekly Forecast"
	(interactive)
	(let ((buf (generate-new-buffer "*Virgo*")))
	(shell-command "fw -u" buf)
	(switch-to-buffer buf)
	(text-mode)
	(end-of-buffer))

	(let ((buf (generate-new-buffer "*Aries*")))
	(shell-command "fw -uf aries |perl -p -e 'chomp if eof'" buf)
	(switch-to-buffer buf)
	(text-mode)
	(ispell-buffer)
	(kill-ring-save (point-min) (point-max))
	(kill-buffer buf)
	(message "Forecast saved to clipboard.")))

(defun az () "Monthly Forecast"
	(interactive)
	(let (
		(buf (generate-new-buffer (make-temp-name "")))
		(output "Monthly Forecast"))
	(shell-command "az -u" buf)
	(switch-to-buffer buf)
	(markdown-preview output)
	(kill-buffer buf)
	(kill-buffer output)
	))
