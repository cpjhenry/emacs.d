;; execute bash scripts

(defun di () "Daily information."
	(interactive)
	(shell-command "di-mode&" "*daily-info*")
	(diary-list-entries (calendar-current-date) diary-number-of-entries))

(defun gc () "Daily tasks"
	(interactive)
	(shell-command "gcal-mode" "*gcal*"))

(defun cm () "Print version of monthly calendar"
	(interactive)
	(shell-command "calm-mode" "*calm(p)*"))

(defun wx () "Weather"
	(interactive)
	(shell-command "wx-mode" "*wx*"))

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
	(let ((buf (generate-new-buffer "*az*")))
	(shell-command "az -u" buf)
	(switch-to-buffer buf)
	(markdown-preview)
	(kill-buffer buf)
	(kill-buffer "*markdown-output*"))

	;; (with-output-to-temp-buffer "*az*" ; HACK
	;; 	(shell-command "az -u"))
	;; 	(markdown-preview)
	)
