;; execute bash scripts

(defun di () "Daily information."
	(interactive)

	;(cbc)
	(switch-to-buffer "*daily-info*")
	(shell-command "di-mode&" (current-buffer))
	(view-mode)

	(diary-list-entries (calendar-current-date) diary-number-of-entries)
	(kill-buffer "diary"))

(defun cbc () "Today's headlines from CBC Ottawa"
	(interactive)
	(switch-to-buffer "*CBC*")
	(shell-command "cbc-mode" (current-buffer))
	(org-mode)
	(view-mode)
	(goto-char (point-min)))

(defun cm () "Print version of monthly calendar."
	(interactive)
	(switch-to-buffer "*calm(p)*")
	(shell-command "cm-mode" (current-buffer))
	(view-mode))

(defun wx () "Local weather."
	(interactive)
	(shell-command "wx-mode"))

(defun fw () "Weekly Forecast"
	(interactive)
	(switch-to-buffer "*Virgo*")
	(shell-command "fw -u" (current-buffer))
	(text-mode)
	(view-mode)
	(end-of-buffer)

	(switch-to-buffer "*Aries*")
	(shell-command "fw -uf aries |perl -p -e 'chomp if eof'" (current-buffer))
	(text-mode)
	(ispell-buffer)
	(kill-ring-save (point-min) (point-max))
	(kill-buffer (current-buffer))
	(message "Forecast saved to clipboard."))

(defun az () "Monthly Forecast"
	(interactive)
	(let ((buf (make-temp-name ""))
	      (output "Monthly Forecast"))
	(switch-to-buffer buf)
	(shell-command "az -u" buf)
	(require 'markdown-mode)
	(markdown-preview output)
	(kill-buffer buf)
	(kill-buffer output)))
