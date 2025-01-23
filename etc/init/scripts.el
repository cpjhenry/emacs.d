;; execute bash scripts

(defun di () "Daily information."
	(interactive)
	(switch-to-buffer "*daily-info*")
	(shell-command "di-mode&" (current-buffer))
	(view-mode)

	(switch-to-buffer "*alert*")
	(shell-command "alert -f" (current-buffer))
	(view-mode)

	(diary-list-entries (calendar-current-date) diary-number-of-entries)
	(kill-buffer "diary")

	(message "'cbc' / 'xkcd' / 'elfeed'"))

(defun cbc () "Today's headlines from CBC Ottawa"
	(interactive)
	(switch-to-buffer "*CBC*")
	(shell-command "cbc-mode" (current-buffer))
	(org-mode)
	(view-mode)
	(goto-char (point-min)))

(defun /. () "/."
	(interactive)
	(switch-to-buffer "*/.*")
	(shell-command "slashdot" (current-buffer))
	(org-mode)
	(view-mode)
	(jinx-mode -1)
	(goto-char (point-min)))

(defun cm () "Print version of monthly calendar."
	(interactive)
	(switch-to-buffer "*calm(p)*")
	(shell-command "cm-mode" (current-buffer))
	(view-mode))

(defun wx () "Local weather."
	(interactive)
	(shell-command "wx-mode"))

(defun fw () "Weekly Forecast."
	(interactive)
	(switch-to-buffer "*Virgo*")
	(shell-command "fw -u" (current-buffer))
	(text-mode)
	(turn-off-cursor)
	(view-mode)

	(switch-to-buffer "*Aries*")
	(shell-command "fw -uf aries |perl -p -e 'chomp if eof'" (current-buffer))
	(text-mode)
	(ispell-buffer)
	(kill-ring-save (point-min) (point-max))
	(kill-buffer (current-buffer))
	(message "Forecast saved to clipboard."))

(defun az () "Monthly Forecast."
	(interactive)
	(let ((output "Monthly Forecast"))
	(switch-to-buffer (make-temp-name ""))
	(shell-command "az -u" (current-buffer))
	(require 'markdown-mode)
	(markdown-preview output)
	(kill-buffer (current-buffer))
	(kill-buffer output)))
