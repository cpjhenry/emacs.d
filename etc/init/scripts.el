;;; scripts.el --- execute bash scripts -*- lexical-binding: t -*-

;;; commentary:

;;; code:

(defun di () "Daily information."
	(interactive)
	(switch-to-buffer "*daily-info*")
	(shell-command "di-mode&" (current-buffer))
	(view-mode)

	(alert)

	(diary-list-entries (calendar-current-date) diary-number-of-entries)
	(kill-buffer "diary")

	(message "'cbc' / 'xkcd' / 'elfeed'"))

(defun alert () "Weather alerts from Environment Canada."
	(interactive)
	(switch-to-buffer "*alert*")
	(shell-command "alert -f" (current-buffer))
	(view-mode))

(defun maritime () "Maritime alerts from Environment Canada."
	(interactive)
	(switch-to-buffer "*maritime alerts*")
	(shell-command "alert -m |sed '1s/$/\\n/'" (current-buffer))
	(view-mode))

(defun cbc () "Today's headlines from CBC Ottawa."
	(interactive)
	(switch-to-buffer "*CBC*")
	(shell-command "cbc-mode" (current-buffer))
	(org-mode)
	(view-mode)
	(goto-char (point-min)))

(defun /. () "/."
	(interactive)
	(switch-to-buffer "*/.*")
	(shell-command "slashdot-mode" (current-buffer))
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

(defun wttr () "Local weather / forecast"
  (interactive)
  (switch-to-buffer "*wttr*")
  (shell-command "curl -s https://wttr.in/Ottawa?1nqT |head -n -2" (current-buffer))
  (view-mode))

(defun fw () "Weekly Forecast."
  (interactive)
  (switch-to-buffer "*Virgo*")
  (shell-command "fw -u" (current-buffer))
  (text-mode)

  (turn-off-cursor)
  (text-scale-increase 1)
  (visual-fill-column-mode)
  (toggle-fill-column-center)
  (view-mode)

  ;; leaves view-mode 'on' (keys work), but otherwise modifiable by spell-checker
  (setq-local inhibit-read-only t)

  (switch-to-buffer "*Aries*")
  (shell-command "fw -uf aries |perl -p -e 'chomp if eof'" (current-buffer))
  (text-mode)

  ;; First approach, automate spell and copying.
  ;; Doesn't work, at least not with Jinx, which spawns its own process.

  ;(ispell-buffer)
  ;(kill-ring-save (point-min) (point-max))
  ;(kill-buffer (current-buffer))
  ;(message "Forecast saved to clipboard.")

  ;; Second approach, leaving spell-checking and copying to user.
  (view-mode)
  (setq-local inhibit-read-only t))

(defun az () "Monthly Forecast."
  (interactive)
  (let ((output "Monthly Forecast"))
    (switch-to-buffer (make-temp-name ""))
    (shell-command "az -u" (current-buffer))
    (markdown-preview output)
    (kill-buffer (current-buffer))
    (kill-buffer output)

    (switch-to-buffer-matching output)
    (visual-line-mode)
    (eww-unfill-paragraph)
    (visual-fill-column-mode)
    (toggle-fill-column-center)

    ;; leaves view-mode 'on' (keys work), but otherwise modifiable by spell-checker
    (setq-local inhibit-read-only t)))

(defun wwv () "Geophysical alerts and space weather."
       (interactive)
       (eww "https://services.swpc.noaa.gov/text/"))

;;; scripts.el ends here

; LocalWords:  cbc
