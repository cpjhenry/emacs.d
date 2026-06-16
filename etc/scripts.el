;;; scripts.el --- execute bash scripts -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'diary-lib)
(declare-function turn-off-cursor "filesandbuffers")
(declare-function markdown-preview "markdown-mode")
(declare-function toggle-fill-column-center "filesandbuffers")

(defun wx-alert (&rest _ignore)
  "Weather forecast from Environment Canada."
  (interactive)
  (switch-to-buffer "*WX*")
  (shell-command "alert -sfm" (current-buffer))
  (text-scale-increase 1)
  (view-mode))

(defun wx () "Local weather."
  (interactive)
  (shell-command "alert -d"))

(defun wttr () "Local weather / forecast."
  (interactive)
  (switch-to-buffer "*wttr*")
  (shell-command "curl -s https://wttr.in/Ottawa?1nqT |head -n -2" (current-buffer))
  (view-mode)
  (turn-off-cursor))

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
  (if (featurep 'jinx) (jinx-mode -1))
  (goto-char (point-min)))

(defun fw () "Weekly Forecast."
  (interactive)
  (switch-to-buffer "*Virgo*")
  (shell-command "fw -u" (current-buffer))
  (text-mode)

  (turn-off-cursor)
  (text-scale-increase 1)
  (toggle-fill-column-center)
  (view-mode)

  ;; leaves view-mode 'on' (keys work), but otherwise modifiable by spell-checker
  (setq-local inhibit-read-only t)

  (switch-to-buffer "*Aries*")
  (shell-command "fw -uf aries |perl -p -e 'chomp if eof'" (current-buffer))
  (text-mode)

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
    (text-scale-increase 1)

    ;; leaves view-mode 'on' (keys work), but otherwise modifiable by spell-checker
    (setq-local inhibit-read-only t)))

;;; scripts.el ends here

; LocalWords:  cbc sfm slashdot fw uf wttr aries perl eof az
; LocalWords:  filesandbuffers
