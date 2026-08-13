;;; scripts.el --- execute bash scripts -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'diary-lib)
(declare-function turn-off-cursor "filesandbuffers")
(declare-function markdown-preview "markdown-mode")
(declare-function toggle-fill-column-center "filesandbuffers")

(require 'seq)
(require 'subr-x)

(defun wx-alert (&rest _ignore)
  "Weather forecast from Environment Canada."
  (interactive)
  (switch-to-buffer "*WX*")
  (shell-command "alert -sfml" (current-buffer))
  (text-scale-increase 1)
  (form-feed-st-mode)
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

(defun ccalt ()
  "Display today's Chinese calendar."
  (interactive)
  (let ((missing
         (seq-remove #'executable-find '("calendar" "ccal"))))
    (when missing
      (user-error
       "Required executable%s not found: %s"
       (if (cdr missing) "s" "")
       (string-join missing ", "))))
  (let ((buffer (get-buffer-create "*Chinese Calendar (today)*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (shell-command
         "calendar -s chinese | head -n3; echo; ccal"
         buffer)
        (goto-char (point-min))
        (while (re-search-forward
                "\e\\[7m\\(.*?\\)\e\\[0m"
                nil t)
          (replace-match
           (propertize (match-string 1) 'face 'bold)
           t t))
        (goto-char (point-min))
        (view-mode 1)
        (turn-off-cursor)))
    (switch-to-buffer buffer)))

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

(defun az ()
  "Monthly Forecast from Astrology Zone."
  (interactive)
  (let ((output "*Monthly-Forecast*"))
    (switch-to-buffer output)
    (shell-command "az -u" output)

    (markdown-preview)
    (kill-buffer output)
    (kill-buffer "*markdown-output*")

    (switch-to-buffer-matching output)
    (text-scale-increase 1)
    (setq-local inhibit-read-only t)))

;;; scripts.el ends here

; LocalWords:  cbc sfm slashdot fw uf wttr aries perl eof az ccal
; LocalWords:  filesandbuffers sfml chinese
