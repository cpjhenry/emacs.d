;;; misc.el --- Miscellaneous functions
;;; commentary:
;; My miscellaneous routines moved to: cpjhelp.el.

;;; code:
;; https://www.emacswiki.org/emacs/DisabledCommands

(defun enable-all-commands ()
"Enable all commands, reporting on which were disabled."
	(interactive)
	(with-output-to-temp-buffer "*Commands that were disabled*"
	(mapatoms (function (lambda (symbol)
		(when (get symbol 'disabled)
			(put symbol 'disabled nil)
			(prin1 symbol)
			(princ "\n")))))))

(defun enable-me (&rest args)
"Called when a disabled command is executed. Enable it and re-execute it."
	(put this-command 'disabled nil)
	(message "You typed %s.  %s was disabled.  It ain't no more."
	(key-description (this-command-keys)) this-command)
	(sit-for 0)
	(call-interactively this-command))

;; https://olddeuteronomy.github.io/post/emacs-startup-screen/
(defun tec/welcome-page ()
    (interactive)
    (let* ((buffer-today (get-buffer-create "*today*"))
           (buffer-calendar "*Calendar*")
           (buffer-agenda "*Org Agenda*")
           (buffer-diary "*Fancy Diary Entries*"))
      ;; Call calendar first to obtain the current date
      ;; required to display the diary.
      (calendar)
      (diary)
      (org-agenda-list)
      ;; Fill and show the Today Events buffer.
      ;; NOTE: requires `fortune' and `calendar' command line utilities.
      (switch-to-buffer buffer-today)
      (call-process "fortune" nil buffer-today)
      (insert "\n")
      (call-process "calendar" nil buffer-today)
      (goto-char 0)
      (toggle-truncate-lines)
      ;; Maximize the Today Events window
      (delete-other-windows)
      ;; Show Agenda in the lower left quadrant.
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer buffer-agenda))
      (split-window-horizontally)
      ;; Try to show Diary in the lower right quadrant.
      (other-window 1)
      (if (get-buffer buffer-diary)
          ;; If Diary exists then show it ...
          (switch-to-buffer (get-buffer buffer-diary))
        ;; ... else show the scratch buffer.
        (let* ((buffer-scratch (switch-to-buffer (get-buffer "*scratch*"))))
          (goto-char (point-max))
          (insert (format-time-string "\n;; No diary entries for %A %d %b")))
        )
      ;; Go back to the Today Events buffer.
      (other-window -2)
      (split-window-horizontally)
      ;; Show Calendar in the upper left quadrant.
      (switch-to-buffer (get-buffer buffer-calendar))
      ))

;;; misc.el ends here
