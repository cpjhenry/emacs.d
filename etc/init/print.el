;; PRINT functions

(setq enscript "enscript -cqB")

;; https://stackoverflow.com/questions/15869131/emacs-shell-command-on-buffer (adapted)
(defun spool-to-enscript ()
	"Sends current buffer to 'enscript'."
	(interactive)
	(shell-command-on-region (point-min) (point-max) enscript))

;; https://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region (adapted)
(defun spool-to-enscript-region (&optional b e)
	"Sends current region to 'enscript'."
	(interactive "r")
	(shell-command-on-region b e enscript))

(defun print-to-receipt-printer ()
	"Re-formats and sends text in current buffer to POS printer."
	(interactive)
	(copy-current-buffer-to-temp-buffer)
	(text-mode)
	(setq fill-column 32)
	(mark-whole-buffer)
	(fill-region (point-min) (point-max))
	(spool-to-enscript)
	(kill-current-buffer) )

(defun print-to-a5-printer ()
	"Re-formats and sends text in current buffer to a5 printer."
	(interactive)
	(copy-current-buffer-to-temp-buffer)
	(text-mode)
	(setq fill-column 50)
	(mark-whole-buffer)
	(fill-region (point-min) (point-max))
	(ps-print-buffer-with-faces)
	(kill-current-buffer) )

;; https://genomeek.wordpress.com/2013/03/08/emarch-2-create-a-pdf-with-highlighted-code-source/
(defun print-to-pdf ()
 (interactive)
 (ps-spool-buffer-with-faces)
 (switch-to-buffer "*PostScript*")
 (write-file "tmp.ps")
 (kill-buffer "tmp.ps")
 (setq cmd (concat "pstopdf tmp.ps -o " (buffer-name) ".pdf"))
 (shell-command cmd)
 (shell-command "rm tmp.ps")
 (message (concat "File printed in : "(buffer-name) ".pdf")) )
