;; Misc. printing commands

;; https://emacs.stackexchange.com/questions/42145/printing-exporting-to-pdf-from-text-mode
(require 'ps-print)
(when (executable-find "ps2pdf")
(defun pdf-print-buffer-with-faces (&optional filename)
"Print file in the current buffer as pdf, including font, color, and
underline information.  This command works only if you are using a window system,
so it has a way to determine color values.

C-u COMMAND prompts user where to save the Postscript file (which is then
converted to PDF at the same location."
	(interactive (list (if current-prefix-arg
		(ps-print-preprint 4)
		(concat (file-name-sans-extension (buffer-file-name)) ".ps"))))
	(ps-print-with-faces (point-min) (point-max) filename)
	(shell-command (concat "ps2pdf " filename))
	(delete-file filename)
	(message "Deleted %s" filename)
	(message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf"))))

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
	(message (concat "File printed in : "(buffer-name) ".pdf")))

;; https://www.emacswiki.org/emacs/PrintingBdfFonts
