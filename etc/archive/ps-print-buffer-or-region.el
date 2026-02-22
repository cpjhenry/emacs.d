(defun ps-print-buffer-or-region ()
  "Convert buffer or region to pdf, and print it. Ghostscript is required."
	(interactive)
	;; create temp buffer and auto-fill, if needed
	(if (> (longest-line-in-buffer) ps-printer-line-length)
	  (fill-to-printer) ; otherwise, just
	  (copy-current-to-temp-buffer))

	;; print temp buffer and discard
	(let ((filename (buffer-name)))
	(ps-print-buffer (concat filename ".ps"))
	(kill-buffer)

	(when (executable-find "gs")
	  (shell-command (concat "ps2pdfwr " filename ".ps"))
	  (shell-command (concat "pdfcrop " filename ".pdf " filename "-cropped.pdf"))
	  (shell-command (concat "lp -d " ps-printer-name " -o media=" (symbol-name 'ps-paper-type)
		" -o fit-to-page " filename "-cropped.pdf"))

	  (delete-file (concat filename ".ps") t)
	  (delete-file (concat filename ".pdf") t)
	  (delete-file (concat filename "-cropped.pdf") t))))
