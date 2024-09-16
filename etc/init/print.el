;; PRINT functions

(setq ps-page-dimensions-database '(
	(a4 595 842 "A4")
	(a5 420 595 "A5")
	(b4 709 1001 "B4")
	(b5 499 709 "B5")
	(letter 612.0 792.0 "Letter")
	(legal 612.0 1008.0 "Legal")
	(tabloid 792.0 1224.0 "Tabloid")
	(ledger 1224.0 792.0 "Ledger")
	(statement 396.0 612.0 "Statement")
	(executive 540.0 720.0 "Executive")
	(pos80 204.0 595.0 "POS-80")))

(when *mac* (setq
	ps-printer-name "Brother_HL_L2370DW"
	ps-lpr-switches '("-o media=a5")
	ps-paper-type 'a5

	ps-font-size 12
	ps-font-family 'Courier
	ps-footer-font-family 'Courier
	ps-print-color-p nil
	ps-print-header nil

	ps-print-footer t
	ps-print-footer-frame nil
	ps-footer-lines 1
	ps-right-footer nil
	ps-left-footer (list (concat
	"{pagenumberstring dup stringwidth pop"
	" 2 div PrintWidth 2 div exch sub 0 rmoveto}"))

	ps-top-margin 42
	ps-bottom-margin 14
	ps-left-margin 28
	ps-right-margin 28))

(defun fill-to-printer (&optional parg)
	"Re-formats text in current buffer for printer."
	(interactive "P")
	(copy-current-to-temp-buffer)
	(text-mode)
	(let ((prefix (car parg))) (cond
		((not prefix)(setq fill-column 50))
		((= prefix 4)(setq fill-column 32)) ))
	(fill-region (point-min) (point-max)))

(defun print-buffer-or-region (parg)
"Print buffer or region.

- With no prefix, use a5 printer.
- With one prefix arg, use POS printer."
	(interactive "P")
	(let ((prefix (car parg))) (cond
		((not prefix)(setq
			printer-name "Brother_HL_L2370DW"
			lpr-switches '("-o media=a5 -o cpi=12 -o lpi=8")))
		((= prefix 4)(setq
			printer-name "Munbyn_ITPP047"
			lpr-switches '("-o media=pos80 cpi=13 -o lpi=9")))
		))
	(let ((beg (point-min)) (end (point-max)))
	(when (region-active-p)
		(setq beg (region-beginning))
		(setq end (region-end)))
	(print-region beg end)))

(defun ps-print-buffer-or-region ()
"Convert buffer or region to pdf, and print it. Ghostscript is required."
	(interactive)
	(fill-to-printer)
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

;; https://stackoverflow.com/questions/15869131/emacs-shell-command-on-buffer (adapted)
;; https://stackoverflow.com/questions/1548605/emacs-lisp-shell-command-on-region (adapted)
(defun enscript ()
	"Sends current buffer or region to 'enscript'."
	(interactive)
	(let ((beg (point-min)) (end (point-max)) (enscript "enscript -cqB"))
	(when (region-active-p)
		(setq beg (region-beginning))
		(setq end (region-end)))
	(shell-command-on-region beg end enscript)))

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

;; Local Variables:
;; truncate-lines: -1
;; End:
