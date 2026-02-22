;;; print-functions.el --- PRINT functions
;;; commentary:

;;; code:
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

(defvar ps-printer-line-length 50)
(when *mac* (setq
	printer-name "Brother_HL_L2370DW"
	lpr-switches '("-o media=a5")

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

(defun enscript (parg)
"Sends current buffer or region to 'enscript'. Prefix fills to printer before printing."
	(interactive "P")
	(let ((prefix (car parg))) (cond
		((not prefix))
		((= prefix 4) (fill-to-printer '(4)))))
	(let ((beg (point-min)) (end (point-max)) (enscript "enscript -cqB"))
	(when (region-active-p)
		(setq beg (region-beginning))
		(setq end (region-end)))
	(shell-command-on-region beg end enscript)))

(defun fill-to-printer (&optional parg)
	"Re-formats text in current buffer for printer."
	(interactive "P")
	(copy-current-to-temp-buffer)
	(text-mode)
	(let ((prefix (car parg))) (cond
		((not prefix)(setq fill-column ps-printer-line-length))
		((= prefix 4)(setq fill-column 32)) ))
	(fill-region (point-min) (point-max)))

;; Local Variables:
;; truncate-lines: -1
;; End:

; LocalWords:  Ghostscript Munbyn

;;; print-functions.el ends here
