(defun print-buffer-or-region (parg)
"Print buffer or region.

- With no prefix, use a5 printer.
- With one prefix arg, use POS printer."
	(interactive "P")
	(let ((prefix (car parg))) (cond
		((not prefix)(setq
			printer-name "Brother_HL_L2370DW"
			lpr-switches '("-o media=a5 -o cpi=9.5 -o lpi=7")))
		((= prefix 4)(setq
			printer-name "Munbyn_ITPP047"
			lpr-switches '("-o media=pos80 cpi=13 -o lpi=9")))
		))
	(let ((beg (point-min)) (end (point-max)))
	(when (region-active-p)
		(setq beg (region-beginning))
		(setq end (region-end)))
	(print-region beg end)))
