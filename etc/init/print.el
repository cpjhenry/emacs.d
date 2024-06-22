;; PRINT functions

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

(defun fill-to-receipt-printer ()
	"Re-formats text in current buffer for POS printer."
	(interactive)
	(copy-current-buffer-to-temp-buffer)
	(text-mode)
	(setq fill-column 32)
	(fill-region (point-min) (point-max)))

(defun fill-to-a5-printer ()
	"Re-formats text in current buffer for a5 printer."
	(interactive)
	(copy-current-buffer-to-temp-buffer)
	(text-mode)
	(setq fill-column 50)
	(fill-region (point-min) (point-max)))

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
(defadvice ps-do-despool (before ps-2-ps activate)
   "we apply the ps2ps command to the postscript buffer just before printing"
   (if (or (not (boundp 'ps-spool-buffer))
           (not (symbol-value 'ps-spool-buffer)))
       (message "No spooled PostScript to print")
    (save-excursion
     (set-buffer ps-spool-buffer)
     (shell-command-on-region
        (point-min) (point-max) "ps2ps - -" nil t "*Message*"))))
