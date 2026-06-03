;;; print-buffer-or-region.el --- Prints to PDF, opens Preview -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defvar org-ascii-text-width)

(defgroup print-buffer-or-region nil
  "Print buffer or region through a PDF typesetting pipeline."
  :group 'printing)

(defcustom print-buffer-or-region-save-output nil
  "When non-nil, save the generated PDF to a persistent destination.

When nil, leave the generated PDF in its temporary location and open
that file directly."
  :type 'boolean
  :group 'print-buffer-or-region)

;; NOTE: Reserved for future page-numbering support.
(defcustom print-buffer-or-region-page-numbers nil
  "When non-nil, add centred footer page numbers to generated PDFs."
  :type 'boolean
  :group 'print-buffer-or-region)

(defun insert-page-breaks-every-n-lines (n)
  "Insert form-feed page breaks every N lines. Return number inserted."
  (interactive "nLines per page: ")
  (save-excursion
    (goto-char (point-min))
    (let ((i 0) (ins 0))
      (while (not (eobp))
        (setq i (1+ i))
        (forward-line 1)
        (when (and (= i n) (not (eobp)))
          (insert "\f\n")
          (setq i 0)
          (setq ins (1+ ins))))
      (message "Inserted %d page break(s)" ins)
      ins)))

(defun insert-page-breaks-max-lines (max-lines)
  "Insert page breaks so no page exceeds MAX-LINES.
Prefer to break at the last blank line before the limit."
  (interactive "nMax lines per page: ")
  (save-excursion
    (goto-char (point-min))
    (let ((line 0)
          (last-blank-pos nil))
      (while (not (eobp))
        (setq line (1+ line))
        (when (looking-at-p "^[[:space:]]*$")
          (setq last-blank-pos (point)))
        (forward-line 1)
        (when (and (>= line max-lines) (not (eobp)))
          (goto-char (or last-blank-pos (line-beginning-position)))
          (insert "\f\n")
          ;; reset counters for next page
          (setq line 0
                last-blank-pos nil))))))

(defun insert-page-breaks-before-months ()
  "Insert form-feed page breaks before month headers like:

February
--------------------------------

Skips the first such header in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((months (regexp-opt
                   '("January" "February" "March" "April" "May" "June"
                     "July" "August" "September" "October" "November" "December")
                   'words))
          (first t)
          (count 0))
      (while (re-search-forward
              (concat "^\\(" months "\\)\\s-*$\n^-\\{8,\\}\\s-*$")
              nil t)
        (unless first
          (goto-char (match-beginning 0))
          (unless (bolp) (beginning-of-line))
          (insert "\f\n")
          (setq count (1+ count))
          ;; Move past the inserted break so we don't match the same header again
          (forward-line 1))
        (setq first nil))
      (message "Inserted %d month page break(s)" count)
      count)))

(defconst print-a5-profile
  '(:font "Noto Sans Mono CJK SC 17"
    :margins ("20" "25" "20" "20")
    :trim "15mm 25mm 25mm 15mm"
    :scale "0.85"
    :delta "5mm 5mm"))

(defun longest-line-in-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let ((max-len 0)
          (curr-len 0))
      (cl-loop when (eobp)
	         return (max curr-len max-len)
               if (= (char-after) ?\n)
                 do (setq max-len (max curr-len max-len))
                   and do (setq curr-len 0)
               else
                 do (cl-incf curr-len)
               do (forward-char 1))
                 )))

(declare-function copy-current-to-temp-buffer "copy-current-to-temp-buffer")
(declare-function fill-to-printer "fill-to-printer")

(defun print-buffer-or-region (&optional toggle-save-output)
  "Unicode-safe: text -> paps -> gs -> pdfcrop -> save/open PDF.

Normally obey `print-buffer-or-region-save-output'.

With prefix arg TOGGLE-SAVE-OUTPUT, temporarily invert
`print-buffer-or-region-save-output' for this invocation."
  (interactive "P")
  (require 'cl-lib)

  (save-window-excursion
    (let ((paps    (executable-find "paps"))
          (gs      (executable-find "gs"))
          (pdfcrop (executable-find "pdfcrop"))
          (open    (executable-find "open"))

          (ps-printer-line-length org-ascii-text-width)
          (save-output
           (if toggle-save-output
               (not print-buffer-or-region-save-output)
             print-buffer-or-region-save-output)))

      (unless (and paps gs pdfcrop open)
        (user-error "Missing tool(s): %s"
                    (string-join
                     (delq nil
                           (list (unless paps "paps")
                                 (unless gs "gs")
                                 (unless pdfcrop "pdfcrop")
                                 (unless open "open")))
                     ", ")))

      ;; If `visual-line-mode' is enabled (which includes wordwrap),
      ;; invoke `fill-region' on buffer to enforce hard breaks
      ;; before printing.
      (if (> (longest-line-in-buffer) ps-printer-line-length)
          (fill-to-printer)
        (copy-current-to-temp-buffer))

      ;; `insert-page-breaks...' helpers go here, if needed.

      (let* ((src-buf (current-buffer))
             (workdir (make-temp-file "emacs-paps-" t))
             (base (file-name-base (or (buffer-file-name src-buf)
                                       (buffer-name src-buf))))
             (txt-file   (expand-file-name (format "%s.txt" base) workdir))
             (ps-file    (expand-file-name (format "%s.ps" base) workdir))
             (pdf-file   (expand-file-name (format "%s.pdf" base) workdir))
             (crop-file  (expand-file-name (format "%s-cropped.pdf" base) workdir))
             (dest-dir   (expand-file-name "~/Downloads/"))
             (dest-file  (expand-file-name
                          (format "%s-%s.pdf"
                                  base
                                  (format-time-string "%Y-%m-%d_%H-%M-%S"))
                          dest-dir)))
        (unwind-protect
            (progn
              (when save-output
                (unless (file-directory-p dest-dir)
                  (make-directory dest-dir t)))

              ;; 1) Write UTF-8 text.
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region (if (use-region-p) (region-beginning) (point-min))
                              (if (use-region-p) (region-end) (point-max))
                              txt-file nil 'silent))

              ;; 2) paps -> PS.
              (let ((buf (get-buffer-create "*paps*")))
                (with-current-buffer buf
                  (erase-buffer))
                (let ((code (call-process paps nil buf t
                                          "--font" "Noto Sans Mono CJK SC 17"
                                          "--top-margin" "20"
                                          "--bottom-margin" "25"
                                          "--left-margin" "20"
                                          "--right-margin" "20"
                                          "-o" ps-file
                                          txt-file)))
                  (if (and (eq 0 code) (file-exists-p ps-file))
                      (kill-buffer buf)
                    (pop-to-buffer buf)
                    (error "paps failed (exit %s)" code))))

              ;; 3) gs -> PDF.
              (let ((code (call-process gs nil nil nil
                                        "-dSAFER" "-dBATCH" "-dNOPAUSE"
                                        "-sDEVICE=pdfwrite"
                                        "-dEmbedAllFonts=true"
                                        "-dSubsetFonts=true"
                                        (concat "-sOutputFile=" pdf-file)
                                        ps-file)))
                (unless (and (eq 0 code) (file-exists-p pdf-file))
                  (with-current-buffer (get-buffer-create "*ps2pdf*")
                    (erase-buffer))
                  (call-process gs nil "*ps2pdf*" t
                                "-dSAFER" "-dBATCH" "-dNOPAUSE"
                                "-sDEVICE=pdfwrite"
                                "-dEmbedAllFonts=true"
                                "-dSubsetFonts=true"
                                (concat "-sOutputFile=" pdf-file)
                                ps-file)
                  (pop-to-buffer "*ps2pdf*")
                  (error "gs pdfwrite failed (exit %s)" code)))

              ;; 4) Impose onto A5.
              (let* ((pdfjam  (executable-find "pdfjam"))
                     (a5-file (expand-file-name
                               (format "%s-a5.pdf" base)
                               workdir)))
                (unless pdfjam
                  (user-error "`pdfjam' not found in exec-path"))

                (let ((buf (get-buffer-create "*pdfjam*")))
                  (with-current-buffer buf
                    (erase-buffer))
                  (let ((code (call-process pdfjam nil buf t
                                            pdf-file
                                            "--paper" "a5paper"
                                            "--trim"  "15mm 25mm 25mm 15mm"
                                            "--scale" "0.85"
                                            "--delta" "5mm 5mm"
                                            "--outfile" a5-file)))
                    (if (and (eq 0 code) (file-exists-p a5-file))
                        (kill-buffer buf)
                      (pop-to-buffer buf)
                      (error "pdfjam failed (exit %s)" code))))

                (if save-output
                    (progn
                      (copy-file a5-file dest-file t)
                      (message "Saved: %s" dest-file)
                      (call-process open nil 0 nil dest-file))
                  (message "Opened temporary print PDF: %s" a5-file)
                  (call-process open nil 0 nil a5-file))))

          (when (buffer-live-p src-buf)
            (kill-buffer src-buf))

          (cond
           ((not save-output)
            (message "Opened temporary PDF."))

           (t
            (when (file-directory-p workdir)
              (delete-directory workdir t)))))))))

;;; print-buffer-or-region.el ends here

; LocalWords:  nLines nMax Noto arg dSAFER dBATCH dNOPAUSE sDEVICE
; LocalWords:  pdfwrite dEmbedAllFonts dSubsetFonts sOutputFile
; LocalWords:  pdfjam outfile
