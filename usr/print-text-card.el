;;; print-text-card.el --- Render text cards with LaTeX -*- lexical-binding: t; -*-

;;; Commentary:

;; Render the current buffer or active region as a 5x3 landscape PDF card.
;; This is a card-native LaTeX renderer, separate from the paps-based
;; `print-buffer-or-region' pipeline.

;;; Code:

(defgroup print-text-card nil
  "Render plain text cards through LaTeX."
  :group 'printing)

(defcustom print-text-card-save-output nil
  "When non-nil, save generated PDFs to a persistent destination.

When nil, leave generated PDFs in their temporary location and open
that file directly."
  :type 'boolean
  :group 'print-text-card)

(defcustom print-text-card-destination-directory "~/Downloads/"
  "Directory used when `print-text-card-save-output' is non-nil."
  :type 'directory
  :group 'print-text-card)

(defcustom print-text-card-font "Courier Prime"
  "Font used for text cards."
  :type 'string
  :group 'print-text-card)

(defcustom print-text-card-font-size "15pt"
  "Font size used for text cards."
  :type 'string
  :group 'print-text-card)

(defcustom print-text-card-margin "0.18in"
  "Margin used for text cards."
  :type 'string
  :group 'print-text-card)

(defcustom print-text-card-engine "xelatex"
  "LaTeX engine used to render text cards."
  :type '(choice (const "xelatex")
                 (const "lualatex"))
  :group 'print-text-card)

(defun print-text-card--latex-escape (text)
  "Escape TEXT for safe insertion into LaTeX."
  (let ((pairs '(("\\" . "\\textbackslash{}")
                 ("{"  . "\\{")
                 ("}"  . "\\}")
                 ("$"  . "\\$")
                 ("&"  . "\\&")
                 ("%"  . "\\%")
                 ("#"  . "\\#")
                 ("_"  . "\\_")
                 ("~"  . "\\textasciitilde{}")
                 ("^"  . "\\textasciicircum{}"))))
    (dolist (pair pairs text)
      (setq text
            (replace-regexp-in-string
             (regexp-quote (car pair))
             (cdr pair)
             text t t)))))

(defun print-text-card--latex-body (text)
  "Return escaped LaTeX body for TEXT."
  ;; Normalize paragraph spacing.
  (setq text
        (replace-regexp-in-string
         "\n\\{3,\\}" "\n\n" text))

  ;; Add breathing room before Org headings,
  ;; except at beginning of buffer.
  (setq text
        (replace-regexp-in-string
         "\\(.\\)\n\\(\\*+ .+\\)"
         "\\1\n\n\\2"
         text))

  ;; Escape only after structural text shaping.
  (print-text-card--latex-escape text))

(defun print-text-card--tex-document (body)
  "Return complete LaTeX document for BODY."
  (format "\\documentclass{article}
\\usepackage[
  paperwidth=5in,
  paperheight=3in,
  margin=%s
]{geometry}
\\usepackage{fontspec}
\\setmainfont{%s}
\\pagestyle{empty}
\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{0.45em}
\\begin{document}
\\fontsize{%s}{1.15\\fontdimen6\\font}\\selectfont
%s
\\end{document}
"
          print-text-card-margin
          print-text-card-font
          print-text-card-font-size
          body))

(defun print-text-card (&optional toggle-save-output)
  "Render buffer or region as a 5x3 landscape PDF card.

Normally obey `print-text-card-save-output'.

With prefix arg TOGGLE-SAVE-OUTPUT, temporarily invert
`print-text-card-save-output' for this invocation."
  (interactive "P")
  (let* ((engine (executable-find print-text-card-engine))
         (open (executable-find "open"))
         (save-output
          (if toggle-save-output
              (not print-text-card-save-output)
            print-text-card-save-output)))
    (unless (and engine open)
      (user-error "Missing tool(s): %s"
                  (string-join
                   (delq nil
                         (list (unless engine print-text-card-engine)
                               (unless open "open")))
                   ", ")))

    (let* ((source-name (file-name-base
                         (or (buffer-file-name)
                             (buffer-name))))
           (workdir (make-temp-file "text-card-" t))
           (tex-file (expand-file-name
                      (format "%s-card.tex" source-name)
                      workdir))
           (pdf-file (expand-file-name
                      (format "%s-card.pdf" source-name)
                      workdir))
           (dest-dir (expand-file-name print-text-card-destination-directory))
           (dest-file (expand-file-name
                       (format "%s-card-%s.pdf"
                               source-name
                               (format-time-string "%Y-%m-%d_%H-%M-%S"))
                       dest-dir))
           (text (buffer-substring-no-properties
                  (if (use-region-p) (region-beginning) (point-min))
                  (if (use-region-p) (region-end) (point-max))))
           (body (print-text-card--latex-body text))
           (doc (print-text-card--tex-document body)))
      (unwind-protect
          (progn
            (when save-output
              (unless (file-directory-p dest-dir)
                (make-directory dest-dir t)))

            (let ((coding-system-for-write 'utf-8-unix))
              (write-region doc nil tex-file nil 'silent))

            (let ((buf (get-buffer-create "*text-card-latex*")))
              (with-current-buffer buf
                (erase-buffer))
	      (let ((default-directory workdir)
		    (code (call-process engine nil buf t
					"-interaction=nonstopmode"
					"-halt-on-error"
					"-output-directory" workdir
					(file-name-nondirectory tex-file))))
                (if (and (eq code 0) (file-exists-p pdf-file))
                    (kill-buffer buf)
                  (pop-to-buffer buf)
                  (error "%s failed (exit %s)" print-text-card-engine code))))

            (if save-output
                (progn
                  (copy-file pdf-file dest-file t)
                  (message "Saved: %s" dest-file)
                  (call-process open nil 0 nil dest-file))
              (message "Opened temporary text card PDF: %s" pdf-file)
              (call-process open nil 0 nil pdf-file)))

        (when save-output
          (when (file-directory-p workdir)
            (delete-directory workdir t)))))))

(provide 'print-text-card)

;;; print-text-card.el ends here
