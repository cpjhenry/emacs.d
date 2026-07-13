;;; print-text-latex.el --- Render text PDFs with LaTeX -*- lexical-binding: t; -*-

;;; Commentary:

;; Render the current buffer or active region as a PDF using LaTeX.
;;
;; This package creates a temporary LaTeX document from plain text,
;; renders it with XeLaTeX or LuaLaTeX, and opens the resulting PDF.
;; Output profiles provide different page geometries, such as A5 pages
;; and 3x5 landscape cards.

;; TODO: Consider separate prose and line-preserving render modes.
;; Calendars, schedules, checklists, and reports often require exact
;; source line preservation rather than paragraph reflow.

;; Design note:
;;
;; `print-text-latex' currently treats input as literal text and
;; preserves line structure when generating the PDF. It does not
;; perform paragraph reflow, whitespace normalization, or other
;; prose-oriented transformations prior to typesetting.
;;
;; Users are expected to prepare the source text in its desired
;; printable form before rendering.
;;
;; For mnemonic and prose-oriented 3×5 cards, see `print-text-card',
;; which performs additional text shaping before PDF generation.
;;
;; Future versions may distinguish between literal and prose
;; rendering modes.

;;; Code:
(defgroup print-text-latex nil
  "Render plain text PDFs through LaTeX."
  :group 'printing)

(defcustom print-text-latex-save-output nil
  "When non-nil, save generated PDFs to a persistent destination.

When nil, leave generated PDFs in their temporary location and open
that file directly."
  :type 'boolean
  :group 'print-text-latex)

(defcustom print-text-latex-destination-directory "~/Downloads/"
  "Directory used when `print-text-latex-save-output' is non-nil."
  :type 'directory
  :group 'print-text-latex)

(defcustom print-text-latex-engine "xelatex"
  "LaTeX engine used to render text PDFs."
  :type '(choice (const "xelatex")
                 (const "lualatex"))
  :group 'print-text-latex)

(defcustom print-text-latex-profile 'a5
  "Default LaTeX text rendering profile."
  :type '(choice (const :tag "A5" a5)
                 (const :tag "3x5 card" card-3x5))
  :group 'print-text-latex)

(defconst print-text-latex-profiles
  '((a5
     :suffix "-a5"
     :paperwidth "148mm"
     :paperheight "210mm"
     :top "7mm"
     :bottom "7mm"
     :left "7mm"
     :right "7mm"
     :includefoot t
     :font "Courier Prime"
     :font-size "13pt"
     :leading "15pt"
     :parskip "0.6em"
     :pagestyle "plain")

    (card-3x5
     :suffix "-card"
     :paperwidth "5in"
     :paperheight "3in"
     :top "0.18mm"
     :bottom "0.18mm"
     :left "0.18mm"
     :right "0.18mm"
     :font "Courier Prime"
     :font-size "13pt"
     :leading "15pt"
     :parskip "0.45em"
     :pagestyle "empty"))
  "Profiles for LaTeX text rendering.")

(defun print-text-latex--profile (profile)
  "Return plist for PROFILE."
  (or (cdr (assq profile print-text-latex-profiles))
      (user-error "Unknown text LaTeX profile: %s" profile)))

(defun print-text-latex--escape (text)
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
             (lambda (_) (cdr pair))
             text t t)))))

(defun print-text-latex--normalize (text)
  "Normalize TEXT before LaTeX escaping."
  (dolist (pair '(("’" . "'")
                  ("‘" . "'")
                  ("ʼ" . "'")
                  ("‛" . "'")
                  ("“" . "\"")
                  ("”" . "\"")
                  ("–" . "--")
                  ("—" . "---"))
                text)
    (setq text
          (replace-regexp-in-string
           (regexp-quote (car pair))
           (lambda (_) (cdr pair))
           text t t))))

(defun print-text-latex--bold-face-p (face)
  "Return non-nil when FACE should print as bold."
  (cond
   ((null face)
    nil)

   ;; Translate colour-based agenda emphasis into bold for
   ;; monochrome printing.
   ((memq face '(org-agenda-date
                 org-agenda-structure))
    t)

   ;; A named face.
   ((symbolp face)
    (memq (face-attribute face :weight nil t)
          '(semi-bold bold extra-bold ultra-bold)))

   ;; An anonymous face specification such as (:weight bold).
   ((keywordp (car-safe face))
    (or (memq (plist-get face :weight)
              '(semi-bold bold extra-bold ultra-bold))
        (print-text-latex--bold-face-p
         (plist-get face :inherit))))

   ;; A list of faces.
   ((listp face)
    (cl-some #'print-text-latex--bold-face-p face))

   (t nil)))

(defun print-text-latex--bold-at-p (position text)
  "Return non-nil when TEXT is bold at POSITION."
  (or (print-text-latex--bold-face-p
       (get-text-property position 'face text))
      (print-text-latex--bold-face-p
       (get-text-property position 'font-lock-face text))))

(defun print-text-latex--escape-fontified (text)
  "Escape TEXT for LaTeX, preserving bold fontification."
  (let ((position 0)
        (end (length text))
        pieces)
    (while (< position end)
      (let* ((next
              (or (next-property-change position text)
                  end))
             (raw
              (substring-no-properties text position next))
             (escaped
              (print-text-latex--escape raw)))
        (push
         (if (and (print-text-latex--bold-at-p position text)
                  (not (string-empty-p raw)))
             (concat "\\textbf{" escaped "}")
           escaped)
         pieces)
        (setq position next)))
    (apply #'concat (nreverse pieces))))

(defun print-text-latex--body (text)
  "Return escaped LaTeX body for TEXT.

Preserve bold text represented by `face' or `font-lock-face'
properties."
  (let ((hard-break "§§LATEX-HARD-BREAK§§")
        (rule-line "§§LATEX-RULE-LINE§§"))

    ;; Normalize excessive blank lines.
    (setq text
          (replace-regexp-in-string
           "\n\\{3,\\}" "\n\n" text))

    ;; Add breathing room before Org headings,
    ;; except at the beginning of the buffer.
    (setq text
          (replace-regexp-in-string
           "\\(.\\)\n\\(\\*+ .+\\)"
           "\\1\n\n\\2"
           text))

    ;; Replace lines of repeated dashes with a horizontal rule.
    (setq text
          (replace-regexp-in-string
           "^-\\{4,\\}$"
           rule-line
           text))

    ;; Preserve remaining hard line breaks.
    (setq text
          (replace-regexp-in-string
           "\\([^\n]\\)\n\\([^\n]\\)"
           (concat "\\1" hard-break "\n\\2")
           text))

    ;; Normalize smart punctuation that may render awkwardly in
    ;; monospaced CJK fonts.
    ;; (setq text (print-text-latex--normalize text))

    ;; Escape ordinary text while translating bold fontification.
    (setq text
          (print-text-latex--escape-fontified text))

    ;; Restore intentional LaTeX constructs.
    (setq text
          (replace-regexp-in-string
           rule-line
           (lambda (_)
             "\\noindent\\rule{\\linewidth}{0.4pt}")
           text t t))

    (replace-regexp-in-string
     hard-break
     (lambda (_)
       "\\\\")
     text t t)))

(defun print-text-latex--document (body profile)
  "Return complete LaTeX document for BODY using PROFILE."
  (let ((geometry-options
         (string-join
          (delq nil
                (list
                 (format "paperwidth=%s"
                         (plist-get profile :paperwidth))
                 (format "paperheight=%s"
                         (plist-get profile :paperheight))
                 (format "top=%s"
                         (plist-get profile :top))
                 (format "bottom=%s"
                         (plist-get profile :bottom))
                 (format "left=%s"
                         (plist-get profile :left))
                 (format "right=%s"
                         (plist-get profile :right))
                 (when (plist-get profile :includehead)
                   "includehead")
                 (when (plist-get profile :includefoot)
                   "includefoot")))
          ",\n  ")))
    (format "\\documentclass{article}
\\usepackage[
  %s
]{geometry}
\\usepackage{fontspec}
\\setmainfont{%s}
\\pagestyle{%s}

\\usepackage[none]{hyphenat}
\\usepackage{ragged2e}

\\hyphenpenalty=10000
\\exhyphenpenalty=10000
\\emergencystretch=2em
\\RaggedRight

\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{%s}
\\begin{document}
\\fontsize{%s}{%s}\\selectfont
%s
\\end{document}
"
            geometry-options
            (plist-get profile :font)
            (plist-get profile :pagestyle)
            (plist-get profile :parskip)
            (plist-get profile :font-size)
            (plist-get profile :leading)
            body)))

(defun print-text-latex (&optional toggle-save-output)
  "Render buffer or region as a PDF using LaTeX.

Normally obey `print-text-latex-save-output' and
`print-text-latex-profile'.

With prefix arg TOGGLE-SAVE-OUTPUT, temporarily invert
`print-text-latex-save-output' for this invocation.

Preserve text properties so that `print-text-latex--body' can
translate existing fontification into LaTeX."
  (interactive "P")
  (let* ((profile
          (print-text-latex--profile print-text-latex-profile))
         (engine
          (executable-find print-text-latex-engine))
         (open
          (executable-find "open"))
         (save-output
          (if toggle-save-output
              (not print-text-latex-save-output)
            print-text-latex-save-output))
         (beg
          (if (use-region-p)
              (region-beginning)
            (point-min)))
         (end
          (if (use-region-p)
              (region-end)
            (point-max))))

    (unless (and engine open)
      (user-error
       "Missing tool(s): %s"
       (string-join
        (delq nil
              (list
               (unless engine print-text-latex-engine)
               (unless open "open")))
        ", ")))

    ;; Ensure that font-lock faces have been applied before copying
    ;; the text with its properties.
    (font-lock-ensure beg end)

    (let* ((source-name
            (file-name-base
             (or (buffer-file-name)
                 (buffer-name))))
           (suffix
            (plist-get profile :suffix))
           (workdir
            (make-temp-file "text-latex-" t))
           (tex-file
            (expand-file-name
             (format "%s%s.tex" source-name suffix)
             workdir))
           (pdf-file
            (expand-file-name
             (format "%s%s.pdf" source-name suffix)
             workdir))
           (dest-dir
            (expand-file-name
             print-text-latex-destination-directory))
           (dest-file
            (expand-file-name
             (format "%s%s-%s.pdf"
                     source-name
                     suffix
                     (format-time-string "%Y-%m-%d_%H-%M-%S"))
             dest-dir))
           ;; Preserve `face', `font-lock-face', and other text
           ;; properties for interpretation by the body renderer.
           (text
	    (replace-regexp-in-string
	     "┄┄┄┄┄" ""
	     (buffer-substring beg end)
	     nil 'literal))
           (body
            (print-text-latex--body text))
           (doc
            (print-text-latex--document body profile)))

      (unwind-protect
          (progn
            (when save-output
              (unless (file-directory-p dest-dir)
                (make-directory dest-dir t)))

            (let ((coding-system-for-write 'utf-8-unix))
              (write-region doc nil tex-file nil 'silent))

            (let ((buf
                   (get-buffer-create "*print-text-latex*")))
              (with-current-buffer buf
                (erase-buffer))

              (let ((default-directory workdir)
                    (code
                     (call-process
                      engine nil buf t
                      "-interaction=nonstopmode"
                      "-halt-on-error"
                      "-output-directory" workdir
                      (file-name-nondirectory tex-file))))
                (if (and (eq code 0)
                         (file-exists-p pdf-file))
                    (kill-buffer buf)
                  (pop-to-buffer buf)
                  (error "%s failed (exit %s)"
                         print-text-latex-engine
                         code))))

            (if save-output
                (progn
                  (copy-file pdf-file dest-file t)
                  (message "Saved: %s" dest-file)
                  (call-process open nil 0 nil dest-file))
              (message "Opened temporary text PDF: %s" pdf-file)
              (call-process open nil 0 nil pdf-file)))

        (when (and save-output
                   (file-directory-p workdir))
          (delete-directory workdir t))))))

(defun print-text-a5 (&optional toggle-save-output)
  "Render buffer or region as an A5 PDF using LaTeX.

With prefix arg TOGGLE-SAVE-OUTPUT, temporarily invert
`print-text-latex-save-output' for this invocation."
  (interactive "P")
  (let ((print-text-latex-profile 'a5))
    (print-text-latex toggle-save-output)))

(defun print-text-card-3x5 (&optional toggle-save-output)
  "Render buffer or region as a 3x5 landscape PDF card using LaTeX.

With prefix arg TOGGLE-SAVE-OUTPUT, temporarily invert
`print-text-latex-save-output' for this invocation."
  (interactive "P")
  (let ((print-text-latex-profile 'card-3x5))
    (print-text-latex toggle-save-output)))

(provide 'print-text-latex)

;;; print-text-latex.el ends here

; LocalWords:  plist textbackslash textasciitilde textasciicircum arg
; LocalWords:  noindent linewidth documentclass usepackage paperwidth
; LocalWords:  paperheight fontspec setmainfont pagestyle setlength
; LocalWords:  parindent parskip fontsize selectfont nonstopmode
