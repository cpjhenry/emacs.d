;;; ox-rapport.el --- English report/minutes exporter for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Christopher J. H. (with Sage)
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: outlines, wp, tex
;; URL: local

;;; Commentary:

;; `ox-rapport' is a small Org export backend for meeting minutes and
;; reports.  It recognises English metadata names, despite its French
;; name.
;;
;; Example:
;;
;;   #+title: Committee of General Purpose
;;   #+author: Senior Warden
;;   #+date: 28 July 2026
;;   #+location: Goodwood Lodge No. 159 GRC
;;   #+start: 19:35
;;   #+end: 20:53
;;   #+project: Pig Roast Planning
;;   #+present: Paul, Jimmy, Dax
;;   #+logo: ~/Documents/AFAM/Goodwood/compass.jpg
;;
;; Missing metadata is omitted rather than represented by an empty
;; label.  Duration is calculated from START and END when both are
;; present.

;;; Code:

(require 'ox)
(require 'ox-latex)
(require 'seq)
(require 'subr-x)

(defgroup ox-rapport nil
  "Export Org meeting reports through LaTeX."
  :tag "Org Rapport"
  :group 'org-export)

(defcustom ox-rapport-latex-class "rapport"
  "LaTeX class used by the Rapport exporter."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-font-size "12pt"
  "Base font size passed to the LaTeX document class."
  :type '(choice
          (const :tag "10 point" "10pt")
          (const :tag "11 point" "11pt")
          (const :tag "12 point" "12pt")
          string)
  :group 'ox-rapport)

(defcustom ox-rapport-paper-size "letterpaper"
  "Paper size passed to the LaTeX `geometry' package."
  :type '(choice
          (const :tag "US Letter" "letterpaper")
          (const :tag "A4" "a4paper")
          (const :tag "US Legal" "legalpaper")
          string)
  :group 'ox-rapport)

(defcustom ox-rapport-left-margin "22mm"
  "Left page margin passed to the LaTeX `geometry' package."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-right-margin "22mm"
  "Right page margin passed to the LaTeX `geometry' package."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-top-margin "20mm"
  "Top page margin passed to the LaTeX `geometry' package."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-bottom-margin "20mm"
  "Bottom page margin passed to the LaTeX `geometry' package."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-project-suffix " Meeting"
  "Text appended to PROJECT in the title block.

An empty string leaves the project unchanged.  The suffix is not added
when the project already ends with the same text, ignoring case."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-logo-height "24mm"
  "Maximum rendered height of the report logo."
  :type 'string
  :group 'ox-rapport)

(defcustom ox-rapport-show-date-strip t
  "When non-nil, show the date and duration strip below attendance."
  :type 'boolean
  :group 'ox-rapport)

(defconst ox-rapport--latex-class
  (list
   ox-rapport-latex-class
   (format
    "\\documentclass[%s]{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]

\\usepackage[
  %s,
  left=%s,
  right=%s,
  top=%s,
  bottom=%s,
  headheight=15pt,
  headsep=8mm,
  footskip=14mm
]{geometry}

\\usepackage{fontspec}
\\usepackage{graphicx}
\\usepackage[dvipsnames,table]{xcolor}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{array}
\\usepackage{tabularx}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{enumitem}
\\usepackage{microtype}
\\usepackage{fancyhdr}
\\usepackage{lastpage}
\\usepackage{hyperref}
\\usepackage{rotating}

\\definecolor{rapportgray}{HTML}{A19589}
\\definecolor{rapportlightgray}{HTML}{D8D0C7}

\\microtypesetup{
  protrusion=true,
  final
}

\\setlength{\\parindent}{0pt}
\\setlength{\\parskip}{0.55em}

\\setlist{
  topsep=0.35em,
  itemsep=0.15em,
  parsep=0pt,
  partopsep=0pt
}

\\newcommand{\\rapportseparator}{%%
  \\par\\nointerlineskip
  \\vspace{0.45em}
  \\textcolor{rapportgray}{%%
    \\rule{\\textwidth}{0.35pt}}%%
  \\par\\nointerlineskip
  \\vspace{0.55em}
}

\\newcommand{\\rapportlabel}[1]{%%
  \\normalsize\\scshape\\textcolor{rapportgray}{#1}%%
}

\\pagestyle{fancy}
\\fancyhead{}
\\fancyfoot{}
\\renewcommand{\\headrulewidth}{0pt}
\\renewcommand{\\footrulewidth}{0pt}"
    ox-rapport-font-size
    ox-rapport-paper-size
    ox-rapport-left-margin
    ox-rapport-right-margin
    ox-rapport-top-margin
    ox-rapport-bottom-margin)
   '("\\section{%s}" . "\\section*{%s}")
   '("\\subsection{%s}" . "\\subsection*{%s}")
   '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   '("\\paragraph{%s}" . "\\paragraph*{%s}")
   '("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  "LaTeX class definition installed by `ox-rapport'.")

;; Replace any older registration when this file is reloaded.
(setq org-latex-classes
      (cons ox-rapport--latex-class
            (assoc-delete-all ox-rapport-latex-class
                              org-latex-classes)))

(defun ox-rapport--value (key info)
  "Return exported string for KEY from INFO, or nil when empty."
  (let ((value (plist-get info key)))
    (when value
      (setq value (org-export-data value info))
      (setq value (string-trim value))
      (unless (string-empty-p value)
        value))))

(defun ox-rapport--raw-value (key info)
  "Return the unexported string for KEY from INFO, or nil when empty.

Use this for metadata whose value is not document text, such as a
filesystem path."
  (let ((value (plist-get info key)))
    (when value
      (setq value (string-trim value))
      (unless (string-empty-p value)
        value))))

(defun ox-rapport--tex-text (value)
  "Escape plain VALUE for use as LaTeX text."
  (when value
    (org-latex-plain-text value nil)))

(defun ox-rapport--split-names (value)
  "Split comma-separated VALUE into a list of trimmed names."
  (when value
    (seq-filter
     (lambda (name)
       (not (string-empty-p name)))
     (mapcar #'string-trim
             (split-string value ",")))))

(defun ox-rapport--minutes (time)
  "Convert TIME in HH:MM form to minutes after midnight."
  (when (and time
             (string-match
              "\\`\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)\\'"
              time))
    (+ (* 60
          (string-to-number
           (match-string 1 time)))
       (string-to-number
        (match-string 2 time)))))

(defun ox-rapport--duration (start end)
  "Return a human-readable duration between START and END."
  (let ((from (ox-rapport--minutes start))
        (to (ox-rapport--minutes end)))
    (when (and from to)
      ;; Permit a meeting to pass midnight.
      (when (< to from)
        (setq to (+ to (* 24 60))))
      (let* ((minutes (- to from))
             (hours (/ minutes 60))
             (remainder (% minutes 60)))
        (cond
         ((and (> hours 0)
               (> remainder 0))
          (format "%d h %02d min"
                  hours
                  remainder))
         ((> hours 0)
          (format "%d h" hours))
         (t
          (format "%d min" remainder)))))))

(defun ox-rapport--meeting-line (date start end)
  "Construct the displayed meeting DATE and START/END line."
  (cond
   ((and date start end)
    (format "%s, %s--%s"
            date start end))
   ((and date start)
    (format "%s, %s"
            date start))
   (date
    date)
   ((and start end)
    (format "%s--%s"
            start end))
   (start
    start)
   (end
    end)))

(defun ox-rapport--project-title (project)
  "Return PROJECT with `ox-rapport-project-suffix' when appropriate."
  (when project
    (let ((suffix ox-rapport-project-suffix))
      (if (or (string-empty-p suffix)
              (string-suffix-p suffix project t))
          project
        (concat project suffix)))))

(defun ox-rapport--logo (logo)
  "Return the LaTeX logo block for LOGO, or an empty string."
  (if (not logo)
      ""
    (let* ((expanded
            (expand-file-name logo))
           (path
            (replace-regexp-in-string
             "\\\\" "/" expanded t t)))
      (format
       (concat
        "\\begin{minipage}[b]{0.18\\textwidth}\n"
        "\\raggedleft\n"
        "\\includegraphics["
        "height=%s,"
        "width=\\linewidth,"
        "keepaspectratio"
        "]{\\detokenize{%s}}\n"
        "\\end{minipage}")
       ox-rapport-logo-height
       path))))

(defun ox-rapport--title-block
    (title location meeting-line project logo)
  "Build the report title block."
  (let ((details
         (delq nil
               (list location
                     meeting-line
                     (ox-rapport--project-title
                      project)))))
    (concat
     "\\noindent\n"
     "\\begin{minipage}[b]{0.76\\textwidth}\n"
     (format
      "\\raggedright\n"
      "\\fontsize{23}{27}\\selectfont"
      "\\textbf{%s}\\par\n"
      (or (ox-rapport--tex-text title) ""))
     (when details
       (concat
        "\\vspace{0.45em}\n"
        "\\large\n"
        (mapconcat
         (lambda (line)
           (format "%s\\par"
                   (ox-rapport--tex-text line)))
         details
         "\n")))
     "\\end{minipage}\n"
     (if logo
         (concat
          "\\hfill\n"
          (ox-rapport--logo logo)
          "\n")
       "\n")
     "\\rapportseparator\n")))

(defun ox-rapport--roles-block (author initiated-by)
  "Build the optional AUTHOR and INITIATED-BY block."
  (let (cells)
    (when initiated-by
      (push
       (format
        (concat
         "\\begin{minipage}[t]{0.47\\textwidth}\n"
         "\\rapportlabel{Initiated By}\\\\[-0.1em]\n"
         "%s\n"
         "\\end{minipage}")
        (ox-rapport--tex-text initiated-by))
       cells))
    (when author
      (push
       (format
        (concat
         "\\begin{minipage}[t]{0.47\\textwidth}\n"
         "\\rapportlabel{Written By}\\\\[-0.1em]\n"
         "%s\n"
         "\\end{minipage}")
        (ox-rapport--tex-text author))
       cells))
    (when cells
      (concat
       "\\noindent\n"
       (mapconcat
        #'identity
        (nreverse cells)
        "\\hfill\n")
       "\n"
       "\\rapportseparator\n"))))

(defun ox-rapport--participant-rows (names column)
  "Build participant rows for NAMES, marking COLUMN.

COLUMN is one of `present', `absent', or `excused'."
  (mapconcat
   (lambda (name)
     (format
      "%s & %s & %s & %s \\\\\n"
      (ox-rapport--tex-text name)
      (if (eq column 'present)
          "$\\bullet$"
        "")
      (if (eq column 'absent)
          "$\\bullet$"
        "")
      (if (eq column 'excused)
          "$\\bullet$"
        "")))
   names
   ""))

(defun ox-rapport--participants-block
    (present absent excused)
  "Build attendance table, omitting it when all lists are empty."
  (let ((present-list
         (ox-rapport--split-names present))
        (absent-list
         (ox-rapport--split-names absent))
        (excused-list
         (ox-rapport--split-names excused)))
    (when (or present-list
              absent-list
              excused-list)
      (concat
       "\\rapportlabel{Participants}\\par\n"
       "\\vspace{0.2em}\n"
       "\\rowcolors{2}{}{rapportlightgray}\n"
       "\\renewcommand{\\arraystretch}{1.05}\n"
       "\\begin{tabularx}{\\textwidth}{@{}Xccc@{}}\n"
       "\\textbf{Name}"
       " & \\begin{sideways}\\small\\textbf{Present}\\end{sideways}"
       " & \\begin{sideways}\\small\\textbf{Absent}\\end{sideways}"
       " & \\begin{sideways}\\small\\textbf{Excused}\\end{sideways}"
       " \\\\\n"
       (ox-rapport--participant-rows
        present-list
        'present)
       (ox-rapport--participant-rows
        absent-list
        'absent)
       (ox-rapport--participant-rows
        excused-list
        'excused)
       "\\end{tabularx}\n"
       "\\renewcommand{\\arraystretch}{1}\n"
       "\\rowcolors{1}{}{}\n"
       "\\rapportseparator\n"))))

(defun ox-rapport--metadata-strip (date duration)
  "Build optional DATE and DURATION strip."
  (let (cells)
    (when date
      (push
       (format
        (concat
         "\\begin{minipage}[t]{0.47\\textwidth}\n"
         "\\rapportlabel{Date}\\\\[-0.1em]\n"
         "%s\n"
         "\\end{minipage}")
        (ox-rapport--tex-text date))
       cells))
    (when duration
      (push
       (format
        (concat
         "\\begin{minipage}[t]{0.47\\textwidth}\n"
         "\\rapportlabel{Duration}\\\\[-0.1em]\n"
         "%s\n"
         "\\end{minipage}")
        (ox-rapport--tex-text duration))
       cells))
    (when (and ox-rapport-show-date-strip
               cells)
      (concat
       "\\noindent\n"
       (mapconcat
        #'identity
        (nreverse cells)
        "\\hfill\n")
       "\n"
       "\\rapportseparator\n"))))

(defun ox-rapport--footer (project title)
  "Build footer configuration for PROJECT and TITLE."
  (let ((identity
         (or project title "")))
    (format
     (concat
      "\\fancyfoot[L]{%%\n"
      "  \\sffamily\\small\\color{rapportgray}%s}\n"
      "\\fancyfoot[R]{%%\n"
      "  \\sffamily\\small\\color{rapportgray}"
      "\\thepage\\ of \\pageref{LastPage}}\n")
     (ox-rapport--tex-text identity))))

(defun ox-rapport-template (contents info)
  "Return complete LaTeX document string for CONTENTS and INFO."
  (let* ((title
          (ox-rapport--value :title info))
         (author
          (ox-rapport--value :author info))
         (date
          (ox-rapport--value :date info))
         (location
          (ox-rapport--value :location info))
         (start
          (ox-rapport--value :start info))
         (end
          (ox-rapport--value :end info))
         (project
          (ox-rapport--value :project info))
         (present
          (ox-rapport--value :present info))
         (absent
          (ox-rapport--value :absent info))
         (excused
          (ox-rapport--value :excused info))
         (initiated-by
          (ox-rapport--value :initiated-by info))
         (logo
          (ox-rapport--raw-value :logo info))
         (duration
          (or (ox-rapport--value
               :duration info)
              (ox-rapport--duration
               start end)))
         (meeting-line
          (ox-rapport--meeting-line
           date start end))
         (frontmatter
          (concat
           (ox-rapport--footer project title)
           "\\begin{minipage}{\\textwidth}\n"
           (ox-rapport--title-block
            title
            location
            meeting-line
            project
            logo)
           (or
            (ox-rapport--roles-block
             author
             initiated-by)
            "")
           (or
            (ox-rapport--participants-block
             present
             absent
             excused)
            "")
           (or
            (ox-rapport--metadata-strip
             date
             duration)
            "")
           "\\end{minipage}\n"
           "\\vspace{0.75em}\n")))
    ;; Let the parent LaTeX backend create the document, but suppress
    ;; its ordinary title command because Rapport supplies its own
    ;; front matter.
    (setq info
          (copy-sequence info))
    (plist-put info
               :latex-class
               ox-rapport-latex-class)
    (plist-put info
               :with-title
               nil)
    (org-latex-template
     (concat frontmatter contents)
     info)))

;;;###autoload
(org-export-define-derived-backend
    'rapport
    'latex
  :menu-entry
  '(?R "Export with Rapport"
       ((?L "As LaTeX file"
            ox-rapport-export-to-latex)
        (?P "As PDF file"
            ox-rapport-export-to-pdf)
        (?O "As PDF file and open"
            ox-rapport-export-to-pdf-and-open)))
  :options-alist
  '((:location "LOCATION" nil nil t)
    (:start "START" nil nil t)
    (:end "END" nil nil t)
    (:project "PROJECT" nil nil t)
    (:present "PRESENT" nil nil t)
    (:absent "ABSENT" nil nil t)
    (:excused "EXCUSED" nil nil t)
    (:initiated-by "INITIATED_BY" nil nil t)
    (:duration "DURATION" nil nil t)
    (:logo "LOGO" nil nil t))
  :translate-alist
  '((template . ox-rapport-template)))

;;;###autoload
(defun ox-rapport-export-to-latex
    (&optional
     async
     subtreep
     visible-only
     body-only
     ext-plist)
  "Export the current Org buffer to a Rapport LaTeX file."
  (interactive)
  (org-export-to-file
      'rapport
      (org-export-output-file-name
       ".tex"
       subtreep)
    async
    subtreep
    visible-only
    body-only
    ext-plist))

;;;###autoload
(defun ox-rapport-export-to-pdf
    (&optional
     async
     subtreep
     visible-only
     body-only
     ext-plist)
  "Export the current Org buffer to a Rapport PDF file."
  (interactive)
  (let ((outfile
         (org-export-output-file-name
          ".tex"
          subtreep)))
    (org-export-to-file
        'rapport
        outfile
      async
      subtreep
      visible-only
      body-only
      ext-plist
      (lambda (file)
        (org-latex-compile file)))))

;;;###autoload
(defun ox-rapport-export-to-pdf-and-open
    (&optional
     async
     subtreep
     visible-only
     body-only
     ext-plist)
  "Export the current Org buffer to a Rapport PDF and open it."
  (interactive)
  (if async
      (ox-rapport-export-to-pdf
       async
       subtreep
       visible-only
       body-only
       ext-plist)
    (org-open-file
     (ox-rapport-export-to-pdf
      nil
      subtreep
      visible-only
      body-only
      ext-plist))))

(provide 'ox-rapport)

;;; ox-rapport.el ends here
