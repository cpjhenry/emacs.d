;;; org-subauthor.el --- SUBAUTHOR support for Org export -*- lexical-binding: t; -*-

;;; Commentary:

;; Add support for a #+SUBAUTHOR keyword to Org export.
;;
;; The subauthor is rendered beneath the author in both LaTeX and
;; OpenDocument exports.

;;; Code:

(require 'ox)

(add-to-list 'org-export-options-alist
             '(:subauthor "SUBAUTHOR" nil nil parse))

(defun org-subauthor--export (info backend)
  "Return INFO's SUBAUTHOR transcoded for BACKEND."
  (when-let* ((subauthor (plist-get info :subauthor)))
    (org-export-data-with-backend subauthor backend info)))

(defun org-subauthor--latex-filter (output backend info)
  "Insert SUBAUTHOR after AUTHOR in LaTeX OUTPUT.
BACKEND is the export backend and INFO is the export plist."
  (if (not (org-export-derived-backend-p backend 'latex))
      output
    (if-let* ((subauthor (org-subauthor--export info 'latex))
              (author (org-export-data
                       (plist-get info :author) info))
              (old (format "\\author{%s}" author)))
        (replace-regexp-in-string
         (regexp-quote old)
         (format "\\author{%s\\\\\n%s}" author subauthor)
         output t t)
      output)))

(defun org-subauthor--odt-filter (output backend info)
  "Insert SUBAUTHOR after AUTHOR in OpenDocument OUTPUT.
BACKEND is the export backend and INFO is the export plist."
  (if (not (org-export-derived-backend-p backend 'odt))
      output
    (if-let* ((subauthor (org-subauthor--export info 'odt))
              (author (org-export-data
                       (plist-get info :author) info))
              (old
               (format
                "<text:p text:style-name=\"OrgSubtitle\"><text:initial-creator>%s</text:initial-creator></text:p>"
                author)))
        (replace-regexp-in-string
         (regexp-quote old)
         (concat
          old
          "\n<text:p text:style-name=\"OrgSubtitle\">"
          subauthor
          "</text:p>")
         output t t)
      output)))

(add-hook 'org-export-filter-final-output-functions
          #'org-subauthor--latex-filter)

(add-hook 'org-export-filter-final-output-functions
          #'org-subauthor--odt-filter)

(provide 'org-subauthor)

;;; org-subauthor.el ends here
