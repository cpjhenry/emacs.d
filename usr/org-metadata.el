;;; org-metadata.el --- Extended metadata support for Org export -*- lexical-binding: t; -*-

;;; Commentary:

;; Add support for additional document metadata to Org export.
;;
;; Supported keywords:
;;
;;   #+SUBAUTHOR:
;;   #+AFFILIATION:
;;
;; SUBAUTHOR and AFFILIATION are rendered beneath the author in both
;; LaTeX and OpenDocument exports.

;;; Code:

(require 'ox)

(add-to-list 'org-export-options-alist
             '(:subauthor "SUBAUTHOR" nil nil parse))

(add-to-list 'org-export-options-alist
             '(:affiliation "AFFILIATION" nil nil parse))


(defun org-metadata--export (property info backend)
  "Return INFO's PROPERTY transcoded for BACKEND."
  (when-let* ((value (plist-get info property)))
    (org-export-data-with-backend value backend info)))


(defun org-metadata--latex-filter (output backend info)
  "Insert extended metadata after AUTHOR in LaTeX OUTPUT.
BACKEND is the export backend and INFO is the export plist."
  (if (not (org-export-derived-backend-p backend 'latex))
      output
    (if-let* ((author (org-export-data
                       (plist-get info :author) info))
              (old (format "\\author{%s}" author)))
        (let* ((subauthor
                (org-metadata--export :subauthor info 'latex))
               (affiliation
                (org-metadata--export :affiliation info 'latex))
               (metadata
                (delq nil (list subauthor affiliation))))
          (if metadata
              (replace-regexp-in-string
               (regexp-quote old)
               (format "\\author{%s\\\\\n%s}"
                       author
                       (mapconcat #'identity metadata "\\\\\n"))
               output t t)
            output))
      output)))


(defun org-metadata--odt-filter (output backend info)
  "Insert extended metadata after AUTHOR in OpenDocument OUTPUT.
BACKEND is the export backend and INFO is the export plist."
  (if (not (org-export-derived-backend-p backend 'odt))
      output
    (if-let* ((author (org-export-data
                       (plist-get info :author) info))
              (old
               (format
                "<text:p text:style-name=\"OrgSubtitle\"><text:initial-creator>%s</text:initial-creator></text:p>"
                author)))
        (let ((subauthor
               (org-metadata--export :subauthor info 'odt))
              (affiliation
               (org-metadata--export :affiliation info 'odt)))
          (concat
           (replace-regexp-in-string
            (regexp-quote old)
            (concat
             old
             (when subauthor
               (concat
                "\n<text:p text:style-name=\"OrgSubtitle\">"
                subauthor
                "</text:p>"))
             (when affiliation
               (concat
                "\n<text:p text:style-name=\"OrgSubtitle\">"
                affiliation
                "</text:p>")))
            output t t)))
      output)))


(add-hook 'org-export-filter-final-output-functions
          #'org-metadata--latex-filter)

(add-hook 'org-export-filter-final-output-functions
          #'org-metadata--odt-filter)


(provide 'org-metadata)

;;; org-metadata.el ends here
