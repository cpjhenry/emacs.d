;;; org-quote-indent.el --- Visual indent + pretty quote/verse delimiters -*- lexical-binding: t; -*-

;;; commentary:

;;; code:
(require 'cl-lib)

(defgroup org-quote-indent nil
  "Visually indent Org quote/verse blocks and prettify their delimiters."
  :group 'org)

(defcustom org-quote-indent-prefix "    "
  "Prefix used to visually indent lines inside quote/verse blocks."
  :type 'string)

(defcustom org-quote-indent-begin-placeholders
  '((quote . "· quote ·")
    (verse . "· verse ·"))
  ;; alternatives:
  ;; "⟨quote⟩"
  ;; "— quote —"
  ;; "· quote ·"
  "Alist mapping block symbol -> placeholder text for the begin line."
  :type '(alist :key-type symbol :value-type string))

(defcustom org-quote-indent-placeholder-face 'org-quote-indent-placeholder-face
  "Face for placeholder begin-line text."
  :type 'face)

(defface org-quote-indent-placeholder-face
  '((t (:inherit shadow
	:family "EB Garamond"
	:weight regular
        :height 0.85)))
  "Face for quote/verse placeholder labels like ⟦quote⟧."
  :group 'org-quote-indent)

;; Use a unique invisibility cookie so we don't interfere with other modes.
(defconst org-quote-indent--invis-symbol 'org-quote-indent-mode)

(defvar-local org-quote-indent--overlays nil)
(defvar-local org-quote-indent--invis-enabled nil)

(defun org-quote-indent--clear-buffer ()
  "Remove all overlays created by this mode."
  (when org-quote-indent--overlays
    (mapc #'delete-overlay org-quote-indent--overlays)
    (setq org-quote-indent--overlays nil)))

(defun org-quote-indent--enable-invisibility ()
  "Enable our invisibility symbol in this buffer."
  (unless org-quote-indent--invis-enabled
    (add-to-invisibility-spec (list org-quote-indent--invis-symbol t))
    (setq org-quote-indent--invis-enabled t)))

(defun org-quote-indent--disable-invisibility ()
  "Disable our invisibility symbol in this buffer."
  (when org-quote-indent--invis-enabled
    (remove-from-invisibility-spec (list org-quote-indent--invis-symbol t))
    (setq org-quote-indent--invis-enabled nil)))

(defun org-quote-indent--hide-line (bol eol)
  "Overlay BOL..EOL and make it invisible."
  (org-quote-indent--enable-invisibility)
  (let ((ov (make-overlay bol eol nil t t)))
    (overlay-put ov 'org-quote-indent t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'invisible org-quote-indent--invis-symbol)
    (push ov org-quote-indent--overlays)))

(defun org-quote-indent--make-begin-placeholder (bol eol text)
  "Display the begin line as a standalone flush-left placeholder line.
Point can still land on the (hidden) BEGIN line."
  (let ((ov (make-overlay bol eol nil t t)))
    (overlay-put ov 'org-quote-indent t)
    (overlay-put ov 'evaporate t)
    ;; Replace the BEGIN line with a synthetic header line + newline.
    (overlay-put ov 'display
                 (concat
                  ;; Force flush-left start
                  (propertize "" 'display '(space :align-to 0))
                  ;; The label itself
                  (propertize text 'face org-quote-indent-placeholder-face)
                  "\n"))
    ;; Allow point to sit on this line naturally
    (overlay-put ov 'cursor t)
    (push ov org-quote-indent--overlays)))

(defun org-quote-indent--apply (_beg _end)
  "Indent and prettify quote/verse blocks in the current buffer (display only)."
  (org-quote-indent--clear-buffer)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_\\(quote\\|verse\\)\\b"))
      (while (re-search-forward re nil t)
        (let* ((block-name (downcase (match-string 1))) ; \"quote\" or \"verse\"
               (block (intern block-name))
               (ph (cdr (assq block org-quote-indent-begin-placeholders)))
               (beg-bol (line-beginning-position))
               (beg-eol (line-end-position))
               (beg-to  (min (1+ beg-eol) (point-max)))
               (content-beg (line-beginning-position 2)))
          (when (re-search-forward (concat "^[ \t]*#\\+end_" (regexp-quote block-name) "\\b") nil t)
            (let* ((end-bol (line-beginning-position))
                   (end-eol (line-end-position))
                   (end-to  (min (1+ end-eol) (point-max)))
                   (content-end end-bol))

              ;; Indent content (wrap-prefix via overlay works reliably with visual-line-mode)
              (when (< content-beg content-end)
                (let ((ov (make-overlay content-beg content-end nil t t)))
                  (overlay-put ov 'org-quote-indent t)
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'line-prefix org-quote-indent-prefix)
                  (overlay-put ov 'wrap-prefix org-quote-indent-prefix)
                  (push ov org-quote-indent--overlays)))

              ;; Begin line -> placeholder on its own line (flush left), begin line hidden
              (when (and ph (stringp ph))
                (org-quote-indent--make-begin-placeholder beg-bol beg-to ph))

              ;; End line hidden
              (org-quote-indent--hide-line end-bol end-to))))))))

;;;###autoload
(define-minor-mode org-quote-indent-mode
  "Visually indent Org quote/verse blocks and prettify delimiters (text unchanged)."
  :lighter " QIndent"
  (if org-quote-indent-mode
      (progn
        (jit-lock-register #'org-quote-indent--apply)
        (org-quote-indent--apply (point-min) (point-max)))
    (jit-lock-unregister #'org-quote-indent--apply)
    (org-quote-indent--clear-buffer)
    (org-quote-indent--disable-invisibility)))

(provide 'org-quote-indent)
;;; org-quote-indent.el ends here
