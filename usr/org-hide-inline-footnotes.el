;;; org-hide-inline-footnotes.el --- Inline Org footnotes folding to [*], including nested ones.
;;; commentary:

;;; code:
(require 'org-element)

(defface org-inline-footnote-placeholder-face
  '((t :inherit shadow :underline nil))
  "Face for the collapsed inline-footnote placeholder [*].")

(defvar org-inline-footnote-overlay-category 'org-inline-footnote
  "Overlay category for hiding inline Org footnote text.")

(defun org-inline-footnote--clear-overlays ()
  "Remove all inline footnote overlays in the current buffer."
  (remove-overlays (point-min) (point-max)
                   'category org-inline-footnote-overlay-category))

(defun org-inline-footnote--scan-buffer ()
  "Scan buffer and add overlays to inline Org footnotes.

This uses `org-element-parse-buffer` so it can handle nested
footnotes like [fn::outer [fn::inner] more]."
  (org-inline-footnote--clear-overlays)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let ((ast (org-element-parse-buffer 'object)))
          (org-element-map ast 'footnote-reference
            (lambda (ref)
              (let ((type (org-element-property :type ref)))
                (when (memq type '(inline anonymous))
		  (let* ((beg  (org-element-property :begin ref))
			 (end  (org-element-property :end ref))
			 (post (or (org-element-property :post-blank ref) 0))
			 (end0 (max beg (- end post)))
			 (ov   (make-overlay beg end0)))
		    (overlay-put ov 'category org-inline-footnote-overlay-category)
		    (overlay-put ov 'evaporate t)
		    (overlay-put ov 'priority 1000) ;; make sure it wins
		    (overlay-put ov 'my-footnote-beg beg)
		    (overlay-put ov 'my-footnote-end end0)

		    ;; FORCE the placeholder face (prevents underline from underlying org faces)
		    (overlay-put ov 'face 'org-inline-footnote-placeholder-face)
		    (overlay-put ov 'font-lock-face 'org-inline-footnote-placeholder-face)

		    ;; Collapsed display
		    (overlay-put ov 'display "[*]")) )))))))))

(defun org-inline-footnote--update-visibility ()
  "Show or hide inline footnotes depending on point position."
  (when (derived-mode-p 'org-mode)
    (let ((pos (point)))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get ov 'category)
                  org-inline-footnote-overlay-category)
          (let* ((beg (overlay-get ov 'my-footnote-beg))
                 (end (overlay-get ov 'my-footnote-end))
                 (inside (and (number-or-marker-p beg)
                              (number-or-marker-p end)
                              (>= pos beg)
                              (<= pos end))))
            (if inside
                ;; Inside [fn:...]: show full text
                (overlay-put ov 'display nil)
              ;; Outside: show placeholder
              (overlay-put ov 'display "[*]"))))))))

(defun org-inline-footnote--after-change (_beg _end _len)
  "Rescan inline footnotes after changes in Org buffers."
  (when (and org-hide-inline-footnotes-mode
             (derived-mode-p 'org-mode))
    (org-inline-footnote--scan-buffer)
    (org-inline-footnote--update-visibility)))

;;;###autoload
(define-minor-mode org-hide-inline-footnotes-mode
  "Hide inline Org footnotes and show them as [*] until you edit them.

Example:
  \"And thus the thing is interesting [fn::really interesting].\"
is shown as:
  \"And thus the thing is interesting [*].\"

Move the cursor onto [*] (i.e., into the [fn::...] region) to expand it.

Nested inline footnotes like:
  [fn::outer [fn::inner] more]
will each get their own overlay, so both outer and inner can expand
when the cursor enters their respective regions."
  :init-value nil
  :lighter " FN[*]"
  (if org-hide-inline-footnotes-mode
      (progn
        (org-inline-footnote--scan-buffer)
        (add-hook 'after-change-functions
                  #'org-inline-footnote--after-change nil t)
        (add-hook 'post-command-hook
                  #'org-inline-footnote--update-visibility nil t))
    (remove-hook 'after-change-functions
                 #'org-inline-footnote--after-change t)
    (remove-hook 'post-command-hook
                 #'org-inline-footnote--update-visibility t)
    (org-inline-footnote--clear-overlays)))

;; Optional: turn it on automatically in Org buffers
;(add-hook 'org-mode-hook #'org-hide-inline-footnotes-mode)

(provide 'org-hide-inline-footnotes)

;;; org-hide-inline-footnotes.el ends here.
