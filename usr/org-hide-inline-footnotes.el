;;; org-hide-inline-footnotes.el --- Inline Org footnotes folding  -*- lexical-binding: t; -*-

;;; Commentary:

;; Hide inline Org footnotes behind a small placeholder, while leaving the
;; underlying buffer text unchanged.
;;
;; For example:
;;
;;   And thus the thing is interesting [fn::really interesting].
;;
;; is displayed as:
;;
;;   And thus the thing is interesting [*].
;;
;; Moving point into the footnote reveals the original text.
;;
;; This package uses overlays only.  It does not rewrite buffer contents.
;;
;; A previous version reparsed the buffer directly from
;; `after-change-functions'.  That was unsafe: Org commands such as
;; `org-todo' may temporarily depend on match data while editing buffer
;; text.  This version merely marks the overlays stale from
;; `after-change-functions', then refreshes them from `post-command-hook',
;; after the original command has finished.

;;; Code:

(require 'org-element)

(defface org-inline-footnote-placeholder-face
  '((t :inherit shadow :underline nil))
  "Face for the collapsed inline-footnote placeholder [*].")

(defvar org-inline-footnote-overlay-category 'org-inline-footnote
  "Overlay category for hiding inline Org footnote text.")

(defvar-local org-inline-footnote--dirty nil
  "Non-nil means inline footnote overlays need refreshing.")

(defun org-inline-footnote--clear-overlays ()
  "Remove all inline footnote overlays in the current buffer."
  (remove-overlays (point-min) (point-max)
                   'category org-inline-footnote-overlay-category))

(defun org-inline-footnote--scan-buffer ()
  "Scan buffer and add overlays to inline footnotes.

This uses `org-element-parse-buffer' so it can handle nested
footnotes like [fn::outer [fn::inner] more]."
  (save-match-data
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
                    (let* ((beg (org-element-property :begin ref))
                           (end (org-element-property :end ref))
                           (post (or (org-element-property :post-blank ref) 0))
                           (end0 (max beg (- end post)))
                           (ov (make-overlay beg end0)))
                      (overlay-put ov 'category
                                   org-inline-footnote-overlay-category)
                      (overlay-put ov 'evaporate t)
                      (overlay-put ov 'priority 1000)
                      (overlay-put ov 'org-inline-footnote-beg beg)
                      (overlay-put ov 'org-inline-footnote-end end0)

                      ;; Force the placeholder face so underlying Org faces,
                      ;; especially links and footnotes, do not leak through.
                      (overlay-put ov 'face
                                   'org-inline-footnote-placeholder-face)
                      (overlay-put ov 'font-lock-face
                                   'org-inline-footnote-placeholder-face)

                      ;; Collapse the inline footnote visually.  The buffer
                      ;; text itself is left untouched.
                      (overlay-put ov 'display "[*]"))))))))))))

(defun org-inline-footnote--update-visibility ()
  "Show or hide inline footnotes depending on point position."
  (save-match-data
    (when (derived-mode-p 'org-mode)
      (let ((pos (point)))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (eq (overlay-get ov 'category)
                    org-inline-footnote-overlay-category)
            (let* ((beg (overlay-get ov 'org-inline-footnote-beg))
                   (end (overlay-get ov 'org-inline-footnote-end))
                   (inside (and (number-or-marker-p beg)
                                (number-or-marker-p end)
                                (>= pos beg)
                                (<= pos end))))
              (overlay-put ov 'display
                           (unless inside "[*]")))))))))

(defun org-inline-footnote--after-change (_beg _end _len)
  "Mark inline footnote overlays as needing refresh.

Do not rescan the buffer directly from `after-change-functions'.
Some Org commands, notably `org-todo', perform multi-stage edits and
may depend on match data while the change hook is running."
  (when (and org-hide-inline-footnotes-mode
             (derived-mode-p 'org-mode))
    (setq org-inline-footnote--dirty t)))

(defun org-inline-footnote--post-command ()
  "Refresh inline footnote overlays after commands."
  (when (and org-hide-inline-footnotes-mode
             (derived-mode-p 'org-mode))
    (save-match-data
      (when org-inline-footnote--dirty
        (setq org-inline-footnote--dirty nil)
        (org-inline-footnote--scan-buffer))
      (org-inline-footnote--update-visibility))))

;;;###autoload
(define-minor-mode org-hide-inline-footnotes-mode
  "Hide inline Org footnotes and show them as [*] until point enters them.

Example:
  \"And thus the thing is interesting [fn::really interesting].\"

is displayed as:
  \"And thus the thing is interesting [*].\"

Move point onto the collapsed footnote, that is, into the original
[fn::...] region, to reveal the full inline footnote.

Nested inline footnotes like:
  [fn::outer [fn::inner] more]

are handled by Org's parser, so both outer and inner footnotes can
receive overlays."
  :init-value nil
  :lighter " FN[*]"
  (if org-hide-inline-footnotes-mode
      (progn
        (setq org-inline-footnote--dirty nil)
        (org-inline-footnote--scan-buffer)
        (add-hook 'after-change-functions
                  #'org-inline-footnote--after-change nil t)
        (add-hook 'post-command-hook
                  #'org-inline-footnote--post-command nil t))
    (remove-hook 'after-change-functions
                 #'org-inline-footnote--after-change t)
    (remove-hook 'post-command-hook
                 #'org-inline-footnote--post-command t)
    (setq org-inline-footnote--dirty nil)
    (org-inline-footnote--clear-overlays)))

;; Optional: turn it on automatically in Org buffers.
;;
;;   (add-hook 'org-mode-hook #'org-hide-inline-footnotes-mode)

(provide 'org-hide-inline-footnotes)

;;; org-hide-inline-footnotes.el ends here
