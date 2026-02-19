;;; org-inline-footnote-mode.el --- Inline Org footnotes folding to [*].
;;; Commentary:
;;; Code:

(defvar org-inline-footnote-overlay-category 'org-inline-footnote
  "Overlay category for hiding inline Org footnote text.")

(defun org-inline-footnote--clear-overlays ()
  "Remove all inline footnote overlays in the current buffer."
  (remove-overlays (point-min) (point-max)
                   'category org-inline-footnote-overlay-category))

(defun org-inline-footnote--scan-buffer ()
  "Scan buffer and add overlays to inline Org footnotes.

Matches inline footnotes like:
  [fn:1: This is a note]
  [fn:: Anonymous note]"
  (org-inline-footnote--clear-overlays)
  (save-excursion
    (goto-char (point-min))
    ;; Group 1: optional name; Group 2: the text.
    (let ((re "\\[fn:\\([^]:]*\\):\\([^]]+\\)\\]"))
      (while (re-search-forward re nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (ov (make-overlay beg end)))
          (overlay-put ov 'category org-inline-footnote-overlay-category)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'my-footnote-beg beg)
          (overlay-put ov 'my-footnote-end end)
          ;; The collapsed display (what you see when not editing)
          (overlay-put ov 'display "[*]"))))))

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
                ;; Inside the [fn:...]: show full text
                (overlay-put ov 'display nil)
              ;; Outside: show placeholder
              (overlay-put ov 'display "[*]"))))))))

(defun org-inline-footnote--after-change (_beg _end _len)
  "Rescan inline footnotes after changes in Org buffers."
  (when (and org-inline-footnote-mode
             (derived-mode-p 'org-mode))
    (org-inline-footnote--scan-buffer)
    (org-inline-footnote--update-visibility)))

;;;###autoload
(define-minor-mode org-inline-footnote-mode
  "Hide inline Org footnotes and show them as [*] until you edit them.

Example:
  \"And thus the thing is interesting [fn::really interesting].\"
is shown as:
  \"And thus the thing is interesting [*].\"

Move the cursor onto [*] to expand it."
  :init-value nil
  :lighter " FN[*]"
  (if org-inline-footnote-mode
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
(add-hook 'org-mode-hook #'org-inline-footnote-mode)

(provide 'org-inline-footnote)

;;; org-inline-footnote-mode.el ends here.
