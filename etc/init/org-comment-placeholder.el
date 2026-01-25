;;; org-comment-placeholder.el --- Auto-hide comment blocks with a preview placeholder -*- lexical-binding: t; -*-

;;; commentary:

;;; code:
(require 'cl-lib)

(defgroup org-comment-placeholder nil
  "Auto-hide Org comment blocks and show a preview placeholder."
  :group 'org)

(defcustom org-comment-placeholder-prefix "· comment ·"
  "Text shown at the start of the placeholder."
  :type 'string)

(defcustom org-comment-placeholder-max-preview 80
  "Maximum number of characters to show from the first non-empty line in the comment block."
  :type 'integer)

(defcustom org-comment-placeholder-ellipsis "…"
  "Ellipsis appended when the preview is truncated."
  :type 'string)

(defface org-comment-placeholder-display-face
  '((t (:inherit shadow
        :family "EB Garamond"
        :weight regular
        :height 0.85)))
  "Face used to display the comment placeholder and preview text."
  :group 'org-comment-placeholder)

(defcustom org-comment-placeholder-face 'org-comment-placeholder-display-face
  "Face used for the placeholder label and preview."
  :type 'face)

(defconst org-comment-placeholder--invis-symbol 'org-comment-placeholder-mode)

(defvar-local org-comment-placeholder--overlays nil)
(defvar-local org-comment-placeholder--invis-enabled nil)

(defun org-comment-placeholder--enable-invisibility ()
  "Enable invisibility cookie for this buffer."
  (unless org-comment-placeholder--invis-enabled
    ;; IMPORTANT: invisibility spec must be (SYMBOL . t), not (SYMBOL t)
    (add-to-invisibility-spec (cons org-comment-placeholder--invis-symbol t))
    (setq org-comment-placeholder--invis-enabled t)))

(defun org-comment-placeholder--disable-invisibility ()
  "Disable invisibility cookie for this buffer."
  (when org-comment-placeholder--invis-enabled
    (remove-from-invisibility-spec (cons org-comment-placeholder--invis-symbol t))
    (setq org-comment-placeholder--invis-enabled nil)))

(defun org-comment-placeholder--hide-end-line ()
  "Hide the #+end_comment line at point, including its newline."
  (org-comment-placeholder--enable-invisibility)
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (to  (min (1+ eol) (point-max)))  ; include newline so the line truly vanishes
         (ov  (make-overlay bol to nil t t)))
    (overlay-put ov 'org-comment-placeholder t)
    (overlay-put ov 'org-comment-placeholder-end t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'invisible org-comment-placeholder--invis-symbol)
    (push ov org-comment-placeholder--overlays)))

(defun org-comment-placeholder--unhide-end-line-at (pos)
  "Remove any hidden-end overlay on the #+end_comment line at POS."
  (dolist (ov (overlays-at pos))
    (when (overlay-get ov 'org-comment-placeholder-end)
      (setq org-comment-placeholder--overlays (delq ov org-comment-placeholder--overlays))
      (delete-overlay ov))))

(defun org-comment-placeholder--clear ()
  "Remove all comment placeholder overlays in the current buffer."
  (when org-comment-placeholder--overlays
    (mapc #'delete-overlay org-comment-placeholder--overlays)
    (setq org-comment-placeholder--overlays nil)))

(defun org-comment-placeholder--delete-at-line ()
  "Delete any comment placeholder overlay on the current line (begin-line overlay)."
  (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
    (when (and (overlay-get ov 'org-comment-placeholder)
               (not (overlay-get ov 'org-comment-placeholder-end)))
      (setq org-comment-placeholder--overlays (delq ov org-comment-placeholder--overlays))
      (delete-overlay ov))))

(defun org-comment-placeholder--on-begin-line-p ()
  "Return non-nil if point is on a #+begin_comment line (case-insensitive)."
  (save-excursion
    (beginning-of-line)
    (let ((case-fold-search t))
      (looking-at-p "^[ \t]*#\\+begin_comment\\b"))))

(defun org-comment-placeholder--bounds-at-point ()
  "Return (BOL EOL CONTENT-BEG CONTENT-END) for the comment block at point.
Assumes point is on the #+begin_comment line."
  (save-excursion
    (beginning-of-line)
    (let* ((case-fold-search t)
           (bol (line-beginning-position))
           (eol (line-end-position))
           (content-beg (line-beginning-position 2)))
      (unless (re-search-forward "^[ \t]*#\\+end_comment\\b" nil t)
        (user-error "No matching #+end_comment found"))
      (let ((content-end (line-beginning-position)))
        (list bol eol content-beg content-end)))))

(defun org-comment-placeholder--preview-from (beg end)
  "Return a one-line preview from the first non-empty line between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((preview nil))
      (while (and (< (point) end) (not preview))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (trim (replace-regexp-in-string
                      "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" line)))
          (when (and trim (not (string-empty-p trim)))
            (setq preview (replace-regexp-in-string "[ \t]+" " " trim))))
        (forward-line 1))
      (setq preview (or preview ""))
      (if (and (integerp org-comment-placeholder-max-preview)
               (> (length preview) org-comment-placeholder-max-preview))
          (concat (substring preview 0 org-comment-placeholder-max-preview)
                  org-comment-placeholder-ellipsis)
        preview))))

(defun org-comment-placeholder--line-count (beg end)
  "Return number of physical lines between BEG and END."
  (max 0 (count-lines beg end)))

(defun org-comment-placeholder--make (bol eol content-beg content-end)
"Create a placeholder overlay on the begin_comment line as a standalone header.
Header line shows label + line count. Preview is on its own line, indented by
4 fixed-pitch columns (display-only)."
  (let* ((preview (org-comment-placeholder--preview-from content-beg content-end))
         (nlines  (org-comment-placeholder--line-count content-beg content-end))
         (header  (format "%s - %d -" org-comment-placeholder-prefix nlines))
         (hface   org-comment-placeholder-face)
         (pface   org-comment-placeholder-face)
         ;; Exactly 4 monospace columns, regardless of preview font:
         (indent4 (propertize "   " 'face 'fixed-pitch))
         (map (let ((m (make-sparse-keymap)))
                (define-key m (kbd "TAB") #'org-comment-placeholder-toggle)
                (define-key m (kbd "<tab>") #'org-comment-placeholder-toggle)
                (define-key m (kbd "RET") #'org-comment-placeholder-toggle)
                m))
         (ov (make-overlay bol eol nil t t)))
    (overlay-put ov 'org-comment-placeholder t)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'keymap map)
    ;; Avoid inheriting any indentation prefixes from elsewhere.
    (overlay-put ov 'line-prefix "")
    (overlay-put ov 'wrap-prefix "")
    ;; Two-line display: header flush-left, preview on its own line with fixed-pitch indent.
    (overlay-put ov 'display
                 (concat
                  (propertize header 'face hface)
                  "\n"
                  (when (and preview (not (string-empty-p preview)))
                    (concat
                     indent4
                     (propertize preview 'face pface)
                     "\n"))))
    (overlay-put ov 'cursor t)
    (push ov org-comment-placeholder--overlays)))

(defun org-comment-placeholder-hide-all ()
  "Hide all #+begin_comment blocks, replace the begin line with a placeholder,
and hide the #+end_comment line (including its newline)."
  (interactive)
  (org-comment-placeholder--clear)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_comment\\b" nil t)
        (let* ((beg-bol (line-beginning-position))
               (beg-eol (line-end-position))
               (content-beg (line-beginning-position 2)))
          (when (re-search-forward "^[ \t]*#\\+end_comment\\b" nil t)
            (let* ((end-bol (line-beginning-position))
                   (content-end end-bol))
              ;; Hide END line itself (incl newline) so it doesn't leave a blank line.
              (save-excursion
                (goto-char end-bol)
                (org-comment-placeholder--hide-end-line))
              ;; Hide block body (Org folding).
              (goto-char beg-bol)
              (org-hide-block-toggle t)
              ;; Replace begin line display with placeholder.
              (org-comment-placeholder--make beg-bol beg-eol content-beg content-end)
              (forward-line 1))))))))

(defun org-comment-placeholder-toggle ()
  "Toggle the comment block at point.

- If placeholder is visible: show block, remove placeholder overlay, unhide END line.
- If placeholder not visible: hide block, hide END line, add placeholder overlay."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (unless (org-comment-placeholder--on-begin-line-p)
    (user-error "Put point on a #+begin_comment line"))

  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (has-placeholder
          (cl-some (lambda (ov) (and (overlay-get ov 'org-comment-placeholder)
                                     (not (overlay-get ov 'org-comment-placeholder-end))))
                   (overlays-in bol eol))))
    (if has-placeholder
        ;; SHOW
        (progn
          (org-hide-block-toggle) ; hidden -> shown
          (org-comment-placeholder--delete-at-line)
          (save-excursion
            (beginning-of-line)
            (let ((case-fold-search t))
              (when (re-search-forward "^[ \t]*#\\+end_comment\\b" nil t)
                (org-comment-placeholder--unhide-end-line-at
                 (line-beginning-position))))))
      ;; HIDE
      (let* ((b (org-comment-placeholder--bounds-at-point))
             (bol (nth 0 b)) (eol (nth 1 b))
             (content-beg (nth 2 b)) (content-end (nth 3 b)))
        (org-hide-block-toggle t)
        (save-excursion
          (beginning-of-line)
          (let ((case-fold-search t))
            (when (re-search-forward "^[ \t]*#\\+end_comment\\b" nil t)
              (goto-char (line-beginning-position))
              (org-comment-placeholder--hide-end-line))))
        (org-comment-placeholder--make bol eol content-beg content-end)))))

(defun org-comment-placeholder-tab ()
  "If on a #+begin_comment line, toggle placeholder; otherwise run `org-cycle`."
  (interactive)
  (if (org-comment-placeholder--on-begin-line-p)
      (org-comment-placeholder-toggle)
    (call-interactively #'org-cycle)))

(defvar org-comment-placeholder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-cycle] #'org-comment-placeholder-tab)
    (define-key map (kbd "TAB") #'org-comment-placeholder-tab)
    (define-key map (kbd "<tab>") #'org-comment-placeholder-tab)
    map)
  "Keymap for `org-comment-placeholder-mode`.")

;;;###autoload
(define-minor-mode org-comment-placeholder-mode
  "Automatically hide Org comment blocks and show a placeholder + preview."
  :lighter " CmtHide"
  (if org-comment-placeholder-mode
      (progn
        (if (derived-mode-p 'org-mode)
            (org-comment-placeholder-hide-all)
          (add-hook 'org-mode-hook #'org-comment-placeholder-hide-all nil t))
        (add-hook 'after-save-hook #'org-comment-placeholder-hide-all nil t))
    (remove-hook 'org-mode-hook #'org-comment-placeholder-hide-all t)
    (remove-hook 'after-save-hook #'org-comment-placeholder-hide-all t)
    (org-comment-placeholder--clear)
    (org-comment-placeholder--disable-invisibility)))

(provide 'org-comment-placeholder)
;;; org-comment-placeholder.el ends here
