;;; org-quote-indent.el --- Visual indent + styling for Org blocks -*- lexical-binding: t; -*-

;;; commentary:

;;; code:
(require 'cl-lib)

(defgroup org-quote-indent nil
  "Visually indent and style selected Org blocks."
  :group 'org)

;; Faces (separate so you can theme them differently if you want)
(defface org-quote-indent-quote-face
  '((t (:inherit org-block :extend t)))
  "Face applied to the contents of #+begin_quote blocks."
  :group 'org-quote-indent)

(defface org-quote-indent-verse-face
  '((t (:inherit org-block :extend t)))
  "Face applied to the contents of #+begin_verse blocks."
  :group 'org-quote-indent)

(defface org-quote-indent-comment-face
  '((t (:inherit org-block :extend t)))
  "Face applied to the contents of #+begin_comment blocks."
  :group 'org-quote-indent)

(defcustom org-quote-indent-prefix "    "
  "Prefix used to visually indent lines inside supported blocks."
  :type 'string)

(defcustom org-quote-indent-comment-prefix ""
  "Prefix used to visually indent lines inside #+begin_comment blocks.
Set to \"\" if you want comments to be only shaded/faded without indent."
  :type 'string)

(defcustom org-quote-indent-delimiter-style 'fade
  "How to display #+begin_... / #+end_... delimiter lines.
- nil   : leave as-is
- fade  : apply a faint face (default)
- hide  : make delimiters invisible"
  :type '(choice (const :tag "Leave as-is" nil)
                 (const :tag "Fade" fade)
                 (const :tag "Hide" hide)))

(defcustom org-quote-indent-delimiter-face 'shadow
  "Face used when `org-quote-indent-delimiter-style` is `fade`."
  :type 'face)

(defcustom org-quote-indent-blocks
  '((quote   . org-quote-indent-quote-face)
    (verse   . org-quote-indent-verse-face)
    (comment . org-quote-indent-comment-face))
  "Blocks to style/indent.
Alist of (BLOCK-TYPE . FACE). BLOCK-TYPE is a symbol like `quote`, `verse`, `comment`."
  :type '(alist :key-type symbol :value-type face))

(defvar-local org-quote-indent--overlays nil)
(defvar-local org-quote-indent--invis-enabled nil)

(defun org-quote-indent--clear-buffer ()
  "Remove indentation/styling overlays from the whole buffer."
  (when org-quote-indent--overlays
    (mapc #'delete-overlay org-quote-indent--overlays)
    (setq org-quote-indent--overlays nil)))

(defun org-quote-indent--enable-invisibility ()
  "Enable delimiter invisibility symbol in this buffer."
  (unless org-quote-indent--invis-enabled
    (add-to-invisibility-spec '(org-quote-indent . t))
    (setq org-quote-indent--invis-enabled t)))

(defun org-quote-indent--disable-invisibility ()
  "Disable delimiter invisibility symbol in this buffer."
  (when org-quote-indent--invis-enabled
    (remove-from-invisibility-spec '(org-quote-indent . t))
    (setq org-quote-indent--invis-enabled nil)))

(defun org-quote-indent--block-prefix (block)
  "Return prefix string for BLOCK symbol."
  (if (eq block 'comment)
      org-quote-indent-comment-prefix
    org-quote-indent-prefix))

(defun org-quote-indent--apply (_beg _end)
  "Indent and style configured Org blocks in the current buffer (display only).
Uses overlays so `visual-line-mode' wraps get indented reliably.
Case-insensitive for #+BEGIN_.../# +END_...."
  (org-quote-indent--clear-buffer)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      ;; Build regexp like: quote\\|verse\\|comment
      (let* ((blocks (mapcar (lambda (x) (symbol-name (car x))) org-quote-indent-blocks))
             (alt (regexp-opt blocks 'symbols))
             ;; `regexp-opt` with 'symbols adds \\_< \\_>, which isn't ideal here.
             ;; We'll do our own simple alternation.
             (alt (mapconcat #'identity blocks "\\|"))
             (re (concat "^[ \t]*#\\+begin_\\(" alt "\\)\\b")))
        (while (re-search-forward re nil t)
          (let* ((block-name (match-string 1))
                 (block (intern (downcase block-name)))
                 (face (cdr (assq block org-quote-indent-blocks)))
                 (prefix (org-quote-indent--block-prefix block))
                 (beg-line-bol (line-beginning-position))
                 (beg-line-eol (line-end-position))
                 (content-beg (line-beginning-position 2)))
            (when (re-search-forward (concat "^[ \t]*#\\+end_" (regexp-quote block-name) "\\b") nil t)
              (let* ((end-line-bol (line-beginning-position))
                     (end-line-eol (line-end-position))
                     (content-end end-line-bol))
                (when (< content-beg content-end)
                  ;; Content overlay: indent + background/face
                  (let ((ov (make-overlay content-beg content-end nil t t)))
                    (overlay-put ov 'org-quote-indent t)
                    (overlay-put ov 'evaporate t)
                    (when (and prefix (not (string-empty-p prefix)))
                      (overlay-put ov 'line-prefix prefix)
                      (overlay-put ov 'wrap-prefix prefix))
                    ;; (when face
                    ;;   (overlay-put ov 'face face))
                    (push ov org-quote-indent--overlays))

                  ;; Delimiter overlays
                  (pcase org-quote-indent-delimiter-style
                    ('fade
                     (dolist (range (list (cons beg-line-bol (min (1+ beg-line-eol) (point-max)))
                                          (cons end-line-bol (min (1+ end-line-eol) (point-max)))))
                       (let ((dov (make-overlay (car range) (cdr range) nil t t)))
                         (overlay-put dov 'org-quote-indent t)
                         (overlay-put dov 'evaporate t)
                         (overlay-put dov 'face org-quote-indent-delimiter-face)
                         (push dov org-quote-indent--overlays))))
                    ('hide
                     (org-quote-indent--enable-invisibility)
                     (dolist (range (list (cons beg-line-bol (min (1+ beg-line-eol) (point-max)))
                                          (cons end-line-bol (min (1+ end-line-eol) (point-max)))))
                       (let ((dov (make-overlay (car range) (cdr range) nil t t)))
                         (overlay-put dov 'org-quote-indent t)
                         (overlay-put dov 'evaporate t)
                         (overlay-put dov 'invisible 'org-quote-indent)
                         (push dov org-quote-indent--overlays))))
                    (_ nil)))))))))))

;;;###autoload
(define-minor-mode org-quote-indent-mode
  "Visually indent and style Org blocks (quote/verse/comment) without changing text."
  :lighter " QIndent"
  (if org-quote-indent-mode
      (progn
        (jit-lock-register #'org-quote-indent--apply)
        (org-quote-indent--apply (point-min) (point-max)))
    (jit-lock-unregister #'org-quote-indent--apply)
    (org-quote-indent--clear-buffer)
    (org-quote-indent--disable-invisibility)))

;(add-hook 'org-mode-hook #'org-quote-indent-mode)

(provide 'org-quote-indent)
;;; org-quote-indent.el ends here
