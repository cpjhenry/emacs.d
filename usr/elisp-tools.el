;;; elisp-tools.el --- Utilities for working with Emacs Lisp  -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for editing and evaluating Emacs Lisp source.
;;
;; Page-aware evaluation treats literal ^L characters as source-page
;; delimiters. Form-feeds intended as string data should be written
;; as \f.
;;
;; Files that should check for accidental literal form-feeds before
;; saving may use:
;;
;;   Local Variables:
;;   before-save-hook: (elisp-check-literal-form-feeds)
;;   End:
;;
;; Documentation-spacing cleanup normalizes repeated prose spaces in
;; Commentary sections and docstrings while preserving structural
;; spacing and indentation.

;;; Code:
(require 'cl-lib)

(defun elisp-eval-page--buffer-has-pages-p ()
  "Return non-nil if the current buffer contains form-feed page breaks."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\f" nil t))))

(defun elisp-eval-page ()
  "Evaluate the current Elisp page, bounded by form-feed characters."
  (save-excursion
    (save-restriction
      (narrow-to-page)
      (let ((beg (point-min))
            (end (point-max))
            (page-name
             (save-excursion
               (goto-char (point-min))
               (if (re-search-forward "^;;;+ +\\(.+\\)" nil t)
                   (match-string 1)
                 "unnamed"))))
        (eval-region beg end)
        (message "Evaluated page: %s" page-name)))))

(defun elisp-eval-page-region-or-buffer ()
  "Evaluate active region, current page, or whole Elisp buffer."
  (interactive)
  (if (and (not (use-region-p))
           (elisp-eval-page--buffer-has-pages-p))
      (elisp-eval-page)
    (call-interactively #'elisp-eval-region-or-buffer)))

(defun elisp-check-literal-form-feeds ()
  "Signal an error for literal form-feeds occurring inside strings."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (search-forward "\f" nil t)
        (let ((pos (1- (point))))
          (when (nth 3 (parse-partial-sexp (point-min) pos))
            (user-error
             "Literal ^L inside string at line %d; use \\f instead"
             (line-number-at-pos pos))))))))

(defun elisp--structural-comment-space-p (pos)
  "Return non-nil if whitespace at POS follows a comment prefix.

Whitespace is structural when everything preceding POS on the same
line consists only of horizontal whitespace and one or more semicolons."
  (save-excursion
    (goto-char pos)
    (string-match-p
     "\\`[ \t]*;+\\'"
     (buffer-substring-no-properties
      (line-beginning-position) pos))))

(defun elisp-normalize-doc-spacing ()
  "Normalize multiple spaces in Commentary and docstrings.

Replace runs of two or more spaces between non-whitespace characters
with a single space. Leading indentation and spacing immediately
following comment prefixes are treated as structural and left
unchanged.

Report replacements and excess spaces removed separately for the
Commentary section and docstrings."
  (interactive)
  (unless (derived-mode-p 'emacs-lisp-mode)
    (user-error "Not an Emacs Lisp buffer"))

  (font-lock-ensure)

  (let ((regexp "  +")
        (commentary-count 0)
        (commentary-spaces 0)
        (docstring-count 0)
        (docstring-spaces 0)
        commentary-beg
        commentary-end)

    ;; Locate the Commentary section.
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^;;; Commentary:[ \t]*$" nil t)
        (setq commentary-beg
              (copy-marker (line-beginning-position 2)))
        (when (re-search-forward "^;;; Code:[ \t]*$" nil t)
          (setq commentary-end
                (copy-marker (line-beginning-position) t))))

    ;; Normalize Commentary.
    (when (and commentary-beg commentary-end)
      (save-excursion
        (goto-char commentary-beg)
        (while (re-search-forward regexp commentary-end t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (when
                (and
                 ;; The run must be internal prose spacing.
                 (> beg (line-beginning-position))
                 (< end (line-end-position))
                 (not (memq (char-before beg) '(?\s ?\t)))
                 (not (memq (char-after end) '(?\s ?\t)))

                 ;; Preserve `;;   Example' and similar layout.
                 (not (elisp--structural-comment-space-p beg)))

              (let ((removed (1- (- end beg))))
                (replace-match " ")
                (cl-incf commentary-count)
                (cl-incf commentary-spaces removed)))))))

    ;; Normalize docstrings.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (face (get-text-property beg 'face)))
          (when
              (and
               ;; It must be fontified as a docstring.
               (if (listp face)
                   (memq 'font-lock-doc-face face)
                 (eq face 'font-lock-doc-face))

               ;; Commentary has already been handled.
               (not
                (and commentary-beg commentary-end
                     (<= commentary-beg beg)
                     (< beg commentary-end)))

               ;; Only internal prose spacing.
               (> beg (line-beginning-position))
               (< end (line-end-position))
               (not (memq (char-before beg) '(?\s ?\t)))
               (not (memq (char-after end) '(?\s ?\t))))

            (let ((removed (1- (- end beg))))
              (replace-match " ")
              (cl-incf docstring-count)
              (cl-incf docstring-spaces removed))))))

    ;; Release markers.
    (when commentary-beg
      (set-marker commentary-beg nil))
    (when commentary-end
      (set-marker commentary-end nil))

    ;; Report statistics.
    (let ((total-count (+ commentary-count docstring-count))
          (total-spaces (+ commentary-spaces docstring-spaces)))
      (message
       (concat
        "Normalized %d spacing run%s; removed %d excess space%s "
        "(Commentary: %d run%s/%d space%s; "
        "docstrings: %d run%s/%d space%s)")
       total-count
       (if (= total-count 1) "" "s")
       total-spaces
       (if (= total-spaces 1) "" "s")
       commentary-count
       (if (= commentary-count 1) "" "s")
       commentary-spaces
       (if (= commentary-spaces 1) "" "s")
       docstring-count
       (if (= docstring-count 1) "" "s")
       docstring-spaces
       (if (= docstring-spaces 1) "" "s"))))))

(provide 'elisp-tools)

;;; elisp-tools.el ends here

; LocalWords:  elisp
