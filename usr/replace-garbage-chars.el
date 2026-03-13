;;; replace-garbage-chars.el --- Repair CP1252/HTML garbage text -*- lexical-binding: t; -*-

;; Author: cpj
;; Version: 0.1
;; Keywords: convenience, text
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Inspired by https://www.emacswiki.org/emacs/ReplaceGarbageChars

;; Replace common mojibake, CP1252 artefacts, and HTML entities
;; with plain ASCII equivalents.
;;
;; Main entry point:
;;
;;   M-x replace-garbage-chars
;;
;; Behaviour:
;; - operates on the active region if any, otherwise the whole buffer
;; - works in read-only buffers via `inhibit-read-only'
;; - reports the number of replacements made
;; - with prefix argument, suppresses the message
;;
;; The replacement table is defined once, and internal caches
;; (regexp and hash table) are derived automatically.

;;; Code:

(defmacro define-replacement-table (name table &optional doc)
  "Define TABLE and derived caches for NAME.

Creates:
- NAME-alist
- NAME--regexp
- NAME--table
- NAME--rebuild-cache"
  (let* ((alist   (intern (format "%s-alist" name)))
         (regexp  (intern (format "%s--regexp" name)))
         (htable  (intern (format "%s--table" name)))
         (rebuild (intern (format "%s--rebuild-cache" name))))
    `(progn
       (defvar ,alist ,table ,doc)

       (defvar ,regexp nil
         ,(format "Regexp matching entries in `%s'." alist))

       (defvar ,htable nil
         ,(format "Hash table cache derived from `%s'." alist))

       (defun ,rebuild ()
         ,(format "Rebuild internal caches for `%s'." name)
         (setq ,regexp
               (regexp-opt (mapcar #'car ,alist)))
         (setq ,htable
               (let ((table (make-hash-table :test #'equal)))
                 (dolist (pair ,alist table)
                   (puthash (car pair) (cdr pair) table))))))))

(define-replacement-table
 replace-garbage-chars
 '(("΄" . "'")
   ("‘" . "'")
   ("’" . "'")
   ("“" . "\"")
   ("”" . "\"")
   ("" . "'")
   ("" . "'")
   ("" . "\"")
   ("" . "\"")
   ("¡\"" . "\"")
   ("¡­" . "...")
   ("" . "...")
   ("" . " ")
   ("" . "`")
   ("" . "'")
   ("" . "``")
   ("" . "''")
   ("" . "*")
   ("" . "--")
   ("" . "--")
   ("¡" . "\"")
   ("•" . "-")
   ("–" . "--")   ; en dash
   ("—" . "---")  ; em dash
   ("―" . "---")  ; horizontal bar
   ("&#x27;" . "'")
   ("&#38;" . "&")
   ("&#39;" . "'")

   ;; Ellipsis normalization
   ("…" . "...")
   ("… " . "... ")
   (". . ." . "...")
   (". . . " . "... ")
   (" . . ." . "...")
   (" . . . " . "... ")
   ("&#8230;" . "...")
   ("&#x2026;" . "...")
   ("&hellip;" . "..."))
 "Alist of (FROM . TO) replacements for `replace-garbage-chars`.")

(defun replace-garbage-chars--ensure-cache ()
  "Ensure internal caches for `replace-garbage-chars` exist."
  (unless (and replace-garbage-chars--regexp
               replace-garbage-chars--table)
    (replace-garbage-chars--rebuild-cache)))

(defun replace-garbage-chars-rebuild-cache ()
  "Interactively rebuild caches used by `replace-garbage-chars`."
  (interactive)
  (replace-garbage-chars--rebuild-cache)
  (message "replace-garbage-chars cache rebuilt."))

;;;###autoload
(defun replace-garbage-chars (&optional beg end quiet)
  "Replace MS/CP1252 and other garbage characters with plain equivalents.

If the region is active, operate on BEG and END.
Otherwise, operate on the whole buffer.

When called interactively:
- no prefix argument: report replacement count
- with \\[universal-argument]: suppress the message

When called from Lisp:
- QUIET non-nil suppresses the message

Returns the number of replacements made."
  (interactive
   (list (and (region-active-p) (region-beginning))
         (and (region-active-p) (region-end))
         current-prefix-arg))
  (replace-garbage-chars--ensure-cache)
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (count 0)
         (suppress (and quiet t)))
    (atomic-change-group
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t))
            (narrow-to-region beg end)
            (goto-char (point-min))
            (while (re-search-forward replace-garbage-chars--regexp nil t)
              (let ((replacement
                     (gethash (match-string-no-properties 0)
                              replace-garbage-chars--table)))
                (when replacement
                  (replace-match replacement t t)
                  (setq count (1+ count)))))))))
    (unless suppress
      (message "Garbage in, garbage out. %d replacement%s made."
               count
               (if (= count 1) "" "s")))
    count))

;; Build caches at load time.
(replace-garbage-chars--rebuild-cache)

(provide 'replace-garbage-chars)

;;; replace-garbage-chars.el ends here
