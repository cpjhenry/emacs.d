;;; first-letter-only.el --- First-letter-only cypher buffers -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: cypher

;;; Commentary:

;; Generate first-letter-only mnemonic reductions from prose.
;;
;; This package creates a derived buffer containing a cue-text version
;; of the current buffer or active region.  Each word is reduced to its
;; initial letter, while selected punctuation, plural markers, paragraph
;; breaks, and Org-style typographical conventions are preserved or
;; normalized.
;;
;; The resulting text is not intended as a cryptographic cipher.  It is
;; a rehearsal aid: a compact mnemonic scaffold for material already
;; known through prior study or oral instruction.
;;
;; The original buffer is never modified.  Output is written to a
;; temporary display buffer named by `first-letter-only-buffer-name',
;; suitable for review, printing, or further export.

;;; Code:
(defconst first-letter-only--plural-marker "§PLURAL§")

(defgroup first-letter-only nil
  "Generate `first-letter-only' cypher text."
  :group 'text)

(defcustom first-letter-only-buffer-name "*First Letter Only*"
  "Name of the buffer used for `first-letter-only' cypher output."
  :type 'string
  :group 'first-letter-only)

(defcustom first-letter-only-org-translations
  '(("\\\\longdash{}" . "———")
    ("\\\\first{}" . "1st")
    ("\\\\second{}" . "2nd")
    ("\\\\third{}" . "3rd")
    ("---" . "—")
    ("--" . "–"))
  "Org/LaTeX-style strings translated before `first-letter-only' cypher conversion."
  :type '(alist :key-type regexp :value-type string)
  :group 'first-letter-only)

(defun first-letter-only--replace-pairs (text pairs)
  "Apply PAIRS of regexp replacements to TEXT."
  (dolist (pair pairs text)
    (setq text
          (replace-regexp-in-string
           (car pair) (cdr pair) text t))))

(defun first-letter-only--first-letters-line (line)
  "Reduce words in LINE to their first letters."
  (replace-regexp-in-string
   "\\([^[:space:]]\\)[^[:space:]]*"
   "\\1"
   line))

(defun first-letter-only--transform (text)
  "Return `first-letter-only' cypher version of TEXT."
  (let ((text text))

    ;; org translations first.
    (setq text
	  (first-letter-only--replace-pairs
	   text
	   first-letter-only-org-translations))

    ;; strip org directives.
    ;; NOTE: Should restrict Org directive stripping to Org-derived
    ;; buffers. (Take note, future self.)
    (setq text
	  (replace-regexp-in-string
	   "^#\\+.*\\(?:\n\\|\\'\\)"
	   ""
	   text))

    ;; Mask special sequences.
    (setq text
          (first-letter-only--replace-pairs
           text
           '(("\\.\\.\\.\\.\\." . "-----")
             ("-ry" . "- #"))))

    ;; Punctuation spacing.
    (setq text
          (first-letter-only--replace-pairs
           text
           '(("$" . " ")
             ("\\." . " . ")
             (", " . " , ")
             ("; " . " ; "))))

    ;; Whitespace and case.
    (setq text (replace-regexp-in-string "  +" " " text))
    (setq text (capitalize (downcase text)))

    ;; Small-word fixes.
    (setq text
          (first-letter-only--replace-pairs
           text
           '((" A " . " a ")
             (" An " . " an ")
             (" The " . " the ")
             (" At " . " at ")
             (" By " . " by ")
             (" For " . " for ")
             (" In " . " in ")
             (" Of " . " of ")
             (" On " . " on ")
             (" To " . " to ")
             (" Up " . " up ")
             (" And " . " and ")
             (" As " . " as ")
             (" But " . " but ")
             (" Or " . " or ")
             (" Nor " . " nor "))))

    ;; Plurals and Lodge marker.
    (setq text
          (first-letter-only--replace-pairs
           text
           '((" Is " . " I ")
             ("\\([[:alpha:]]\\)s\\([[:space:][:punct:]]\\)" . "\\1§PLURAL§\\2")
             ("s\\. " . " s . ")
             (" Lodge" . " ["))))

    ;; Transmogrification: reduce each token to its first character.
    (setq text
	  (mapconcat #'first-letter-only--first-letters-line
                     (split-string text "\n" nil)
                     "\n"))

    ;; Trim.
    (setq text (string-trim text))

    ;; Restore plural markers, Lodge marker, punctuation, dots, underlines.
    (setq text
          (first-letter-only--replace-pairs
           text
           '(("§PLURAL§" . "s")
             ("\\[" . "[]")
             (" \\. " . ". ")
             (" \\." . ".")
             (" , " . ", ")
             (" ; " . "; ")
             ("- #" . "-ry")
             ("-" . "-----")
             ("- -" . "--")
             ("-" . ".")
             ("_" . "_____"))))

    text))

(defun first-letter-only (&optional beg end)
  "Create a `first-letter-only' cypher buffer from region or buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (let* ((org-source-p (derived-mode-p 'org-mode))
         (text (buffer-substring-no-properties
                (or beg (point-min))
                (or end (point-max)))))
    (with-current-buffer (get-buffer-create first-letter-only-buffer-name)
      (erase-buffer)
      (insert (first-letter-only--transform text))

      (when org-source-p
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\*+ .+\\)\n\\([^ \n*]\\)" nil t)
          (replace-match "\\1\n\n\\2")))

      (goto-char (point-min))
      (text-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'first-letter-only)

;;; first-letter-only.el ends here

; LocalWords:  punct
