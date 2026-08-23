;;; org-prose.el --- Count semantic prose in Org documents -*- lexical-binding: t; -*-

;; Author: Paul J Henry
;; Keywords: outlines, text, convenience
;; Package-Requires: ((emacs "31.0") (org "9.7"))

;;; Commentary:

;; `org-prose' provides prose-oriented statistics for Org documents.
;;
;; Unlike ordinary word-counting commands, it attempts to count only the
;; text which constitutes the document itself.  Org structure and other
;; non-prose material are ignored, including headings, metadata, comments,
;; source blocks, tables, footnote definitions, and footnote references.
;;
;; Subtrees excluded from export through `org-export-exclude-tags' are
;; also excluded from the prose count.  Additional tags may be excluded
;; from prose counting without affecting export through
;; `org-prose-exclude-tags'.
;;
;; `org-prose-count' operates on the active region when one exists, on
;; the current subtree when called with a prefix argument, and otherwise
;; on the accessible portion of the buffer.
;;
;; The command returns a list of the form
;;
;;     (LINES SENTENCES WORDS CHARACTERS)
;;
;; so that other commands may use the statistics for purposes such as
;; estimating delivery time.

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)

(defgroup org-prose nil
  "Count semantic prose in Org documents."
  :group 'org)

(defcustom org-prose-exclude-tags '("optionally")
  "Org tags whose subtrees should not contribute to prose counts.

Unlike tags in `org-export-exclude-tags', these tags affect only prose
counting and do not prevent a subtree from being exported."
  :type '(repeat string)
  :group 'org-prose)

(defun org-prose--excluded-p (paragraph)
  "Return non-nil when PARAGRAPH should be excluded from prose counts."
  (or
   ;; Footnote definitions are supporting apparatus, not prose.
   (org-element-lineage paragraph '(footnote-definition))

   ;; Honour both Org's export exclusions and our own prose exclusions.
   (let ((headline (org-element-lineage paragraph '(headline) t)))
     (and headline
          (seq-intersection
           (org-element-property :tags headline)
           (append org-export-exclude-tags
                   org-prose-exclude-tags)
           #'string=)))))

(defun org-prose--paragraph-string (paragraph)
  "Return the text of PARAGRAPH with footnote references removed.

Return nil when PARAGRAPH should not contribute to the prose count."
  (unless (org-prose--excluded-p paragraph)
    (let* ((begin (org-element-property :contents-begin paragraph))
           (end   (org-element-property :contents-end paragraph))
           (text  (buffer-substring-no-properties begin end))
           ranges)
      (org-element-map paragraph 'footnote-reference
        (lambda (footnote)
          ;; A nested footnote disappears with its enclosing footnote.
          (unless (org-element-lineage footnote '(footnote-reference))
            (push (cons (- (org-element-property :begin footnote) begin)
                        (- (org-element-property :end footnote) begin))
                  ranges))))
      ;; Delete from the end so earlier positions remain valid.
      (dolist (range (sort ranges
                           (lambda (a b)
                             (> (car a) (car b)))))
        (setq text
              (concat (substring text 0 (car range))
                      (substring text (cdr range)))))
      text)))

;;;###autoload
;;;###autoload
(defun org-prose-count (&optional begin end scope)
  "Count lines, sentences, words, and characters of Org prose.

Exclude headings, metadata, structural elements, footnote definitions,
footnote references, export-excluded subtrees, and subtrees tagged with
one of `org-prose-exclude-tags'.

When the region is active, count prose within the region.

With a prefix argument, count prose within the current Org subtree.

Otherwise, count prose in the accessible portion of the buffer.

Return a list of the form (LINES SENTENCES WORDS CHARACTERS)."
  (interactive
   (cond
    ((use-region-p)
     (list (region-beginning) (region-end) 'region))
    (current-prefix-arg
     (save-excursion
       (org-back-to-heading t)
       (let ((begin (point)))
         (org-end-of-subtree t t)
         (list begin (point) 'subtree))))
    (t
     (list nil nil 'buffer))))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command is intended for Org buffers"))
  (let ((begin      (or begin (point-min)))
        (end        (or end (point-max)))
        (lines      0)
        (sentences  0)
        (words      0)
        (characters 0))
    (save-restriction
      (narrow-to-region begin end)
      (org-element-map
          (org-element-parse-buffer)
          'paragraph
        (lambda (paragraph)
          (when-let* ((text
                       (org-prose--paragraph-string paragraph)))
            (with-temp-buffer
              (insert text)
              (setq lines
                    (+ lines
                       (count-lines (point-min) (point-max)))
                    sentences
                    (+ sentences
                       (count-sentences (point-min) (point-max)))
                    words
                    (+ words
                       (count-words (point-min) (point-max)))
                    characters
                    (+ characters
                       (buffer-size))))))))
    (when (called-interactively-p 'interactive)
      (message
       "%s has %s line%s, %s sentence%s, %s word%s, and %s character%s"
       (pcase scope
         ('region  "Region")
         ('subtree "Subtree")
         (_        "Buffer"))
       (commify-number lines)
       (if (= lines 1) "" "s")
       (commify-number sentences)
       (if (= sentences 1) "" "s")
       (commify-number words)
       (if (= words 1) "" "s")
       (commify-number characters)
       (if (= characters 1) "" "s")))
    (list lines sentences words characters)))

(provide 'org-prose)

;;; org-prose.el ends here
