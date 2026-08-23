(defun cpj/org--prose-paragraph-string (paragraph)
  "Return the text of PARAGRAPH with footnote references removed.

Return nil when PARAGRAPH belongs to a footnote definition or to a
subtree excluded from export."
  (unless (or (org-element-lineage paragraph '(footnote-definition))
              (let ((headline
                     (org-element-lineage paragraph '(headline) t)))
                (and headline
                     (seq-intersection
                      (org-element-property :tags headline)
                      org-export-exclude-tags
                      #'string=))))
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
      (dolist (range (sort ranges
                           (lambda (a b)
                             (> (car a) (car b)))))
        (setq text
              (concat (substring text 0 (car range))
                      (substring text (cdr range)))))
      text)))

(defun cpj/org-count-prose (&optional begin end)
  "Count lines, sentences, words, and characters of Org prose.

Exclude headings, metadata, structural elements, footnote
definitions, and inline or labelled footnote references.

When the region is active, count prose within the region.
Otherwise, count prose in the accessible portion of the buffer.

Return a list of the form (LINES SENTENCES WORDS CHARACTERS)."
  (interactive
   (when (use-region-p)
     (list (region-beginning) (region-end))))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command is intended for Org buffers"))
  (let ((regionp    (and begin end))
        (begin      (or begin (point-min)))
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
                       (cpj/org--prose-paragraph-string paragraph)))
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
       "%s has %d line%s, %d sentence%s, %d word%s, and %d character%s"
       (if regionp "Region" "Buffer")
       lines       (if (= lines 1) "" "s")
       sentences   (if (= sentences 1) "" "s")
       words       (if (= words 1) "" "s")
       characters  (if (= characters 1) "" "s")))
    (list lines sentences words characters)))
