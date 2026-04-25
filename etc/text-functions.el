;;; text-functions.el --- text functions
;;; commentary:

;;; code:
(defun my/fill-paragraph ()
  "Call `fill-paragraph' region-sensitive."
  (interactive)
  (fill-paragraph nil t))

(defun insert-iso-date ()
  "Insert ISO-formatted date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-date ()
  "Insert European-formatted date."
  (interactive)
  (insert (format-time-string "%-d %B %Y")))

(defun markdown-preview-file ()
  "Run `Marked' on the current file and revert the buffer."
  (interactive)
  (shell-command (format "open -a /Applications/Marked\\ 2.app %s"
		 (shell-quote-argument (buffer-file-name)))
		 nil nil))

(defun flush-blank-lines (beg end)
  "Remove blank lines in a buffer.

BEG and END mark the limits of the region."
  (interactive "r")
  (flush-lines "^\\s-*$" beg end nil))

(defun delete-duplicate-blank-lines (&optional quiet)
  "Remove duplicate blank lines in region or buffer.

When called interactively:
- no prefix → report number removed
- \\[universal-argument] → suppress message

When called from Lisp:
- QUIET non-nil suppresses message.

Returns the number of duplicate blank lines removed."
  (interactive "P")
  (let* ((beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end)       (point-max)))
         (inhibit-read-only t)
         (before (count-lines beg end)))
    (delete-duplicate-lines beg end nil t)
    ;; Buffer may have shrunk; clamp END again before recounting.
    (let* ((end (max (point-min) (min end (point-max))))
           (after (count-lines beg end))
           (removed (- before after)))
      (unless quiet
        (message "Removed %d duplicate blank line%s."
                 removed
                 (if (= removed 1) "" "s")))
      removed)))

;; https://emacs.stackexchange.com/questions/51629/add-paragraph-numbers
(defun number-paragraphs (parg &optional takefirst)
  "Numbers resp. renumber paragraphs.

If starting from already numbered, take that value as offset.

Prefix removes numbering."
  (interactive "*P")

  ;; HACK un-number first,
  ;; renumber existing numbers isn't quite working
  (unnumber-paragraphs)

  (save-excursion
    (let ((prefix (car parg)))
      (cond ((not prefix)
	(let ((counter 0) (last 0))
	  ;; (when (looking-at "\\([0-9]+\\)\. ")
	  ;;   (setq counter (car (read-from-string  (match-string-no-properties 1))))
	  ;;   (forward-paragraph))
	  (while (and (forward-paragraph) (< last (point)))
	    (setq last (copy-marker (point)))
	    (backward-paragraph)
	    (skip-chars-forward " \t\r\n\f")
	    (when (looking-at "[0-9]+\. ")
	      (delete-region (match-beginning 0) (match-end 0)))
	    (insert (format "%s. " (1+ counter)))
	    (setq counter (1+ counter))
	    (goto-char last)))) ))))

(defun unnumber-paragraphs ()
"Remove numbering from paragraphs."
	(interactive)
	(save-excursion
	(replace-regexp "^[0-9]+\. " "")))

(defun collapse-multiple-spaces-in-region (beg end)
  "Collapse runs of 2+ literal spaces to one space in the active region.
Tabs untouched. Skips Org tables/src/fixed-width blocks.
Requires Transient Mark Mode with a visibly active region."
  (interactive "r")
  (unless (and transient-mark-mode (use-region-p))
    (user-error "No active region (Transient Mark Mode required)"))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t)
        (unless (or (and (derived-mode-p 'org-mode)
                         (or (org-at-table-p)
                             (org-in-src-block-p)
                             (org-in-fixed-width-region-p))))
          (replace-match " " t t))))))

(defun normalize-text-dwim (beg end)
  "Normalize text in region, or whole buffer if no region.

Removes leading whitespace, trailing whitespace, and collapses multiple
blank lines to a single blank line. Internal spacing within lines is left
alone."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      ;; Remove leading and trailing whitespace line by line.
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (delete-horizontal-space)
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1))

      ;; Collapse 3+ newlines to 2 newlines.
      ;; This preserves paragraph breaks, but removes blank-line bloat.
      (goto-char (point-min))
      (while (re-search-forward "\n\\{3,\\}" nil t)
        (replace-match "\n\n")))))

(defun mark-from-beginning-of-buffer ()
  "Mark the region from the beginning of the buffer to point."
  (interactive)
  (push-mark (point-min) nil t))

;; https://www2.lib.uchicago.edu/keith/emacs/init.el
(defun undo-yank (arg)
"Undo the yank you just did. Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))

;; https://emacs.stackexchange.com/questions/35069/best-way-to-select-a-word
(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but select whole words and skips over whitespace.
If you use a negative prefix ARG then select words backward.
Otherwise select them forward.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)

When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
set ARG words from point.

With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix
argument), the place to which mark goes is the same place \\[forward-word]
would move to with the same argument; if the mark is active, it moves
ARG words from its current position, otherwise it is set ARG words
from point."
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
        (skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
                (if (natnump num)
                    (looking-at "\\b")
                  (looking-back "\\b")))
      (if (natnump num)
          (left-word)
        (right-word)))
    (mark-word arg allow-extend)))

(defun indent-whole-buffer ()
  "Indent the entire buffer without affecting point or mark."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

;; https://github.com/jakebox/jake-emacs/blob/main/jake-emacs/jib-funcs.el
(defun speaking-time ()
  "Calculate how long it would take me to speak aloud the selection."
  (interactive)
  (if (use-region-p)
      (let* ((wpm 150)
	     (word-count (float (count-words-region (region-beginning) (region-end))))
	     (raw-time (* 60 (/ word-count wpm))))
	(message "%s minutes, %s seconds to speak at %d wpm (%d words)"
		 (format-seconds "%m" raw-time)
		 (floor (mod raw-time 60)) wpm word-count))
    (error "Error: select a region")))

;; https://emacs.stackexchange.com/questions/45584/count-the-number-of-paragraphs
(defun count-paragraphs (start end)
  "Return number of paragraphs between START and END."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (- (buffer-size) (forward-paragraph (buffer-size))))))

(defun count-paragraphs-region-or-buffer ()
  "Report number of paragraphs in the region (if it's active) or the entire buffer."
  (declare (interactive-only count-paragraphs))
  (interactive)
  (let ((paragraphs (if (use-region-p)
                        (count-paragraphs (region-beginning) (region-end))
                        (count-paragraphs (point-min) (point-max)))))
    (message "%s has %d paragraph%s"
             (if (use-region-p) "Region" "Buffer")
             paragraphs
             (if (> paragraphs 1) "s" ""))))

;; https://github.com/sprig/org-capture-extension
(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms `\[' into `\(' and `\]' into `\)', other chars left unchanged."
  (concat (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

(defun remove-wikipedia-footnotes (&optional beg end)
  "Remove Wikipedia-style numeric footnotes like [1], [23].

Operate on region if active, otherwise entire buffer."
  (interactive
   (list (when (region-active-p) (region-beginning))
         (when (region-active-p) (region-end))))
  (let ((beg (or beg (point-min)))
        (end (or end (point-max)))
        (count 0))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char beg)
        (while (re-search-forward "\\[[0-9]+\\|[a-zA-Z]\\(?:[-–][a-zA-Z]\\)?\\]" end t)
          (replace-match "")
          (setq count (1+ count)))))
    (when (called-interactively-p 'interactive)
      (message "Removed %d footnotes." count))
    count))

(defun zero-width-space ()
  "Insert ZERO WIDTH SPACE."
  (interactive)
  (insert-char (char-from-name "ZERO WIDTH SPACE"))
  (message "ZWS"))

;; prog-mode functions

(defun align-equals (beg end)
  "Line up equal signs.

BEG and END mark the limits of the region."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)=" 1 1))

;;; text-functions.el ends here
