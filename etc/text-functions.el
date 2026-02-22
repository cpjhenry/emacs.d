;;; text-functions.el --- text functions
;;; commentary:

;;; code:
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

(defun delete-duplicate-words ()
  "Delete duplicate words via `query-replace-regexp'."
  (interactive nil text-mode)
  (save-excursion
    (goto-char (point-min))
    (query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1")))

;; Inspired by https://www.emacswiki.org/emacs/ReplaceGarbageChars
(defvar replace-garbage-chars-alist
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
    ("´" . "\"")
    ("»" . "<<")
    ("Ç" . "'")
    ("È" . "\"")
    ("é" . "e")
    ("ó" . "-")
    ("•" . "-")
    ("–" . "--")
    ("—" . "---")
    ("&#x27;" . "'")
    ("&#38;" . "&")
    ("&#39;" . "'")

    ;; Ellipsis normalization
    ("…"   . "...")
    ("… "  . "... ")     ; sometimes comes with trailing space
    (". . ." . "...")    ; spaced dots
    (". . . " . "... ")
    (" . . ." . "...")   ; leading space variants
    (" . . . " . "... ")
    ("&#8230;" . "...")  ; HTML decimal entity
    ("&#x2026;" . "...") ; HTML hex entity
    ("&hellip;" . "...") ; named HTML entity
    )
  "Alist of (FROM . TO) replacements for `replace-garbage-chars`.")

(defun replace-garbage-chars (&optional beg end quiet)
  "Replace MS/CP1252 and other garbage characters with plain equivalents.

If region is active (BEG END), operate on region; otherwise on whole buffer.

When called interactively:
- no prefix → report replacement count
- \\[universal-argument] → suppress count message

When called from Lisp:
- QUIET non-nil suppresses the message.

Returns the number of replacements made."
  (interactive
   (list (if (region-active-p) (region-beginning))
         (if (region-active-p) (region-end))
         current-prefix-arg))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (count 0)
         (suppress (and quiet t)))   ; normalize truthiness
    (atomic-change-group
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (dolist (pair replace-garbage-chars-alist)
            (goto-char (point-min))
            (let ((from (car pair))
                  (to   (cdr pair)))
              (while (search-forward from nil t)
                (replace-match to t t)
                (setq count (1+ count))))))))
    (unless suppress
      (message "Garbage in, garbage out. %d replacement%s made."
               count (if (= count 1) "" "s")))
    count))

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

(defun narrow-to-section ()
  "Narrow buffer to text section."
  (interactive)
  (save-excursion
    (push-mark)

    ;; HACK - do something at end of file,
    ;; or when there's no marker

    (search-forward "") ; group separator
    (left-char)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark)))

;; https://speechcode.com/blog/narrow-to-focus/
(defun narrow-to-focus (start end)
"If the region is active, narrow to region, marking it (and only
it) for the future. If the mark is not active, narrow to the
region that was the most recent focus."
	(interactive "r")
	(cond ((use-region-p)
	(remove-overlays (point-min) (point-max) 'focus t)
	(let ((overlay (make-overlay start end)))
		(overlay-put overlay 'focus t)
		(narrow-to-region start end)))
	(t (let ((focus
		(seq-find (lambda (o) (overlay-get o 'focus))
		(overlays-in (point-min) (point-max)))))
	(when focus
		(narrow-to-region (overlay-start focus)
		(overlay-end focus)))))))

(defun replace-double-spaces ()
  "Replace double spaces in the buffer with single ones."
  (interactive)
  (save-excursion
    ;; HACK - region only, if selected.
    (goto-char (point-min))
    (replace-regexp "  " " ")))

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
(defun calc-speaking-time ()
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

;; prog-mode functions

(defun align-equals (beg end)
  "Line up equal signs.

BEG and END mark the limits of the region."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)=" 1 1))

;;; text-functions.el ends here
