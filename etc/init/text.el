;;; text.el --- text functions
;;; commentary:

;;; code:
(defun insert-tab-char ()
	"Insert a tab char. (ASCII 9, \t)."
	(interactive)
	(insert "\t"))

(defun unfill-paragraph ()
	"Take a multi-line paragraph and make it into a single line of text."
	(interactive)
	(let ((beg (point-min))
	      (end (point-max))
	      (fill-column (point-max)))
	(when (region-active-p)
	  (setq beg (region-beginning))
	  (setq end (region-end)))
	(fill-region beg end)))

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

(defun flush-blank-lines (start end)
	"Remove blank lines in a buffer."
	(interactive "r")
	(flush-lines "^\\s-*$" start end nil))

(defun delete-duplicate-words ()
	"Delete duplicate words via `query-replace-regexp'."
	(interactive nil text-mode)
	(save-excursion
		(goto-char (point-min))
		(query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1")))

;; https://www.emacswiki.org/emacs/ReplaceGarbageChars
(defun replace-garbage-chars ()
  "Replace goofy MS and other garbage characters with Latin1 equivalents."
	(interactive)
	(let ((beg (point-min))
	      (end (point-max)))
	  (when (region-active-p)
	    (setq beg (region-beginning))
	    (setq end (region-end)))
	  (save-excursion ;save the current point
	    (replace-string "΄" "'" nil beg end)
	    (replace-string "‘" "'" nil beg end)
	    (replace-string "’" "'" nil beg end)
	    (replace-string "“" "\"" nil beg end)
	    (replace-string "”" "\"" nil beg end)
	    (replace-string "" "'" nil beg end)
	    (replace-string "" "'" nil beg end)
	    (replace-string "" "\"" nil beg end)
	    (replace-string "" "\"" nil beg end)
	    (replace-string "" "\"" nil beg end)
	    (replace-string "" "\"" nil beg end)
	    (replace-string "‘" "\"" nil beg end)
	    (replace-string "’" "'" nil beg end)
	    (replace-string "¡\"" "\"" nil beg end)
	    (replace-string "¡­" "..." nil beg end)
	    (replace-string "" "..." nil beg end)
	    (replace-string "" " " nil beg end) ; M-SPC
	    (replace-string "" "`" nil beg end)  ; \221
	    (replace-string "" "'" nil beg end)  ; \222
	    (replace-string "" "``" nil beg end)
	    (replace-string "" "''" nil beg end)
	    (replace-string "" "*" nil beg end)
	    (replace-string "" "--" nil beg end)
	    (replace-string "" "--" nil beg end)
	    (replace-string " " " " nil beg end) ; M-SPC
	    (replace-string "¡" "\"" nil beg end)
	    (replace-string "´" "\"" nil beg end)
	    (replace-string "»" "<<" nil beg end)
	    (replace-string "Ç" "'" nil beg end)
	    (replace-string "È" "\"" nil beg end)
	    (replace-string "é" "e" nil beg end) ;; &eacute;
	    (replace-string "ó" "-" nil beg end)

	    ;; mine
	    (replace-string "•" "-" nil beg end)
	    (replace-string "–" "--" nil beg end)
	    (replace-string "—" "---" nil beg end) ; multi-byte
	    (replace-string "…" "..." nil beg end)
	    (replace-string "&#38;" "&" nil beg end)
	    (replace-string "&#39;" "'" nil beg end)

	    (message "Garbage in, garbage out.") )))

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

(defun replace-double-spaces ()
  "Replace double spaces in the buffer with single ones."
  (interactive)
  (save-excursion
    ;; HACK - region only, if selected.
    (goto-char (point-min))
    (replace-regexp "  " " ")))

(defun mark-from-beginning-of-buffer ()
  "Marks the region from the beginning of the buffer to point."
  (interactive)
  (push-mark (point-min) nil t))

;; https://www2.lib.uchicago.edu/keith/emacs/init.el
(defun undo-yank (arg)
  "Undo the yank you just did. Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))

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

;; eww functions

(defun eww-reddit-redirect (url)
	"Redirect reddit.com to old.reddit.com automatically."
	(replace-regexp-in-string "https://www.reddit.com" "https://old.reddit.com" url))
	(setq eww-url-transformers '(eww-remove-tracking eww-reddit-redirect))

;; prog-mode functions

(defun align-equals (begin end)
	(interactive "r")
	(align-regexp begin end "\\(\\s-*\\)=" 1 1))
