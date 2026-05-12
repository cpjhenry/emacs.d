;;; text-functions.el --- text functions
;;; commentary:

;;; code:
(defun my/fill-paragraph ()
  "Call `fill-paragraph' region-sensitive."
  (interactive)
  (fill-paragraph nil t))

(defun flush-blank-lines (beg end)
  "Remove blank lines in a buffer.

BEG and END mark the limits of the region."
  (interactive "r")
  (flush-lines "^\\s-*$" beg end nil))

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

(defun mark-from-beginning-of-buffer ()
  "Mark the region from the beginning of the buffer to point."
  (interactive)
  (push-mark (point-min) nil t))

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
        (while (re-search-forward
		"\\[\\(?:[0-9]+\\(?:[-–][0-9]+\\)?\\|[a-zA-Z]\\(?:[-–][a-zA-Z]\\)?\\)\\]" end t)
          (replace-match "")
          (setq count (1+ count)))))
    (when (called-interactively-p 'interactive)
      (message "Removed %d footnotes." count))
    count))

;; https://github.com/sprig/org-capture-extension
(defun transform-square-brackets-to-round-ones (string-to-transform)
  "Transforms `\[' into `\(' and `\]' into `\)', other chars left unchanged."
  (concat (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

;; https://www2.lib.uchicago.edu/keith/emacs/init.el
(defun undo-yank (arg)
"Undo the yank you just did. Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))

(defun zero-width-space ()
  "Insert ZERO WIDTH SPACE."
  (interactive)
  (insert-char (char-from-name "ZERO WIDTH SPACE"))
  (message "ZWS"))

;;; text-functions.el ends here
