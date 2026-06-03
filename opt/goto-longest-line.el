;;; goto-longest-line.el --- Jump to longest line -*- lexical-binding: t; -*-
;; https://www.emacswiki.org/emacs/download/misc-cmds.el

;; Extracted from code by Drew Adams.
;; Local modifications:
;; - Added lexical-binding compatibility.
;; - Factored temporary highlighting into
;;   `goto-longest-line--flash'.
;; - Flymake/checkdoc cleanup.

(defvar hl-line-mode)
(declare-function hl-line-highlight "hl-line")
(declare-function hl-line-unhighlight "hl-line")

(defun goto-longest-line--flash ()
  "Briefly highlight the current line."
  (when (require 'hl-line nil t)
    (let ((hl-line-mode t))
      (hl-line-highlight))
    (add-hook 'pre-command-hook #'hl-line-unhighlight nil t)))

(defun goto-longest-line (beg end &optional msgp)
  "Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly)."

  (interactive
   (if (or (not mark-active)  (not (< (region-beginning) (region-end))))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark) 'MSGP)
       (list (mark) (point) 'MSGP))))
  (when (and (not mark-active)  (= beg end)) (error "The buffer is empty"))
  (when (and mark-active  (> (point) (mark))) (exchange-point-and-mark))
  (when (< end beg) (setq end  (prog1 beg (setq beg  end))))
  (when (eq 'goto-longest-line last-command)
    (forward-line 1) (setq beg  (point)))
  (goto-char beg)
  (when (eobp) (error "End of buffer"))
  (cond ((<= end (save-excursion
		   (goto-char beg)
		   (forward-line 1)
		   (point)))
         (let ((inhibit-field-text-motion  t))
	   (beginning-of-line))
	 (goto-longest-line--flash)
         (let ((lineno  (line-number-at-pos))
               (chars   (let ((inhibit-field-text-motion  t))
                          (save-excursion (end-of-line) (current-column)))))
           (message "Only line %d: %d chars" lineno chars)
           (let ((visible-bell  t))  (ding))
           (setq mark-active  nil)
           (list lineno chars nil 1)))
        (t
         (let* ((start-line                 (line-number-at-pos))
                (max-width                  0)
                (line                       start-line)
                (inhibit-field-text-motion  t)
                long-lines col)
           (when (eobp) (error "End of buffer"))
           (while (and (not (eobp))  (< (point) end))
             (end-of-line)
             (setq col  (current-column))
             (when (>= col max-width)
               (setq long-lines  (if (= col max-width)
                                     (cons line long-lines)
                                   (list line))
                     max-width   col))
             (forward-line 1)
             (setq line  (1+ line)))
           (setq long-lines  (nreverse long-lines))
           (let ((lines  long-lines))
             (while (and lines  (> start-line (car lines))) (pop lines))
             (goto-char (point-min))
             (when (car lines) (forward-line (1- (car lines)))))
           (goto-longest-line--flash)
           (when msgp
             (let ((others  (cdr long-lines)))
               (message "Line %d: %d chars%s (%d lines measured)"
                        (car long-lines) max-width
                        (concat
                         (and others
                              (format ", Others: {%s}" (mapconcat
                                                        (lambda (line) (format "%d" line))
                                                        (cdr long-lines) ", "))))
                        (- line start-line))))
           (list (car long-lines) max-width (cdr long-lines) (- line start-line))))))

(provide 'goto-longest-line)
;;; goto-longest-line.el ends here

; LocalWords:  goto hl
