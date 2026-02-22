;; https://stackoverflow.com/questions/16779882/save-buffer-as-a-pdf-with-ns-write-file-using-panel-or-similar-option
(defun harden-newlines ()
  (interactive)
  "Make all the newlines in the buffer hard."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (backward-char)
      (put-text-property (point) (1+ (point)) 'hard t)
      (forward-char))))

(defun spool-buffer-given-name (name)
  (let ((ps-left-header (list (format "(%s)" name))))
    (ps-spool-buffer-with-faces)))

(defun print-to-pdf (pdf-file-name)
  "Print the current file to the given file."
  (interactive "FWrite PDF file: ")
  (let ((ps-file-name (concat (file-name-sans-extension pdf-file-name) ".ps"))
        (wbuf (generate-new-buffer "*Wrapped*"))
        (sbuf (current-buffer)))
    (jit-lock-fontify-now)
    (save-current-buffer
      (set-buffer wbuf)
      (insert-buffer sbuf)
      (setq fill-column 95)
      (longlines-mode t)
      (harden-newlines)
      (message (buffer-name sbuf))
      (spool-buffer-given-name (buffer-name sbuf))
      (kill-buffer wbuf)
      (switch-to-buffer "*PostScript*")
      (write-file ps-file-name t)
      (kill-buffer (current-buffer)))
    (call-process "ps2pdf14" nil nil nil ps-file-name pdf-file-name)
    (delete-file ps-file-name)
    (message "PDF saved to %s" pdf-file-name)))

;; Local Variables:
;; truncate-lines: -1
;; End:
