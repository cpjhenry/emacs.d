;; Convenience functions when working with PDF exports
;; https://github.com/munen/emacs.d

(defun md-compile ()
  "Compiles the currently loaded markdown file using pandoc into a PDF"
  (interactive)
  (save-buffer)
  (shell-command (concat "pandoc " (buffer-file-name) " -o "
                         (replace-regexp-in-string "md" "pdf" (buffer-file-name)))))

(defun update-other-buffer ()
  (interactive)
  (other-window 1)
  (revert-buffer nil t)
  (other-window -1))

(defun md-compile-and-update-other-buffer ()
  "Has as a premise that it's run from a markdown-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (md-compile)
  (update-other-buffer))

(defun latex-compile-and-update-other-buffer ()
  "Has as a premise that it's run from a `latex-mode' buffer and
   that the other buffer already has the PDF open."
  (interactive)
  (save-buffer)
  (shell-command (concat "pdflatex " (buffer-file-name)))
  (switch-to-buffer (other-buffer))
  (kill-buffer)
  (update-other-buffer))

(defun org-compile-beamer-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-beamer-export-to-pdf)
  (update-other-buffer))

(defun org-compile-latex-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-latex-export-to-pdf)
  (update-other-buffer))
