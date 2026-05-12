;; Enable auto-fill for comments in prog-mode buffers:
(defun op/auto-fill-comment ()
  "Enable auto-fill for comments."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode))
(add-hook 'prog-mode-hook #'op/auto-fill-comment)

I finally found a usage for the arrow keys: copy-from-above-command! I've bound that function to the up arrow, so it's easy to copy the previous line. The function bound to the down arrow duplicates the current line.

(define-key prog-mode-map [up] #'copy-from-above-command)

(defun op/dup-line ()
  "Duplicate the current line, using `copy-from-above-command'."
  (interactive)
  (save-excursion
    (forward-line 1)
    (open-line 1)
    (copy-from-above-command))
  (call-interactively #'next-line))
(define-key prog-mode-map [down] #'op/dup-line)
