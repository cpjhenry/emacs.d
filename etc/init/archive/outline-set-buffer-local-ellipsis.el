;; https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
(defun outline-set-buffer-local-ellipsis (ellipsis)
  "Set a buffer-local ellipsis string ELLIPSIS for outline folding display.

This function configures the current buffer to use a custom ellipsis string for
selective display, typically in `outline-mode' or `outline-minor-mode'.

The string ELLIPSIS is trimmed of trailing whitespace before use, as such
whitespace can be misleading when lines are truncated or visually wrapped. In
those cases, the trailing space may appear on a new visual line, creating the
false impression of an additional line. Deleting this apparent line can
inadvertently remove the entire folded logical line."
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c)
                                   (+ face-offset c))
                                 (string-trim-right ellipsis)))))
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table)))

(add-hook 'outline-minor-mode #'(lambda() (outline-set-buffer-local-ellipsis " ▼")))
