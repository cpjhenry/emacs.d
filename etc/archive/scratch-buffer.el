(if (< emacs-major-version 29)
  (defun scratch-buffer ()
    "Switch to the *scratch* buffer.
If the buffer doesn't exist, create it first."
    (interactive)
    (pop-to-buffer-same-window (get-scratch-buffer-create))))
