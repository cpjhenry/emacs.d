;; Lists disabled commands in Emacs
(mapatoms (lambda (x) (when (get x 'disabled) (message "%s" x) ) ) )
