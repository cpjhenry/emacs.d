(mapatoms (lambda (x) (when (get x 'disabled) (message "disabled: %s" x) ) ) )
