;; Lists disabled commands in Emacs
(setq xbuff (generate-new-buffer "*disabled commands*"))
(mapatoms (lambda (x) (when (get x 'disabled) (print x xbuff) ) ) )
(switch-to-buffer xbuff)
