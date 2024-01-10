c;; Lists disabled commands in Emacs
(setq xbuff (generate-new-buffer "*disabled commands*"))
(mapatoms (lambda (x) (when (get x 'disabled) (print x xbuff) ) ) )
(switch-to-buffer xbuff)

;; TODO switch to this code
;(let ((xbuff (generate-new-buffer "*my output*")))
;	(with-output-to-temp-buffer xbuff
;	(print "abc")
;	))
