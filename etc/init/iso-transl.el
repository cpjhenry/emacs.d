(with-eval-after-load 'iso-transl
	(dolist (transl-char iso-transl-char-map)
		(let ((vec (vconcat (car transl-char))))
    	(aset vec 0 (logior (aref vec 0) ?\A-\^@))
    	(define-key key-translation-map vec nil))) )
