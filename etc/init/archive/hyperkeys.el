;; add Hyper- keys (C-M-s-…) to terminal frames (iTerm2)
(add-hook 'server-after-make-frame-hook (lambda()
	;; (unless (display-graphic-p)

	;; TODO add C-M-s translations for display-graphic
	(cl-loop for char from ?a to ?z do
	(define-key input-decode-map (format "\e[1;P%c" char) (kbd (format "H-%c" char))))))
