;;; dynamic-fill-column-mode --- adjust according to screen-width

;; https://emacs.stackexchange.com/questions/46935/adjust-the-line-according-to-the-screen-width
(defun dynamic-fill-column-set-var (frame)
  (when dynamic-fill-column-mode
    (setq fill-column (- (window-total-width) 3))))

(defun dynamic-fill-column-buffer-list-change ()
  (when dynamic-fill-column-mode
    (setq fill-column (- (window-total-width) 3))))

(define-minor-mode dynamic-fill-column-mode
  "Sets `fill-column' when buffer's window is resized"
  :lighter " DFC"
  (if dynamic-fill-column-mode
      (progn
        (add-hook 'window-size-change-functions 'dynamic-fill-column-set-var nil t)
        (add-hook 'buffer-list-update-hook 'dynamic-fill-column-buffer-list-change nil t))
    (remove-hook 'window-size-change-functions 'dynamic-fill-column-set-var t)
    (remove-hook 'buffer-list-update-hook 'dynamic-fill-column-buffer-list-change t)))
