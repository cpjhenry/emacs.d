;;; calfw-blocks-modern.el --- Block views for modern calfw -*- lexical-binding: t; -*-

;;; Commentary:

;; Modern block-view extension for calfw.
;;
;; The original `calfw-blocks' package targets the old `cfw:' compatibility
;; API.  This package targets current calfw internals instead.

;;; Code:

(require 'calfw)
(require 'cl-lib)

(defun calfw-blocks-modern--dispatch-view (oldfun view)
  "Dispatch block VIEW, falling back to OLDFUN."
  (pcase view
    ;; Temporary alias while the real block renderer is being written.
    ('block-3-day (funcall oldfun 'day))
    (_ (funcall oldfun view))))

;;;###autoload
(define-minor-mode calfw-blocks-modern-mode
  "Enable modern block views for calfw."
  :global t
  (if calfw-blocks-modern-mode
      (advice-add 'calfw--cp-dispatch-view-impl
                  :around #'calfw-blocks-modern--dispatch-view)
    (advice-remove 'calfw--cp-dispatch-view-impl
                   #'calfw-blocks-modern--dispatch-view)))

(provide 'calfw-blocks-modern)

;;; calfw-blocks-modern.el ends here
