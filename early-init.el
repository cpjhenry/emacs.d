;; early init file / pjh
(add-to-list 'default-frame-alist '(undecorated-round . t))

; don't show Emacs frame until initialized
(add-to-list 'initial-frame-alist '(visibility . nil))
(add-hook 'after-init-hook (lambda () (make-frame-visible)))
