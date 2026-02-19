;;; input-methods.el --- modifying your keyboard input to support languages
;;; that you wouldn’t ordinarily be able to type with your keyboard layout.

;;; commentary:
;; https://www.masteringemacs.org/article/inserting-emoji-input-methods

;;; code:
(quail-define-package
  "Arrows" "UTF-8" "→" nil
  "Arrow input mode"
  nil t t nil nil nil nil nil nil nil t)

(quail-define-rules
  ("->" ?→))

;; Tests
;; This is a test. After all, we're going -> this → way.
;; Indeed, right?

;;; input-methods.el ends here
