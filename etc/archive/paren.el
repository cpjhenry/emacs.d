;; jump to matching paren

;; The idea behind this is really cool. Pressing % with the cursor on
;; (or before) a parenthesis (of any kind) will jump to the other
;; side. Unfortunately, it doesn't play well with Clojure, where % is
;; used for the "terse" lambda syntax (i.e. #(assoc foo :bar %))

(use-package paren
  :straight nil
  ;; :bind (("%" . op/match-paren))
  :config
  (show-paren-mode +1)

  ;; thanks, manual
  (defun op/match-paren (arg)
    "Go to the matchig paren if on a paren; otherwise self-insert."
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))
