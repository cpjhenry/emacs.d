;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "memoir-tex"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("memoir" "")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("lipsum" "")))
   (TeX-run-style-hooks
    "latex2e"
    "memoir"
    "memoir10"
    "lipsum"))
 :latex)

