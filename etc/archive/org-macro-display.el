;;; org-macro-display.el --- org-mode display replacements
;;; commentary:

;;; code:
(defvar org-macro-displays
  '(("\\fd{}" . ".....")
    ("\\td{}" . "…"))
  "Alist of exact Org/LaTeX macros and their display replacements.")

(defun org-macro-display-setup ()
  "Display exact macros from `org-macro-displays` in Org buffers."
  (font-lock-add-keywords
   nil
   (mapcar
    (lambda (pair)
      (let ((re (regexp-quote (car pair)))
            (disp (cdr pair)))
        `(,re
          (0 (prog1 nil
               (put-text-property (match-beginning 0)
                                  (match-end 0)
                                  'display ,disp))))))
    org-macro-displays)
   'append)
  (font-lock-flush))

(add-hook 'org-mode-hook #'org-macro-display-setup)

(provide 'org-macro-display)
;;; org-macro-display.el ends here
