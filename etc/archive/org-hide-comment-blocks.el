(defun org-hide-comment-blocks ()
  "Hide all #+begin_comment blocks in the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_comment\\b" nil t)
        (goto-char (match-beginning 0))  ; must be on the #+begin line
        (org-fold-hide-block-toggle t)   ; hide this block
        (forward-line 1)))))

(add-hook 'org-mode-hook #'org-hide-comment-blocks)
