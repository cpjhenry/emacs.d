(defun cpj/org-disable-display-modes ()
  "Disable optional display modes in the current Org buffer."
  (org-comment-placeholder-mode -1)
  (org-hide-inline-footnotes-mode -1)
  (org-macro-display-mode -1)
  (org-quote-indent-mode -1))

 # Local Variables:
 # eval: (cpj/org-disable-display-modes)
 # End:

(add-to-list 'safe-local-eval-forms
             '(cpj/org-disable-display-modes))
