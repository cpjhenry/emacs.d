;; https://www.emacswiki.org/emacs/DeletingWhitespace
(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.

With prefix ARG delete across newlines as well. The only danger in this
is that you don't have to actually be at the end of a word to make it
work. It skips over to the next whitespace and then whacks it all to the
next word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))
