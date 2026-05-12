;; https://emacs.stackexchange.com/questions/51629/add-paragraph-numbers
(defun number-paragraphs (parg &optional takefirst)
  "Numbers resp. renumber paragraphs.

If starting from already numbered, take that value as offset.

Prefix removes numbering."
  (interactive "*P")

  ;; HACK un-number first,
  ;; renumber existing numbers isn't quite working
  (unnumber-paragraphs)

  (save-excursion
    (let ((prefix (car parg)))
      (cond ((not prefix)
	(let ((counter 0) (last 0))
	  ;; (when (looking-at "\\([0-9]+\\)\. ")
	  ;;   (setq counter (car (read-from-string  (match-string-no-properties 1))))
	  ;;   (forward-paragraph))
	  (while (and (forward-paragraph) (< last (point)))
	    (setq last (copy-marker (point)))
	    (backward-paragraph)
	    (skip-chars-forward " \t\r\n\f")
	    (when (looking-at "[0-9]+\. ")
	      (delete-region (match-beginning 0) (match-end 0)))
	    (insert (format "%s. " (1+ counter)))
	    (setq counter (1+ counter))
	    (goto-char last)))) ))))

(defun unnumber-paragraphs ()
"Remove numbering from paragraphs."
	(interactive)
	(save-excursion
	(replace-regexp "^[0-9]+\. " "")))
