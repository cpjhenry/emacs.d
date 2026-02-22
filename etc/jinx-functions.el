;; https://github.com/minad/jinx/wiki
(defun my/jinx-ispell-localwords ()
"Return a string of Ispell's local words. Those are the words
following `ispell-words-keyword' (usually \"LocalWords\") in the
current buffer."
	(require 'ispell)
	(save-excursion
	(goto-char (point-min))
	(cl-loop while (search-forward ispell-words-keyword nil t)
	collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
	finally return (mapconcat #'identity result " "))))

(defun my/jinx-add-ispell-localwords ()
"Add Ispell's local words to `jinx-local-words'."
	(let ((ispell-localwords (my/jinx-ispell-localwords)))
	(setq jinx-local-words (concat jinx-local-words ispell-localwords))
	(setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))

(defun my/jinx-save-as-ispell-localword (save key word)
"Save WORD using Ispell's `ispell-words-keyword'. If SAVE is
 non-nil save, otherwise format candidate given action KEY."
	(if save (progn
		(require 'ispell)
		(ispell-add-per-file-word-list word)
		(add-to-list 'jinx--session-words word)
		(setq jinx-local-words (string-join (sort
		(delete-dups (cons word (split-string jinx-local-words))) #'string<)
		" "))))
		(list key word "File"))
