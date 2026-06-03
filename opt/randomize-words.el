;; https://www.emacswiki.org/emacs/RandomizeWords
(defun randomize-region (beg end)
  "Randomize the order of words in region."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                              ;; Randomize words,
                              (cons (random) w)
                            ;; keep everything else in order.
                            (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))

(defun randomize-comma-separated-list (start end)
    "Randomize order of the comma separated list in the region."
    (interactive "r")
    (let ((str (mapconcat 'identity
			  (sort (split-string (replace-regexp-in-string
					       "\\s-+" " "
					       (buffer-substring start end))
					      " ?, ?" t)
				(lambda (a b) (= 1 (random 2))))
			  ", ")))
      (delete-region start end)
      (insert str)))

(defun randomize-string (string)
  "Randomize a string. Equivalent to glibc's strfry()."
  (interactive)
  (let ((i 0)
	(char " ")
	(size (string-width string)))
    (while (< i size)
      (let ((j (random size)))
	(store-substring char 0 (substring string i (+ 1 i)))
	(store-substring string i (substring string j (+ 1 j)))
	(store-substring string j char)
	(setq i (+ 1 i))))
    string))
