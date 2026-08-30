;;; jinx-functions.el --- Jinx Tweaks -*- lexical-binding: t; -*-

;;; Commentary:
;; Adapted from the Jinx wiki:
;; https://github.com/minad/jinx/wiki
;;
;; These helpers integrate Ispell's per-file LocalWords with Jinx.
;; They use Jinx internals and may require adjustment if Jinx changes
;; its save-handler API.

;;; Code:
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
  (let ((localwords (split-string (my/jinx-ispell-localwords))))
    (setq jinx-local-words
          (string-join
           (delete-dups
            (append (split-string jinx-local-words) localwords))
           " "))
    (setq jinx--session-words
          (delete-dups
           (append jinx--session-words localwords)))))

(defun my/jinx-save-as-ispell-localword (action key word)
  "Add or remove WORD using Ispell's `ispell-words-keyword'.

ACTION may be `add', `remove', `has', or `format'.
In the latter case, format the candidate using KEY."
  (pcase-exhaustive action
    ('add
     (require 'ispell)
     (ispell-add-per-file-word-list word)
     (add-to-list 'jinx--session-words word)
     (setq jinx-local-words
           (string-join
            (sort
             (delete-dups
              (cons word (split-string jinx-local-words)))
             #'string<)
            " ")))
    ('remove
     (cl-callf2 remove word jinx--session-words))
    ('has
     (member word (split-string jinx-local-words)))
    ('format
     `((,(char-to-string key) ,word "File")))))

(provide 'jinx-functions)

;;; jinx-functions.el ends here
