;; see https://orgmode.org/list/87r5718ytv.fsf@sputnik.localhost
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))
(defun ndk/checkbox-list-complete ()
	(save-excursion
		(org-back-to-heading t)
		(let ((beg (point)) end)
			(end-of-line)
			(setq end (point))
			(goto-char beg)
			(if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
				(if (match-end 1)
					(if (equal (match-string 1) "100%")
						;; all done - do the state change
						(org-todo 'done)
						(org-todo 'todo))
					(if (and (> (match-end 2) (match-beginning 2))
						(equal (match-string 2) (match-string 3)))
						(org-todo 'done)
						(org-todo 'todo)))))))

(provide 'org-stats-complete)
