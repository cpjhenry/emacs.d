(setq
	gnus-use-cache t
	gnus-use-dribble-file t
	gnus-always-read-dribble-file t

	gnus-select-method user-gnus-select-method
	gnus-thread-hide-subtree t
	gnus-thread-ignore-subject t
	gnus-newsgroup-maximum-articles 100
	gnus-read-active-file 'some
	gnus-summary-line-format "%U%R%z %d  %s\n"
	gnus-thread-sort-functions
		'(gnus-thread-sort-by-most-recent-date
		(not gnus-thread-sort-by-number))
	gnus-use-cache t
	gnus-use-correct-string-widths nil)

;; press "o" to view all groups
;; gnus-guide-en.org
(defun my-gnus-group-list-subscribed-groups ()
	"List all subscribed groups with or without un-read messages"
	(interactive)
	(gnus-group-list-all-groups 5))

;; list all the subscribed groups even they contain zero un-read messages
(define-key gnus-group-mode-map (kbd "o") 'my-gnus-group-list-subscribed-groups)

;; Tree view for groups.
;(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
