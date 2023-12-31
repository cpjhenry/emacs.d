;; Newsticker
(require 'newsticker) (setq
newsticker-dir (concat user-emacs-directory "var/newsticker/")
newsticker-html-renderer nil
newsticker-treeview-own-frame t
newsticker-treeview-use-feed-name-from-url-list-in-itemview nil
newsticker-treeview-use-feed-name-from-url-list-in-treeview nil
newsticker-url-list-defaults nil)

(add-to-list 'newsticker-url-list '("Slashdot" "https://rss.slashdot.org/Slashdot/slashdotMain"))

(add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
(global-set-key [remap gnus] 'newsticker-show-news)
