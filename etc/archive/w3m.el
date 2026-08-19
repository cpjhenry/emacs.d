;;;; Web
(use-package w3m
  :defer t
  :bind ( :map w3m-mode-map
	  ("<left>" . w3m-view-previous-page)
	  ("&" . macosx-open-url)
	  ("Q" . my/w3m-quit)
	  ("M-o" . ace-link-w3m))
  :commands (w3m-browse-url)
  ;; :init (setq browse-url-browser-function 'w3m-browse-url)
  :config (setq
	   w3m-bookmark-file (concat user-emacs-directory "etc/w3m-bookmarks.html")
	   w3m-confirm-leaving-secure-page nil
	   w3m-default-save-directory "~/Downloads"
	   w3m-use-filter nil)
  (load "w3m-functions" nil 'nomessage))
