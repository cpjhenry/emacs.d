;; w3m routines

(defun macosx-open-url ()
	(interactive)
	(browse-url-default-macosx-browser w3m-current-url))
(defun my/w3m-quit ()
	(interactive)
	(w3m-quit 'FORCE))

;; https://tech.toryanderson.com/2021/06/09/how-to-get-readable-mode-in-emacs-w3m/
(defun tsa/readability (url) "Get the Readable.JS version of URL."
	(interactive "P")
	(message "readabilizing...")
	(erase-buffer)
	(insert (shell-command-to-string (concat "curl -s " url
	" 2>/dev/null |readability 2>/dev/null " url))))
(defun tsa/w3m-toggle-readability (&arg) "Toggle readability and reload the current page"
	(interactive "P")
	(w3m-toggle-filtering nil) ;; switch filtering on...
	(w3m-reload-this-page) ;; reload readability
	(w3m-toggle-filtering nil)) ;; and switch filtering back off again
