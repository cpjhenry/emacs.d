;; Emacs server
(defun server-shutdown ()
	"Save buffers, Quit, and Shutdown (kill) server."
	(interactive)
	(save-some-buffers)
	(kill-emacs))
