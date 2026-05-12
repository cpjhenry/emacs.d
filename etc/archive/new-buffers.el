(defvar	default-major-mode 'text-mode "Mode when creating new buffers.")

(defun new-empty-buffer ()
  "Create new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and default-major-mode))
    (setq buffer-offer-save t) ))

(defun new-markdown-buffer ()
"Create new empty `markdown-mode' buffer."
(interactive)
(let ((buf (generate-new-buffer "untitled\.md")))
  (switch-to-buffer buf)
  (markdown-mode)
  (setq buffer-offer-save t) ))

(defun new-org-buffer ()
"Create new empty `org-mode' buffer."
(interactive)
(let ((buf (generate-new-buffer "untitled\.org")))
  (switch-to-buffer buf)
  (org-mode)
  (setq buffer-offer-save t) ))

(bind-key "C-c b m"	'new-markdown-buffer)
(bind-key "C-c b n"	'new-empty-buffer)
(bind-key "C-c b o"	'new-org-buffer)
(which-key-alias "C-c b" "buffers")
