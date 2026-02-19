;; https://whhone.com/emacs-config/

(defvar my/save-buffer-with-thread t
  "If non-nil, execute save-buffer with thread so it does not block the UI")

(defun my/save-buffer-advice (orig-fun &rest args)
  (unless (or (buffer-file-name)                       ; regular buffer
              (buffer-file-name (buffer-base-buffer))) ; indirect buffer
    (user-error "Use 'M-x write-file' to save this buffer."))

  (my/delete-trailing-whitespace-except-current-line)

  (if (and my/save-buffer-with-thread
           (not (file-remote-p default-directory))
           ;; Editing with a thread can prevent a buffer from being killed.
           ;; Disable threading on `with-editor-mode' because its
           ;; `with-editor-finish' need to kill the buffer.
           (not (bound-and-true-p with-editor-mode)))
      (make-thread
       (condition-case err
           (apply orig-fun args)
         (error
          (message "Error from my/save-buffer-advice %S" err)
          nil))
       "my/save-buffer-advice")
    (apply orig-fun args)))

(advice-add 'save-buffer :around #'my/save-buffer-advice)

;; Remove trailing whitespace except current line.
;; https://stackoverflow.com/a/35781486/1747877
(defun my/delete-trailing-whitespace-except-current-line ()
  "Delete trailing whitespace in the whole buffer, except on the current line.
  The current line exception is because we do want to remove any whitespace
  on the current line on saving the file while we are in-between typing something.

  Do not do anything if `do-not-delete-trailing-whitespace' is non-nil."
  (interactive)
  (when (not (bound-and-true-p do-not-delete-trailing-whitespace))
    (delete-trailing-whitespace (point-min) (line-beginning-position))
    (delete-trailing-whitespace (line-end-position) (point-max))))
