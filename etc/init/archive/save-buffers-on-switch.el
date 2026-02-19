;; automatically save buffers associated with files on frame (app) switch
(add-hook 'focus-out-hook (lambda() (save-some-buffers t)))
