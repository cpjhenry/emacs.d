(setq-default message-log-max nil) ; Removes *Messages* from the buffer
(add-hook 'after-init-hook (kill-buffer "*Messages*"))
