;; buffer movement
(require 'windmove) ; use alt + arrow keys to switch between visible buffers
(windmove-default-keybindings 'meta) ; ‘M-left’ and ‘M-right’ to switch windows

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)  
