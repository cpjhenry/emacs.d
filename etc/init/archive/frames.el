;; Frame functions
;(when (display-graphic-p)
;(add-to-list 'default-frame-alist '(background-color . "Ivory")))
;(add-to-list 'default-frame-alist '(fullscreen . maximized))

`(defvar kill-frame-when-buffer-killed-buffer-list
  '("*RefTeX Select*" "*Help*" "*Popup Help*")
  "Buffer names for which the containing frame should be
  killed when the buffer is killed.")

(defun kill-frame-if-current-buffer-matches ()
  "Kill frames as well when certain buffers are closed, helps stop some
  packages spamming frames."
 (interactive)
 (if (member (buffer-name) kill-frame-when-buffer-killed-buffer-list)
     (delete-frame)))

(add-hook 'kill-buffer-hook 'kill-frame-if-current-buffer-matches)

;; make using frames easier
(set 'gdb-use-separate-io-buffer nil)
(set 'gdb-many-windows nil)
(set 'org-agenda-window-setup 'other-frame)
(set 'org-src-window-setup 'other-frame)
(set 'mouse-autoselect-window nil)
(set 'focus-follows-mouse nil)

;; kill frames when a buffer is buried
(set 'frame-auto-hide-function 'delete-frame)
