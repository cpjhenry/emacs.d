;; Disable toggle-frame-fullscreen
;; FIXME - Errors with EMACS30 (causes 'ESC' to stop functioning)

(keymap-global-unset "<f11>")
(put 'toggle-frame-fullscreen 'disabled t)
