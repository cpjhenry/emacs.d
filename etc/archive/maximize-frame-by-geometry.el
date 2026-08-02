;; Finder/Emacs Client.app frames can report `fullscreen' as
;; `maximized' without being visually maximized. Resizing to the
;; full monitor workarea almost works, but on macOS the bottom of
;; the frame can clip the minibuffer. Subtracting a few pixels gives
;; the NS frame enough room to display it properly.

(defconst cpj/frame-workarea-height-fudge 4
  "Pixels to subtract from workarea height when resizing macOS frames.")

(defun cpj/maximize-frame-by-geometry (&optional frame)
  "Resize FRAME to fill its monitor workarea."
  (let* ((frame (or frame (selected-frame)))
         (workarea (alist-get 'workarea
			      (frame-monitor-attributes frame))))
    (when (and (frame-live-p frame)
	       (display-graphic-p frame)
	       workarea)
      (let ((left   (nth 0 workarea))
            (top    (nth 1 workarea))
            (width  (nth 2 workarea))
            (height (- (nth 3 workarea)
		       cpj/frame-workarea-height-fudge)))
        (modify-frame-parameters frame '((fullscreen . nil)))
        (set-frame-position frame left top)
        (set-frame-size frame width height t)))))

;; Uncomment hook ↓ ↓ ↓ when creating new frames via an unmodified
;; emacsclient. When calling emacsclient without `-c', this should not
;; be needed. You may call `cpj/maximize-frame-by-geometry' manually.

;; (add-hook 'server-visit-hook
;;           #'cpj/maximize-frame-by-geometry)
