  ;; tab-bar to cover Mac's notch

  ;; On notched MacBooks, true fullscreen lets Emacs get a little too cosy
  ;; with the camera notch.  Use the tab bar as a harmless strip of padding
  ;; when, and only when, the frame is truly fullscreen (`fullboth').

  ;; This is not meant to be a tab-management interface.  The tab names are
  ;; blank and the buttons are hidden because the tab bar's actual job here
  ;; is to stand between Emacs and Apple's architectural choices.

  (defun cpj/frame-fullscreen-p (&optional frame)
    "Return non-nil if FRAME is truly fullscreen."
    (eq (frame-parameter (or frame (selected-frame)) 'fullscreen)
	'fullboth))

  (defun cpj/tab-bar-visible-when-fullscreen (&optional frame)
    "Show the tab bar on FRAME only when FRAME is fullscreen."
    (let* ((frame (or frame (selected-frame)))
           (lines (if (cpj/frame-fullscreen-p frame) 1 0)))
      (unless (eql (frame-parameter frame 'tab-bar-lines) lines)
	(modify-frame-parameters
	 frame
	 `((tab-bar-lines . ,lines))))))

  (defun cpj/tab-bar-visible-when-fullscreen-after-toggle (&rest _)
    "Update tab bar visibility after toggling fullscreen."
    (let ((frame (selected-frame)))
      (run-at-time
       0.1 nil
       #'cpj/tab-bar-visible-when-fullscreen
       frame)))

  (add-hook 'window-setup-hook
            #'cpj/tab-bar-visible-when-fullscreen)

  (add-hook 'after-make-frame-functions
            #'cpj/tab-bar-visible-when-fullscreen)

  (unless
    (advice-member-p #'cpj/tab-bar-visible-when-fullscreen-after-toggle
                     #'toggle-frame-fullscreen)
    (advice-add 'toggle-frame-fullscreen
		:after #'cpj/tab-bar-visible-when-fullscreen-after-toggle))

  (defun cpj/tab-bar-blank-tab-name ()
    "Return a blank tab name."
    " ")

  (with-eval-after-load 'tab-bar
    (setopt tab-bar-separator ""
            tab-bar-close-button-show nil
            tab-bar-new-button-show nil
            tab-bar-tab-name-function #'cpj/tab-bar-blank-tab-name)

    (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive))
      (set-face-attribute face nil
                          :height 1.05
                          :box nil))))
