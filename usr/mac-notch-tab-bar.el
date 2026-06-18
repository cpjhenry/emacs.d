;;; mac-notch-tab-bar.el --- Use tab bar as notch padding on macOS -*- lexical-binding: t; -*-

;;; Commentary:

;; On notched MacBooks, true fullscreen lets Emacs get a little too cosy
;; with the camera notch.  This minor mode uses the tab bar as a harmless
;; strip of padding when, and only when, the selected frame is truly
;; fullscreen (`fullboth').
;;
;; This is not meant to be a tab-management interface.  Tab names are blank
;; and the close/new buttons are hidden because the tab bar's actual job
;; here is to stand between Emacs and Apple's architectural choices.

;;; Code:

(require 'tab-bar)

(defgroup mac-notch-tab-bar nil
  "Use the tab bar as notch padding on macOS fullscreen frames."
  :group 'frames)

(defcustom mac-notch-tab-bar-height 1.05
  "Height of tab-bar faces used for notch padding."
  :type 'number
  :group 'mac-notch-tab-bar)

(defun mac-notch-tab-bar--fullscreen-p (&optional frame)
  "Return non-nil if FRAME is truly fullscreen."
  (eq (frame-parameter (or frame (selected-frame)) 'fullscreen)
      'fullboth))

(defun mac-notch-tab-bar--blank-tab-name ()
  "Return a blank tab name."
  " ")

(defun mac-notch-tab-bar--update (&optional frame)
  "Show the tab bar on FRAME only when FRAME is fullscreen."
  (let* ((frame (or frame (selected-frame)))
         (lines (if (mac-notch-tab-bar--fullscreen-p frame) 1 0)))
    (unless (eql (frame-parameter frame 'tab-bar-lines) lines)
      (modify-frame-parameters frame
                               `((tab-bar-lines . ,lines))))))

(defun mac-notch-tab-bar--after-toggle (&rest _)
  "Update tab-bar visibility after toggling fullscreen."
  (let ((frame (selected-frame)))
    (run-at-time 0.1 nil
                 #'mac-notch-tab-bar--update
                 frame)))

(defun mac-notch-tab-bar--configure ()
  "Configure the tab bar for notch-padding use."
  (setopt tab-bar-separator ""
          tab-bar-close-button-show nil
          tab-bar-new-button-show nil
          tab-bar-tab-name-function
          #'mac-notch-tab-bar--blank-tab-name)

  (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive))
    (set-face-attribute face nil
                        :height mac-notch-tab-bar-height
                        :box nil)))

;;;###autoload
(define-minor-mode mac-notch-tab-bar-mode
  "Use the tab bar as notch padding in macOS fullscreen frames."
  :global t
  :group 'mac-notch-tab-bar
  (if mac-notch-tab-bar-mode
      (progn
        (mac-notch-tab-bar--configure)

        (add-hook 'window-setup-hook
                  #'mac-notch-tab-bar--update)

        (add-hook 'after-make-frame-functions
                  #'mac-notch-tab-bar--update)

        (unless (advice-member-p #'mac-notch-tab-bar--after-toggle
                                 #'toggle-frame-fullscreen)
          (advice-add 'toggle-frame-fullscreen
                      :after #'mac-notch-tab-bar--after-toggle))

        (mac-notch-tab-bar--update))
    (remove-hook 'window-setup-hook
                 #'mac-notch-tab-bar--update)

    (remove-hook 'after-make-frame-functions
                 #'mac-notch-tab-bar--update)

    (advice-remove 'toggle-frame-fullscreen
                   #'mac-notch-tab-bar--after-toggle)

    ;; Leave the user's tab-bar faces/options alone on disable, but hide the
    ;; padding line in the selected frame.
    (modify-frame-parameters (selected-frame)
                             '((tab-bar-lines . 0)))))

(provide 'mac-notch-tab-bar)

;;; mac-notch-tab-bar.el ends here
