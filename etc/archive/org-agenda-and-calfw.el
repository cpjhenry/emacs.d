;;; calfw
;; Visual calendar dashboard. This section wires together calfw,
;; diary, Org agenda sources, and `org-gcal'. Google Calendar events
;; are synced into an Org file, then displayed through both
;; `org-agenda' and calfw.

;; macOS calendar access is granted to a specific Emacs application
;; bundle. After installing, replacing, or moving Emacs.app, run:
;;
;;     patch-emacs-calendar-permission
;;
;; For example:
;;
;;     patch-emacs-calendar-permission /usr/local/opt/emacs-plus\@31/Emacs.app
;;
;; This restores Mac Calendar access used by `maccalfw’.

(defvar cpj/org-agenda-file
  (expand-file-name "daily.org" org-directory)
  "Default Org agenda file.")

;; Keep this outside `use-package' because other packages, such as
;; `org-gcal', add generated files to `org-agenda-files' later.
(setopt org-agenda-files (list cpj/org-agenda-file))

(use-package org-agenda
  :ensure nil
  :after  org
  :bind ( ("C-c a" . org-agenda-list)
	 :map org-agenda-mode-map
	 ("q"      . org-agenda-exit))
  :hook (org-agenda-finalize . cpj/org-agenda-register-diary-buffer)
        (org-agenda-mode . hl-line-mode)

  :custom
  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-additional-timestamps-same-entry t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday 1)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-time-leading-zero t)
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-use-time-grid nil)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("P" "Project List"
      ((tags "PROJECT")))
     ("O" "Office"
      ((agenda)
       (tags-todo "OFFICE")))
     ("W" "Weekly Plan"
      ((agenda)
       (todo "TODO")
       (tags "PROJECT")))
     ("H" "Home NA Lists"
      ((agenda)
       (tags-todo "HOME")
       (tags-todo "COMPUTER")))))

  (org-agenda-prefix-format
   '((agenda . " %i %-12:c%?-12t")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c")))

  :config
  ;; normalize face for previously scheduled items
  (set-face-attribute 'org-scheduled-previously nil
                      :inherit nil
                      :foreground (face-foreground 'default nil t)
                      :background (face-background 'default nil t)
                      :weight 'normal)

  (defun cpj/org-agenda-register-diary-buffer ()
    "Register diary buffer for cleanup when Org Agenda exits."
    (when-let* ((buf (get-buffer "diary")))
      (add-to-list 'org-agenda-new-buffers buf))))

(use-package calendar-data
  :ensure nil
  :commands calendar-data-refresh
  :custom
  (calendar-data-file
   (expand-file-name "calendar-data.org" org-directory))
  (calendar-data-calendar-names
   '("Family"
     "Birthdays"
     "Ottawa District 1"
     "Home"
     "Goodwood 159 Public Calendar"
     "cpjhenry@gmail.com"))
  (calendar-data-past-days 30)
  (calendar-data-future-days 365))

(use-package calfw
  :after (calendar org)
  :demand t
  :init (setq calfw-render-line-breaker
	      'calfw-render-line-breaker-none) ; wordwrap
  :bind ( :map calfw-calendar-mode-map
	  ("q" . quit-window))
  :config
  (use-package calfw-cal
    :demand t)
  (use-package calfw-ical
    :demand t)
  (use-package calfw-org
    :demand t)

  (defun cpj/calfw ()
    "Display a calfw calendar buffer; clean up on exit."
    (interactive)
    (let ((cfw-buf
           (calfw-open-calendar-buffer
            :contents-sources
            (append
             (list
              (calfw-cal-create-source "diary" "orange")
              (calfw-org-create-source nil "org-agenda" "green"))
             (cpj/maccalfw-create-sources '("Birthdays")))
            :view 'two-weeks)))
      (with-current-buffer cfw-buf
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (kill-unmodified-file-buffer cpj/org-agenda-file)
           (kill-unmodified-file-buffer org-gcal-file)
           (kill-unmodified-file-buffer (concat (expand-file-name org-gcal-file) "_archive")))
         nil t)))))

(use-package maccalfw
  :after calfw
  :demand t
  :commands (maccalfw-open
             maccalfw-get-calendars
             maccalfw-get-calendars-by-name)
  :config
  (defun cpj/maccalfw-create-sources (&optional calendar-names)
    "Return calfw sources for macOS Calendar."
    (when (require 'maccalfw nil t)
      (condition-case err
          (progn
            (maccalfw--load-module)
            (let ((calendars (if calendar-names
                                 (maccalfw-get-calendars-by-name calendar-names)
                               (maccalfw-get-calendars))))
              (mapcar
               (lambda (cal)
                 (maccalfw--create-source
                  (plist-get cal :title)
                  (plist-get cal :id)
                  (plist-get cal :color)))
               calendars)))
        (error
         (message "maccalfw unavailable: %s" (error-message-string err))
         nil)))))
