(use-package calfw
  :after (calendar org)
  :init
  (setq calfw-render-line-breaker
        'calfw-render-line-breaker-none) ; Word wrap.
  :bind (("C-c C" . cpj/calfw)
         :map calfw-calendar-mode-map
         ("q" . quit-window))
  :config
  (use-package calfw-cal
    :demand t)

  (use-package calfw-org
    :demand t)

  (defun cpj/calfw ()
    "Refresh macOS Calendar data and display a calfw calendar."
    (interactive)
    (calendar-data-refresh-if-stale)
    (let ((cfw-buf
           (calfw-open-calendar-buffer
            :contents-sources
            (list
             (calfw-cal-create-source "diary" "orange")
             (calfw-org-create-source nil "org-agenda" "green"))
            :view 'two-weeks)))
      (with-current-buffer cfw-buf
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (kill-unmodified-file-buffer cpj/org-agenda-file)
           (kill-unmodified-file-buffer cpj/calendar-data-file))
         nil t)))))
