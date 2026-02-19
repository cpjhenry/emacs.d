;;; org-sundry.el
;;; org-mode surplus routines not currently being used

(defun org-no-ellipsis-in-headlines ()
  "Remove use of ellipsis in headlines. See `buffer-invisibility-spec'."
  (remove-from-invisibility-spec '(outline . t))
  (add-to-invisibility-spec 'outline))

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp
                    (point-min)
                    (point)))
             (end (if globalp
                    (point-max)
                    (if (eq state 'children)
                      (save-excursion
                        (outline-next-heading)
                        (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                       (save-excursion
                         (outline-next-heading)
                           (point)))
                     (msg (format
                            (concat
                              "org-cycle-hide-drawers:  "
                              "`:END:`"
                              " line missing at position %s")
                            (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
                  (outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))

(defun org-compat-adjust-tab-width-in-buffer (old-width)
  "Adjust visual indentation from `tab-width' equal OLD-WIDTH to 8."
  (interactive "nOld `tab-width': ")
  (cl-assert (derived-mode-p 'org-mode))
  (unless (= old-width 8)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (bound
           (repl (if (< old-width 8)
                     (make-string old-width ?\s)
                   (concat "\t" (make-string (- old-width 8) ?\s)))))
       (while (re-search-forward "^ *\t" nil t)
         (skip-chars-forward " \t")
         (setq bound (point-marker))
         (forward-line 0)
         (while (search-forward "\t" bound t)
           (replace-match repl)))))))

;; https://sachachua.com/blog/2024/11/changing-org-mode-underlines-to-the-html-mark-element/
;(with-eval-after-load 'org (setf (alist-get 'underline org-html-text-markup-alist) "<mark>%s</mark>"))

;; org images
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (iimage-mode ‘toggle))

;;; org-sundry.el ends here