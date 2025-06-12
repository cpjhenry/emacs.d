;;; org-tab-key.el --- The TAB Key in Org Mode, Re-imagined
;;; https://spepo.github.io/2025-03-29-the-tab-key-in-org-mode-reimagined.html

;;; commentary:

;;; code:
(defun /org-next-visible-heading-or-link (&optional arg)
  "Move to the next visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the next link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
  (interactive "P")
  (let ((next-heading (save-excursion
                        (org-next-visible-heading 1)
                        (when (org-at-heading-p) (point))))
        (next-link (save-excursion
                     (when (/org-next-visible-link) (point)))))
    (when arg
      (if (and (org-at-heading-p) next-link)
          (setq next-heading nil)
        (if (and (looking-at org-link-any-re) next-heading)
            (setq next-link nil))))
    (cond
     ((and next-heading next-link) (goto-char (min next-heading next-link)))
     (next-heading (goto-char next-heading))
     (next-link (goto-char next-link)))))

(defun /org-previous-visible-heading-or-link (&optional arg)
  "Move to the previous visible heading or link, whichever comes first.
With prefix ARG and the point on a heading(link): jump over subsequent
headings(links) to the previous link(heading), respectively.  This is useful
to skip over a long series of consecutive headings(links)."
  (interactive "P")
  (let ((prev-heading (save-excursion
                        (org-previous-visible-heading 1)
                        (when (org-at-heading-p) (point))))
        (prev-link (save-excursion
                     (when (/org-next-visible-link t) (point)))))
    (when arg
      (if (and (org-at-heading-p) prev-link)
          (setq prev-heading nil)
        (if (and (looking-at org-link-any-re) prev-heading)
            (setq prev-link nil))))
    (cond
     ((and prev-heading prev-link) (goto-char (max prev-heading prev-link)))
     (prev-heading (goto-char prev-heading))
     (prev-link (goto-char prev-link)))))

;; Adapted from org-next-link to only consider visible links
(defun /org-next-visible-link (&optional search-backward)
  "Move forward to the next visible link.
When SEARCH-BACKWARD is non-nil, move backward."
  (interactive)
  (let ((pos (point))
        (search-fun (if search-backward #'re-search-backward
                      #'re-search-forward)))
    ;; Tweak initial position: make sure we do not match current link.
    (cond
     ((and (not search-backward) (looking-at org-link-any-re))
      (goto-char (match-end 0)))
     (search-backward
      (pcase (org-in-regexp org-link-any-re nil t)
        (`(,beg . ,_) (goto-char beg)))))
    (catch :found
      (while (funcall search-fun org-link-any-re nil t)
        (let ((folded (org-invisible-p nil t)))
          (when (or (not folded) (eq folded 'org-link))
            (let ((context (save-excursion
                             (unless search-backward (forward-char -1))
                             (org-element-context))))
              (pcase (org-element-lineage context '(link) t)
                (link
                 (goto-char (org-element-property :begin link))
                 (throw :found t)))))))
      (goto-char pos)
      ;; No further link found
      nil)))

(defun /org-shifttab (&optional arg)
  "Move to the previous visible heading or link.
If already at a heading, move first to its beginning.  When inside a table,
move to the previous field."
  (interactive "P")
  (cond
   ((org-at-table-p) (call-interactively #'org-table-previous-field))
   ((and (not (bolp)) (org-at-heading-p)) (beginning-of-line))
   (t (call-interactively #'/org-previous-visible-heading-or-link))))

(defun /org-tab (&optional arg)
  "Move to the next visible heading or link.
When inside a table, re-align the table and move to the next field."
  (interactive)
  (cond
   ((org-at-table-p) (org-table-justify-field-maybe)
    (call-interactively #'org-table-next-field))
   (t (call-interactively #'/org-next-visible-heading-or-link))))

(use-package org
  :config
  (define-key org-mode-map (kbd "<tab>") #'/org-tab)
  (define-key org-mode-map (kbd "<backtab>") #'/org-shifttab)

  ;; Customize speed keys: modifying operations must be upper case
  (custom-set-variables
   '(org-speed-commands
     '(("Outline Navigation and Visibility")
       ("n" . (org-speed-move-safe 'org-next-visible-heading))
       ("p" . (org-speed-move-safe 'org-previous-visible-heading))
       ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
       ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
       ("u" . (org-speed-move-safe 'outline-up-heading))
       ("j" . org-goto)
       ("c" . org-cycle)
       ("C" . org-shifttab)
       (" " . org-display-outline-path)
       ("s" . org-toggle-narrow-to-subtree)
       ("Editing")
       ("I" . (progn (forward-char 1) (call-interactively 'org-insert-heading-respect-content)))
       ("^" . org-sort)
       ("W" . org-refile)
       ("@" . org-mark-subtree)
       ("T" . org-todo)
       (":" . org-set-tags-command)
       ("Misc")
       ("?" . org-speed-command-help)))))

;;; org-tab-key.el ends here
