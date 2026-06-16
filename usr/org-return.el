;;; org-return.el --- DWIM RET behaviour for Org lists -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org

;;; Commentary:

;; This package provides context-sensitive RET behaviour for Org mode
;; lists and check-boxes while preserving ordinary line insertion at
;; the beginning of list items.
;;
;; It is intended as a lightweight replacement for packages such as
;; `org-autolist', while avoiding advice-based modification of Org
;; internals.
;;
;; Behaviour:
;; - RET at the beginning of a list item inserts a blank line before
;;   the item.

;; - RET within a populated unordered list item continues the list.
;;   When `org-return-split-items' is non-nil, RET in the middle of
;;   an item splits it at point.

;; - RET within a populated checkbox item continues the checklist.
;;   When `org-return-split-items' is non-nil, RET in the middle of
;;   an item splits it at point.

;; - RET within a populated ordered list item increments the list
;;   number.  When `org-return-split-items' is non-nil, RET in the
;;   middle of an item splits it at point.

;; - RET on an empty list item or checkbox removes the list marker
;;   and returns point to the beginning of the line.
;;
;; - In all other contexts, behaviour falls through to `org-return'.
;;
;; The implementation deliberately restricts unordered list continuation
;; to '-' and '+' markers.  The '*' marker is excluded in order to avoid
;; ambiguity with Org headings.
;;
;; The package does not install keybindings automatically.  A typical
;; configuration is:
;;
;;   (define-key org-mode-map
;;               [remap org-return]
;;               #'org-return-dwim)
;;
;; This preserves the semantic meaning of `org-return' while replacing
;; its behaviour with a context-aware variant for list editing.

;;; Code:
(require 'org)

(defgroup org-return nil
  "DWIM RET behaviour for Org lists and check-boxes."
  :group 'org)

(defcustom org-return-split-items t
  "When non-nil, RET in the middle of an Org list item splits it.

If point is at the end of a populated item, RET creates a new item
below.  If point is within the item text, RET splits the item at
point and inserts the appropriate list or check-box prefix.

When nil, RET always creates a new item below the current one."
  :type 'boolean
  :group 'org-return)

(defun org-empty-list-item-p ()
  "Return non-nil if current line is an empty Org list item."
  (save-excursion
    (beginning-of-line)
    (looking-at-p
     "^[[:space:]]*\\(?:[-+]\\|[0-9]+[.)]\\)\\(?:[[:space:]]+\\[[ X-]\\]\\)?[[:space:]]*$")))

(defun org-list-prefix-at-point ()
  "Return Org list prefix data at point, or nil."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Checkbox, unordered.
     ((looking-at "^\\([[:space:]]*[-+][[:space:]]+\\[[ X-]\\][[:space:]]+\\)")
      (list :prefix (match-string 1)))

     ;; Checkbox, ordered.
     ((looking-at "^\\([[:space:]]*\\)\\([0-9]+\\)\\([.)][[:space:]]+\\[[ X-]\\][[:space:]]+\\)")
      (list :prefix
            (format "%s%d%s"
                    (match-string 1)
                    (1+ (string-to-number (match-string 2)))
                    (match-string 3))))

     ;; Plain unordered.
     ((looking-at "^\\([[:space:]]*[-+][[:space:]]+\\)")
      (list :prefix (match-string 1)))

     ;; Plain ordered.
     ((looking-at "^\\([[:space:]]*\\)\\([0-9]+\\)\\([.)][[:space:]]+\\)")
      (list :prefix
            (format "%s%d%s"
                    (match-string 1)
                    (1+ (string-to-number (match-string 2)))
                    (match-string 3))))

     (t nil))))

(defun org-return-dwim ()
  "Smart RET behaviour for Org links, lists, and check-boxes."
  (interactive)
  (cond
   ;; Follow links with RET, then focus the target buffer.
   ((and org-return-follows-link
         (eq 'link (org-element-type (org-element-context))))
    (org-return)
    (delete-other-windows))

   ;; At beginning of a list item, insert a blank line before it.
   ((and (bolp)
         (org-list-prefix-at-point))
    (org-return-and-maybe-indent))

   ;; Empty item: remove list marker.
   ((org-empty-list-item-p)
    (delete-region (line-beginning-position)
                   (min (1+ (line-end-position))
                        (point-max)))
    (beginning-of-line))

   ;; Populated list item.
   ((org-list-prefix-at-point)
    (let ((prefix (plist-get (org-list-prefix-at-point)
                             :prefix)))
      (cond
       ((eolp)
        (newline)
        (insert prefix))

       (org-return-split-items
        (delete-horizontal-space t)
        (newline)
        (insert prefix)
        (when (looking-at-p "[[:space:]]")
          (delete-char 1)))

       (t
        (end-of-line)
        (newline)
        (insert prefix)))))

   ;; Everything else.
   (t
    (org-return))))

(provide 'org-return)
;;; org-return.el ends here
