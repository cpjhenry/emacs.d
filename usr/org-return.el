;;; org-return.el --- DWIM RET behaviour for Org lists -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides context-sensitive RET behaviour for Org mode
;; lists and check-boxes.
;;
;; It is intended as a lightweight replacement for packages such as
;; `org-autolist', while avoiding advice-based modification of Org
;; internals.
;;
;; Behaviour:
;;
;; - RET on a populated unordered list item continues the list.
;;
;;     - Item
;;       RET →
;;     - Item
;;     -
;;
;; - RET on a populated checkbox item continues the checklist.
;;
;;     - [ ] Task
;;       RET →
;;     - [ ] Task
;;     - [ ]
;;
;; - RET on a populated ordered list item increments the list number.
;;
;;     1. Item
;;        RET →
;;     1. Item
;;     2.
;;
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
  "Smart RET behaviour for Org lists and check-boxes."
  (interactive)
  (cond
   ((org-empty-list-item-p)
    (delete-region (line-beginning-position)
                   (min (1+ (line-end-position)) (point-max)))
    (beginning-of-line))

((org-list-prefix-at-point)
 (let ((prefix (plist-get (org-list-prefix-at-point) :prefix)))
   (end-of-line)
   (newline)
   (insert prefix)))

   (t (org-return))))

(provide 'org-return)
;;; org-return.el ends here
