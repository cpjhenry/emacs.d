;;; org-macro-display.el --- Display selected LaTeX-style macros in Org buffers -*- lexical-binding: t; -*-

;; Author: cpj
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: org, convenience

;;; commentary:
;; Displays certain literal macro strings (e.g. "\fd{}") as nicer text (e.g. ".....")
;; in Org buffers, without changing the underlying buffer contents.

;; To use:
;;	(require 'org-macro-display)
;;	(add-hook 'org-mode-hook #'org-macro-display-mode)
;;	(add-to-list 'org-macro-displays '("\\eg{}" . "e.g."))

;; Then refresh current buffer:
;;	M-x org-macro-display-refresh

;; Or all open org buffers:
;;	C-u M-x org-macro-display-refresh

;;; code:
(defgroup org-macro-display nil
  "Display selected LaTeX-style macros in Org buffers."
  :group 'org)

(defcustom org-macro-displays
  '(("\\fd{}" . ".....")
    ("\\td{}" . "...")
    ("\\longdash{}" . "---------")
    ("#+LATEX: \\newpage" . "=====")
    ("\\first{}" . "(1st)")
    ("\\second{}" . "(2nd)")
    ("\\third{}" . "(3rd)"))
  "Alist of exact Org/LaTeX macro strings and their display replacements.
Keys are matched literally (exact string match) and may include braces."
  :type '(alist :key-type string :value-type string)
  :group 'org-macro-display)

(defun org-macro-display--keywords ()
  "Return font-lock keywords built from `org-macro-displays`."
  (mapcar
   (lambda (pair)
     (let ((re   (regexp-quote (car pair)))
           (disp (cdr pair)))
       `(,re
         (0 (prog1 nil
              ;; Mark region as ours, and set the display.
              (put-text-property (match-beginning 0) (match-end 0)
                                 'org-macro-display ,disp)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'display ,disp))))))
   org-macro-displays))

(defvar-local org-macro-display--saved-keywords nil
  "Buffer-local copy of the keywords installed by `org-macro-display-mode`.")

(defun org-macro-display--clear ()
  "Remove only display properties added by `org-macro-display-mode`."
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let ((next (or (next-single-property-change pos 'org-macro-display)
                      (point-max))))
        (when (get-text-property pos 'org-macro-display)
          (remove-text-properties pos next
                                  '(display nil org-macro-display nil)))
        (setq pos next)))))

;;;###autoload
(define-minor-mode org-macro-display-mode
  "Display certain LaTeX-style macros as nicer strings, without changing buffer text."
  :lighter " MacDisp"
  (if org-macro-display-mode
      (progn
        (setq org-macro-display--saved-keywords
              (org-macro-display--keywords))
        (font-lock-add-keywords nil org-macro-display--saved-keywords 'append)
        (font-lock-flush))
    (when org-macro-display--saved-keywords
      (font-lock-remove-keywords nil org-macro-display--saved-keywords)
      (setq org-macro-display--saved-keywords nil))
    (org-macro-display--clear)
    (font-lock-flush)))

;;;###autoload
(defun org-macro-display-refresh (&optional all-org-buffers)
  "Refresh `org-macro-display-mode` after changing `org-macro-displays`.

With prefix arg ALL-ORG-BUFFERS, refresh all live Org buffers that have the mode on."
  (interactive "P")
  (if all-org-buffers
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (derived-mode-p 'org-mode)
                     (bound-and-true-p org-macro-display-mode))
            (org-macro-display-mode -1)
            (org-macro-display-mode 1))))
    (when (bound-and-true-p org-macro-display-mode)
      (org-macro-display-mode -1)
      (org-macro-display-mode 1))))

;;;###autoload
(defun org-macro-display-set (key display)
  "Set KEY -> DISPLAY in `org-macro-displays`, replacing any existing KEY."
  (interactive
   (list (read-string "Macro (exact): ")
         (read-string "Display as: ")))
  (setq org-macro-displays (assq-delete-all key org-macro-displays))
  (push (cons key display) org-macro-displays)
  (message "Set org-macro-displays: %s -> %s (run org-macro-display-refresh)" key display))

(provide 'org-macro-display)
;;; org-macro-display.el ends here
