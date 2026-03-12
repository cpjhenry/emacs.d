;;; org-functions.el --- Org functions
;;; commentary:

;;; code:
(require 'org)

;; org check-boxes
;; see https://orgmode.org/list/87r5718ytv.fsf@sputnik.localhost
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))
(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
	  (if (match-end 1)
	      (if (equal (match-string 1) "100%")
		  ;; all done - do the state change
		  (org-todo 'done)
		(org-todo 'todo))
	    (if (and (> (match-end 2) (match-beginning 2))
		     (equal (match-string 2) (match-string 3)))
		(org-todo 'done)
	      (org-todo 'todo)))))))

;; Sage/cpj
(defun org-hide-comment-blocks ()
  "Hide all #+begin_comment blocks in the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_comment\\b" nil t)
        (goto-char (match-beginning 0))     ; must be on the #+begin line
        (org-hide-block-toggle t)           ; hide this block
        (forward-line 1)))))

;; https://orgmode.org/worg/org-hacks.html
(require 'cl-lib)
(with-no-warnings (defvar date))
(defun org-lunar-phases ()
  "Show lunar phase in Agenda buffer."
  (require 'lunar)
  (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
         (phase (cl-find-if (lambda (phase) (equal (car phase) date))
                            phase-list)))
    (when phase
      (setq ret (concat (lunar-phase-name (nth 2 phase)) " "
                        (substring (nth 1 phase) 0 5))))))

;; Sage/cpj
(require 'org-element)
(defun org-count-paragraphs-in-region (beg end)
  "Count Org \='paragraph' elements between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ast (org-element-parse-buffer)))
        (length (org-element-map ast 'paragraph #'identity))))))

(defun org-count-paragraphs ()
  "Report number of Org paragraphs in the current top-level heading.

If point is not in a heading, count in the whole buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (save-excursion
    (save-restriction
      (widen)
      (let (beg end scope-label)
        (if (org-before-first-heading-p)
            (setq beg (point-min)
                  end (point-max)
                  scope-label "Buffer")
          ;; Find current top-level (level 1) ancestor, then narrow to its subtree.
          (org-back-to-heading t)
          (while (> (org-current-level) 1)
            (org-up-heading-safe))
          (setq scope-label (format "%s" (org-get-heading t t t t)))
          (setq beg (point))
          (org-end-of-subtree t t)
          (setq end (point)))
        (let ((n (org-count-paragraphs-in-region beg end)))
          (message "%s has %d paragraph%s" scope-label n (if (= n 1) "" "s")))))))

(provide 'org-functions)
;;; org-functions.el ends here
