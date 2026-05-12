;;; org-monologue-memo.el --- Memorization time from Org paragraphs -*- lexical-binding: t; -*-

;;; Commentary:

;; This package estimates the memorization workload for an Org-mode
;; monologue by counting Org paragraph elements and multiplying that count
;; by a configurable number of days per paragraph.
;;
;; The intended workflow is not delivery timing, but memorization planning:
;; each paragraph is treated as a digestible unit, with the default estimate
;; being two days per paragraph -- one day to commit the paragraph to memory,
;; and one day to polish it.
;;
;; By default, the calculation is scoped to the current top-level Org
;; heading, so a longer monologue can be divided into practical rehearsal
;; sections.  The result is displayed in the mode line as:
;;
;;   ¶14~28d
;;
;; meaning fourteen paragraphs, estimated at twenty-eight days to memorize.
;;
;; See also `org-monologue-time.el', which uses the same basic paragraph
;; counting idea for delivery-time estimates rather than memorization
;; planning.

;; Enable with:
;;
;;   (require 'org-monologue-memo)
;;   (add-hook 'org-mode-hook #'org-monologue-memo-mode)

;; To enable automatically only for selected directories:
;;
;;   (setq org-monologue-memo-auto-enable-directories
;;         (list "~/Documents/org/ritual/"
;;               "~/Documents/org/talks/"))
;;   (add-hook 'org-mode-hook #'org-monologue-memo-enable-maybe)

;;; Code:
(require 'org)
(require 'org-element)
(require 'seq)

(defgroup org-monologue-memo nil
  "Estimate memorization time from paragraph count."
  :group 'org)

(defcustom org-monologue-memo-days-per-paragraph 2
  "Estimated days per paragraph (e.g., 1 day to commit + 1 day to polish)."
  :type 'number)

(defcustom org-monologue-memo-scope 'top-level
  "Scope to measure.
- `top-level` = current level-1 subtree
- `subtree`   = current subtree at point
- `buffer`    = whole buffer"
  :type '(choice (const :tag "Top-level subtree" top-level)
                 (const :tag "Current subtree" subtree)
                 (const :tag "Whole buffer" buffer)))

(defcustom org-monologue-memo-idle-delay 1.0
  "Seconds to wait after edits before recomputing."
  :type 'number)

(defvar-local org-monologue-memo--modeline ""
  "Cached modeline string.")

(defvar-local org-monologue-memo--idle-timer nil)

(defconst org-monologue-memo--global-mode-string
  '(:eval org-monologue-memo--modeline)
  "Mode line construct for `org-monologue-memo-mode'.")

(defvar org-monologue-memo-auto-enable-directories nil
  "List of directories where `org-monologue-memo-mode' should auto-enable.

Each entry should be a directory name.  Files in subdirectories also match.
For example:

  (setq org-monologue-memo-auto-enable-directories
        (list \"~/Documents/org/ritual/\"
              \"~/Documents/org/presentations/\"))")

(defun org-monologue-memo--bounds ()
  "Return (BEG . END) for the configured scope."
  (save-excursion
    (save-restriction
      (widen)
      (pcase org-monologue-memo-scope
        ('buffer (cons (point-min) (point-max)))
        ((or 'subtree 'top-level)
         (if (org-before-first-heading-p)
             (cons (point-min) (point-max))
           (org-back-to-heading t)
           (when (eq org-monologue-memo-scope 'top-level)
             (while (> (org-current-level) 1)
               (org-up-heading-safe)))
           (let ((beg (point)))
             (org-end-of-subtree t t)
             (cons beg (point)))))))))

(defun org-monologue-memo--count-paragraphs (beg end)
  "Count Org paragraph elements between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ast (org-element-parse-buffer)))
        (length (org-element-map ast 'paragraph #'identity))))))

(defun org-monologue-memo--recompute ()
  "Recompute cached modeline string."
  (when (derived-mode-p 'org-mode)
    (let* ((b (org-monologue-memo--bounds))
           (p (org-monologue-memo--count-paragraphs (car b) (cdr b)))
           (days (* p org-monologue-memo-days-per-paragraph)))
      (setq org-monologue-memo--modeline
            (format " ¶%d~%dd" p days))
      (force-mode-line-update))))

(defun org-monologue-memo--schedule (&rest _)
  "Schedule recompute after idle (for use in hooks)."
  (when org-monologue-memo--idle-timer
    (cancel-timer org-monologue-memo--idle-timer))
  (let ((buffer (current-buffer)))
    (setq org-monologue-memo--idle-timer
          (run-with-idle-timer
           org-monologue-memo-idle-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq org-monologue-memo--idle-timer nil)
                 (org-monologue-memo--recompute))))))))

(defun org-monologue-memo--file-in-directory-p (file directory)
  "Return non-nil if FILE is inside DIRECTORY."
  (let ((dir (file-name-as-directory
              (file-truename (expand-file-name directory))))
        (file (file-truename (expand-file-name file))))
    (string-prefix-p dir file)))

(defun org-monologue-memo-enable-maybe ()
  "Enable `org-monologue-memo-mode' for configured directories."
  (when-let ((file (buffer-file-name)))
    (when (seq-some
           (lambda (dir)
             (org-monologue-memo--file-in-directory-p file dir))
           org-monologue-memo-auto-enable-directories)
      (org-monologue-memo-mode 1))))

;;;###autoload
(define-minor-mode org-monologue-memo-mode
  "Show paragraph count and estimated memorization days in the mode line."
  :lighter ""
  (if org-monologue-memo-mode
      (progn
        (unless (member org-monologue-memo--global-mode-string global-mode-string)
          (setq global-mode-string
                (append global-mode-string
                        (list org-monologue-memo--global-mode-string))))
        (add-hook 'after-save-hook #'org-monologue-memo--recompute nil t)
        (add-hook 'after-change-functions #'org-monologue-memo--schedule nil t)
        (add-hook 'post-command-hook #'org-monologue-memo--schedule nil t)
        (org-monologue-memo--recompute))
    ;; Leave the global mode-line construct installed; the displayed value is
    ;; buffer-local and empty when the mode is disabled.
    (remove-hook 'after-save-hook #'org-monologue-memo--recompute t)
    (remove-hook 'after-change-functions #'org-monologue-memo--schedule t)
    (remove-hook 'post-command-hook #'org-monologue-memo--schedule t)
    (when org-monologue-memo--idle-timer
      (cancel-timer org-monologue-memo--idle-timer)
      (setq org-monologue-memo--idle-timer nil))
    (setq org-monologue-memo--modeline "")
    (force-mode-line-update t)))

(defun org-monologue-memo-report ()
  "Report paragraph count and memorization days for current scope."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((b (org-monologue-memo--bounds))
         (p (org-monologue-memo--count-paragraphs (car b) (cdr b)))
         (days (* p org-monologue-memo-days-per-paragraph)))
    (message "%d paragraph%s → %d day%s to memorize"
             p (if (= p 1) "" "s")
             days (if (= days 1) "" "s"))))

(provide 'org-monologue-memo)
;;; org-monologue-memo.el ends here
