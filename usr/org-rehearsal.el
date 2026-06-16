;;; org-rehearsal.el --- Estimate rehearsal workload from Org paragraphs -*- lexical-binding: t; -*-

;;; Commentary:

;; This package estimates rehearsal workload for Org-mode texts by
;; counting Org paragraph elements and multiplying that count by a
;; configurable number of rehearsal days per paragraph.
;;
;; The intended workflow is memorization and rehearsal planning rather
;; than delivery timing.  Each paragraph is treated as a manageable
;; rehearsal unit, with the default estimate being two days per paragraph:
;; one day to commit the paragraph to memory, and one day to polish it.
;;
;; By default, the calculation is scoped to the current top-level Org
;; heading, so a longer text can be divided into practical rehearsal
;; sections.  The result is displayed in the mode line as:
;;
;;   ¶14·28d
;;
;; meaning fourteen paragraphs requiring an estimated twenty-eight
;; rehearsal days.
;;
;; Fractional day estimates are allowed.  For example, setting
;; `org-rehearsal-days-per-paragraph' to 2.5 may display:
;;
;;   ¶14·35d
;;   ¶7·17.5d
;;
;; See also `org-monologue-time.el', which estimates delivery duration
;; rather than rehearsal workload.

;; Enable with:
;;
;;   (require 'org-rehearsal)
;;   (add-hook 'org-mode-hook #'org-rehearsal-mode)
;;
;; To enable automatically only for selected directories:
;;
;;   (setq org-rehearsal-auto-enable-directories
;;         (list "~/Documents/org/ritual/"
;;               "~/Documents/org/talks/"))
;;   (add-hook 'org-mode-hook #'org-rehearsal-enable-maybe)

;;; Code:

(require 'org)
(require 'org-element)
(require 'seq)

(defgroup org-rehearsal nil
  "Estimate rehearsal workload from Org paragraph count."
  :group 'org)

(defcustom org-rehearsal-days-per-paragraph 2
  "Estimated rehearsal days per paragraph.

For example, the default value of 2 may represent one day to commit
a paragraph to memory and one day to polish it."
  :type 'number
  :group 'org-rehearsal)

(defcustom org-rehearsal-scope 'top-level
  "Scope to measure.

The value `top-level' means the current level-1 subtree.
The value `subtree' means the current subtree at point.
The value `buffer' means the whole buffer."
  :type '(choice (const :tag "Top-level subtree" top-level)
                 (const :tag "Current subtree" subtree)
                 (const :tag "Whole buffer" buffer))
  :group 'org-rehearsal)

(defcustom org-rehearsal-idle-delay 1.0
  "Seconds to wait after edits before recomputing."
  :type 'number
  :group 'org-rehearsal)

(defcustom org-rehearsal-auto-enable-directories nil
  "List of directories where `org-rehearsal-mode' should auto-enable.

Each entry should be a directory name.  Files in subdirectories also
match."
  :type '(repeat directory)
  :group 'org-rehearsal)

(defvar-local org-rehearsal--modeline ""
  "Cached mode line string for `org-rehearsal-mode'.")

(defvar-local org-rehearsal--idle-timer nil
  "Idle timer used to defer rehearsal recomputation.")

(defconst org-rehearsal--global-mode-string
  '(:eval org-rehearsal--modeline)
  "Mode line construct for `org-rehearsal-mode'.")

(defun org-rehearsal--bounds ()
  "Return cons cell (BEG . END) for the configured rehearsal scope."
  (save-excursion
    (save-restriction
      (widen)
      (pcase org-rehearsal-scope
        ('buffer
         (cons (point-min) (point-max)))
        ((or 'subtree 'top-level)
         (if (org-before-first-heading-p)
             (cons (point-min) (point-max))
           (org-back-to-heading t)
           (when (eq org-rehearsal-scope 'top-level)
             (while (> (org-current-level) 1)
               (org-up-heading-safe)))
           (let ((beg (point)))
             (org-end-of-subtree t t)
             (cons beg (point)))))))))

(defun org-rehearsal--count-paragraphs (beg end)
  "Count Org paragraph elements between BEG and END."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((ast (org-element-parse-buffer)))
        (length (org-element-map ast 'paragraph #'identity))))))

(defun org-rehearsal--format-days (days)
  "Format DAYS cleanly for display.

Whole-number values are displayed without a decimal point.  Fractional
values are displayed to one decimal place."
  (if (= days (truncate days))
      (format "%.0f" days)
    (format "%.1f" days)))

(defun org-rehearsal--recompute ()
  "Recompute cached mode line string."
  (when (derived-mode-p 'org-mode)
    (let* ((bounds (org-rehearsal--bounds))
           (paragraphs
            (org-rehearsal--count-paragraphs
             (car bounds) (cdr bounds)))
           (days (* paragraphs org-rehearsal-days-per-paragraph)))
      (setq org-rehearsal--modeline
            (format " ¶%d·%sd"
                    paragraphs
                    (org-rehearsal--format-days days)))
      (force-mode-line-update))))

(defun org-rehearsal--schedule (&rest _)
  "Schedule rehearsal recomputation after idle time.

This function is suitable for use in hooks such as
`after-change-functions' and `post-command-hook'."
  (when org-rehearsal--idle-timer
    (cancel-timer org-rehearsal--idle-timer))
  (let ((buffer (current-buffer)))
    (setq org-rehearsal--idle-timer
          (run-with-idle-timer
           org-rehearsal-idle-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq org-rehearsal--idle-timer nil)
                 (org-rehearsal--recompute))))))))

(defun org-rehearsal--file-in-directory-p (file directory)
  "Return non-nil if FILE is inside DIRECTORY."
  (let ((dir (file-name-as-directory
              (file-truename (expand-file-name directory))))
        (file (file-truename (expand-file-name file))))
    (string-prefix-p dir file)))

(defun org-rehearsal-enable-maybe ()
  "Enable `org-rehearsal-mode' for configured directories."
  (when-let* ((file (buffer-file-name)))
    (when (seq-some
           (lambda (dir)
             (org-rehearsal--file-in-directory-p file dir))
           org-rehearsal-auto-enable-directories)
      (org-rehearsal-mode 1))))

;;;###autoload
(define-minor-mode org-rehearsal-mode
  "Show paragraph count and estimated rehearsal days in the mode line."
  :lighter ""
  (if org-rehearsal-mode
      (progn
        (unless (member org-rehearsal--global-mode-string
                        global-mode-string)
          (setq global-mode-string
                (append global-mode-string
                        (list org-rehearsal--global-mode-string))))
        (add-hook 'after-save-hook #'org-rehearsal--recompute nil t)
        (add-hook 'after-change-functions #'org-rehearsal--schedule nil t)
        (add-hook 'post-command-hook #'org-rehearsal--schedule nil t)
        (org-rehearsal--recompute))
    ;; Leave the global mode-line construct installed; the displayed value is
    ;; buffer-local and empty when the mode is disabled.
    (remove-hook 'after-save-hook #'org-rehearsal--recompute t)
    (remove-hook 'after-change-functions #'org-rehearsal--schedule t)
    (remove-hook 'post-command-hook #'org-rehearsal--schedule t)
    (when org-rehearsal--idle-timer
      (cancel-timer org-rehearsal--idle-timer)
      (setq org-rehearsal--idle-timer nil))
    (setq org-rehearsal--modeline "")
    (force-mode-line-update t)))

;;;###autoload
(defun org-rehearsal-report ()
  "Report paragraph count and rehearsal days for current scope."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((bounds (org-rehearsal--bounds))
         (paragraphs
          (org-rehearsal--count-paragraphs
           (car bounds) (cdr bounds)))
         (days (* paragraphs org-rehearsal-days-per-paragraph)))
    (message "%d paragraph%s → %s rehearsal day%s"
             paragraphs (if (= paragraphs 1) "" "s")
             (org-rehearsal--format-days days)
             (if (= days 1) "" "s"))))

(provide 'org-rehearsal)
;;; org-rehearsal.el ends here
