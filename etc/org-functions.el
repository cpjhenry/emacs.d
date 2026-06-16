;;; org-functions.el --- Org functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)

(defvar cpj/org-agenda-file)
(defun my/agenda ()
  "Load `org-agenda' file."
  (interactive)
  (find-file cpj/org-agenda-file))

(defun my/org-backward-paragraph ()
  "Move backward by Org paragraph and recenter at the top."
  (interactive "^")
  (org-backward-paragraph)
  (recenter-top-bottom 0))

(defun my/org-forward-paragraph ()
"Move forward by Org paragraph and recenter at the top."
  (interactive "^")
  (org-forward-paragraph)
  (recenter-top-bottom 0))

;; make it an interactive command
(defun my/org-end-of-subtree ()
  "Move to the end of the current Org subtree."
  (interactive)
  (org-end-of-subtree t))

(defun cpj/org-emphasize-bold ()
  "Bold current word or active region."
  (interactive)
  (unless (region-active-p)
    (mark-whole-word))
  (org-emphasize ?*))

(defun cpj/org-emphasize-italic ()
  "Italicize current word or active region."
  (interactive)
  (unless (region-active-p)
    (mark-whole-word))
  (org-emphasize ?/))

;; org check-boxes
;; see https://orgmode.org/list/87r5718ytv.fsf@sputnik.localhost
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
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))


;; Org ad hoc code, quick hacks and workarounds
;; https://orgmode.org/worg/org-hacks.html

(defun org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))

(require 'cl-lib)
(require 'lunar)
(with-no-warnings (defvar date))
(defun org-lunar-phases ()
  "Show lunar phase in Agenda buffer."
  (require 'lunar)
  (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
         (phase (cl-find-if (lambda (phase) (equal (car phase) date))
                            phase-list)))
    (when phase
      (concat (lunar-phase-name (nth 2 phase)) " "
              (substring (nth 1 phase) 0 5)))))

(defun org-check-misformatted-subtree ()
  "Check misformatted entries in the current buffer."
  (interactive)
  (outline-show-all)
  (org-map-entries
   (lambda ()
     (when (and (move-beginning-of-line 2)
                (not (looking-at org-heading-regexp)))
       (if (or (and (org-get-scheduled-time (point))
                    (not (looking-at (concat "^.*" org-scheduled-regexp))))
               (and (org-get-deadline-time (point))
                    (not (looking-at (concat "^.*" org-deadline-regexp)))))
           (when (y-or-n-p "Fix this subtree? ")
             (message "Call the function again when you're done fixing this subtree.")
             (recursive-edit))
         (message "All subtrees checked."))))))

(defun org-align-all-tables ()
  "Align all tables in a buffer."
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun org-transpose-paragraphs (arg)
  "Transpose the Org paragraph at point by ARG paragraphs.

This is intended for use from `org-metaup-hook' and `org-metadown-hook',
so that M-up and M-down move ordinary Org paragraphs instead of
transposing lines. After transposition, leave point at the first
non-whitespace character of the moved paragraph.

Do nothing in tables, headings, or list items, leaving Org's usual
behaviour unchanged.

Example:

  (add-to-list \\='org-metaup-hook
               (lambda () (interactive) (org-transpose-paragraphs -1)))
  (add-to-list \\='org-metadown-hook
               (lambda () (interactive) (org-transpose-paragraphs 1)))"
  (interactive "p")
  (when (and (not (or (org-at-table-p) (org-at-heading-p) (org-at-item-p)))
             (thing-at-point 'sentence))
    (transpose-paragraphs arg)
    (backward-paragraph)
    (re-search-forward "[[:graph:]]")
    (goto-char (match-beginning 0))
    t))


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

(require 'browse-url)
(defun org-open-link-at-point-external ()
  "Open the Org link at point in the secondary browser."
  (interactive)
  (let* ((context (org-element-context))
         (url (and (eq (org-element-type context) 'link)
                   (org-element-property :raw-link context))))
    (unless url
      (user-error "No Org link at point"))
    (unless (functionp browse-url-secondary-browser-function)
      (user-error "`browse-url-secondary-browser-function' is not set"))
    (message "Opening externally: %s" url)
    (funcall browse-url-secondary-browser-function url)))


;; Convert to org-mode from other formats
;; https://jao.io/blog/eww-to-org.html
(require 'eww)
(defun eww-to-org (&optional dest)
  "Render the current eww buffer using org markup.
If DEST, a buffer, is provided, insert the markup there."
  (interactive)
  (unless (org-region-active-p)
    (let ((shr-width 80)) (eww-readable)))
  (let* ((start (if (org-region-active-p) (region-beginning) (point-min)))
         (end (if (org-region-active-p) (region-end) (point-max)))
         (buff (or dest (generate-new-buffer "*eww-to-org*")))
         (link (eww-current-url))
         (title (or (plist-get eww-data :title) "")))
    (with-current-buffer buff
      (insert "#+title: " title "\n#+link: " link "\n\n")
      (org-mode))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((p (point))
               (props (text-properties-at p))
               (k (seq-find (lambda (x) (plist-get props x))
                            '(shr-url image-url outline-level face)))
               (prop (and k (list k (plist-get props k))))
               (next (if prop
                         (next-single-property-change p (car prop) nil end)
                       (next-property-change p nil end)))
               (txt (buffer-substring (point) next))
               (txt (replace-regexp-in-string "\\*" "·" txt)))
          (with-current-buffer buff
            (insert
             (pcase prop
               ((and (or `(shr-url ,url) `(image-url ,url))
                     (guard (string-match-p "^http" url)))
                (let ((tt (replace-regexp-in-string "\n\\([^$]\\)" " \\1" txt)))
                  (org-link-make-string url tt)))
               (`(outline-level ,n)
                (concat (make-string (- (* 2 n) 1) ?*) " " txt "\n"))
               ('(face italic) (format "/%s/ " (string-trim txt)))
               ('(face bold) (format "*%s* " (string-trim txt)))
               (_ txt))))
          (goto-char next))))
    (pop-to-buffer buff)
    (goto-char (point-min))))

;; https://emacs.stackexchange.com/questions/5465/how-to-migrate-markdown-files-to-emacs-org-mode-format
(defun markdown-convert-buffer-to-org ()
"Convert the current buffer's content from markdown to `org-mode' format
and save it with the current buffer's file name but with .org
extension."
	(interactive)
	(shell-command-on-region (point-min) (point-max)
	(format "pandoc -f markdown_mmd -t org -o '%s'"
	(concat (file-name-sans-extension (buffer-file-name)) ".org")))
	(find-file (concat (file-name-sans-extension (buffer-file-name)) ".org")))

(provide 'org-functions)
;;; org-functions.el ends here

; LocalWords:  http pandoc mmd metadown metaup Org's
