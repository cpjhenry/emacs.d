;;; org-functions.el --- Org functions -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'org)
(require 'org-element)

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

(defun my/org-end-of-subtree ()
  "Move point to the end of the current Org subtree, including invisible text."
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

(defun cpj/org-edit-special-disable-visual-fill-column (&rest _)
  "Disable `visual-fill-column-mode' in Org special edit buffers."
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-mode -1)))

(defun cpj/org-entities-help-outline-cleanup (&rest _)
  "Make `org-entities-help' easier to browse with Outline folding."
  (let ((inhibit-read-only t))
    (flush-blank-lines (point-min) (point-max)))
  (outline-mode)
  (setq-local truncate-lines t)
  (outline-cycle-buffer)
  (view-mode 1))

(defun cpj/org-latex-export-as-latex-cleanup-windows (&rest _)
  "Clean up window layout after `org-latex-export-as-latex'."
  (when (called-interactively-p 'any)
    (delete-other-windows)))

(defun cpj/org-at-table-p-any-advice (oldfun &rest _args)
  "Call OLDFUN as though `org-at-table-p' had been given ANY."
  (funcall oldfun t))

;;; auto-sort after capture (when variable set)
(defun my/org-sort ()
  "Sort all top-level Org entries alphabetically."
  (interactive)
  (save-mark-and-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (push-mark (point-max) nil t)
      (org-sort-entries nil ?a))))

(defvar-local cpj/org-sort-after-capture nil
  "Non-nil means sort this Org buffer after capture finalization.")

(defun cpj/org-sort-capture-target ()
  "Sort and save an opted-in Org capture target."
  (unless org-note-abort
    (when-let* ((marker org-capture-last-stored-marker)
                (buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (when cpj/org-sort-after-capture
          (save-restriction
            (widen)
            (save-excursion
              (my/org-sort)))
          (save-buffer))))))

(add-hook 'org-capture-after-finalize-hook
          #'cpj/org-sort-capture-target)

;;; mine
;;; FIXME add prefix `cpj/'

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

(defun cpj/org-count-books-in-year ()
  "Report the number of books under the enclosing year heading."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (while (> (org-current-level) 1)
      (org-up-heading-safe))
    (let ((book-level (1+ (org-current-level)))
          (count 0))
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) book-level)
           (cl-incf count)))
       nil 'tree)
      (message "%d books" count))))

(defun cpj/org-normalize-top-level-heading-separation ()
  "Ensure exactly one blank line before each top-level Org heading."
  (interactive)
  (org-with-wide-buffer
    (let (headings)
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (push (line-beginning-position) headings))

      ;; Process from the bottom upward so edits do not disturb
      ;; the positions still to be visited.
      (dolist (heading headings)
        (goto-char heading)
        (unless (= (point) (point-min))
          (let ((end (point)))
            (forward-line -1)
            (while (and (not (bobp))
                        (looking-at-p "[[:blank:]]*$"))
              (forward-line -1))
            (forward-line 1)
            (delete-region (point) end)
            (insert "\n")))))))


;;; Org ad hoc code, quick hacks and workarounds
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

;;; org check-boxes
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


;;; Convert to org-mode from other formats
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


;;; Teach org-mode to follow new protocols
(require 'ol)

(org-link-set-parameters "gemini" ; gemini://host/index.gmi
	:follow (lambda (path) (elpher-go (concat "gemini:" path)))
	:face '(:foreground "turquoise" :weight bold)
	:display 'full)

(org-link-set-parameters "gopher"
	:follow (lambda (path) (elpher-go (concat "gopher:" path)))
	:face '(:foreground "blue" :weight bold)
	:display 'full)

;; Support for links to man pages in Org mode
;; https://orgmode.org/manual/Adding-Hyperlink-Types.html
(org-link-set-parameters "man" ; [[man:printf][The printf manual]]
	:follow #'org-man-open
	:export #'org-man-export
	:store #'org-man-store-link)

(defcustom org-man-command 'man
	"The Emacs command to be used to display a man page."
	:group 'org-link
	:type '(choice (const man) (const woman)))

(defun org-man-open (path _)
"Visit the man page on PATH.
PATH should be a topic that can be thrown at the man command."
  (funcall org-man-command path))

(defun org-man-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    ;; This is a man page, we do make this link.
    (let* ((page (org-man-get-page-name))
	   (link (concat "man:" page))
	   (description (format "Man page for %s" page)))
      (org-link-store-props
       :type "man"
       :link link
       :description description))))

(defun org-man-get-page-name ()
  "Extract the page name from the buffer name."
  ;; This works for both `Man-mode' and `woman-mode'.
  (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
      (match-string 1 (buffer-name))
    (error "Cannot create link to this man page")))

(defun org-man-export (link description format _)
  "Export a man page link from Org files."
  (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
	(desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
	;y(t path)
      )))

(provide 'org-functions)
;;; org-functions.el ends here

; LocalWords:  http pandoc mmd metadown metaup Org's
