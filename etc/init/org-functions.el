;;; org-functions.el --- Org functions
;;; commentary:

;;; code:
;; remaps
(defun org-todo-left () (interactive)(org-call-with-arg 'org-todo 'left))
(defun org-todo-right() (interactive)(org-call-with-arg 'org-todo 'right))

;; org links
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

;; other org functions
(defun org-clocking-buffer (&rest _))

(defun org-toggle-iimage-in-org ()
	"display images in your org file"
	(interactive)
	(if (face-underline-p 'org-link)
		(set-face-underline-p 'org-link nil)
		(set-face-underline-p 'org-link t))
	(iimage-mode ‘toggle))

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

;; https://jao.io/blog/eww-to-org.html
(defun jao-eww-to-org (&optional dest)
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

;; https://sachachua.com/blog/2024/11/changing-org-mode-underlines-to-the-html-mark-element/
;(with-eval-after-load 'org (setf (alist-get 'underline org-html-text-markup-alist) "<mark>%s</mark>"))

;; https://emacs.stackexchange.com/questions/5465/how-to-migrate-markdown-files-to-emacs-org-mode-format
(defun markdown-convert-buffer-to-org ()
"Convert the current buffer's content from markdown to orgmode format
and save it with the current buffer's file name but with .org
extension."
	(interactive)
	(shell-command-on-region (point-min) (point-max)
	(format "pandoc -f markdown_mmd -t org -o '%s'"
	(concat (file-name-sans-extension (buffer-file-name)) ".org")))
	(find-file (concat (file-name-sans-extension (buffer-file-name)) ".org")))

;;; org-functions.el ends here
