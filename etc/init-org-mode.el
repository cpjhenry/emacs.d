;; org-mode

(setq org-todo-keywords
      '((sequence "TODO" "DONE")))
(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "blue" :weight bold)))) ; add inprogress keyword

(setq org-emphasis-alist
  '(("*" bold)
    ("**" bold)
    ("/" italic)
    ("_" italic)
    ("=" (:background "maroon" :foreground "white"))
    ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
    ("+" (:strike-through t)) ))

(require 'ol)

(org-link-set-parameters ; link type: gemini://host/index.gmi
	"gemini"
	:follow (lambda (path) (elpher-go (concat "gemini:" path)))
	:face '(:foreground "turquoise" :weight bold)
	:display 'full)

(org-link-set-parameters ; link type: gopher
	"gopher"
	:follow (lambda (path) (elpher-go (concat "gopher:" path)))
	:face '(:foreground "blue" :weight bold)
	:display 'full)

(org-link-set-parameters "man" ; links to man pages in Org mode
	:follow #'org-man-open
    :export #'org-man-export
    :store #'org-man-store-link)

(defcustom org-man-command 'man
	"The Emacs command to be used to display a man page."
	:group 'org-link
	:type '(choice (const man) (const woman)))

(defun org-man-open (path _)
	"Visit the manpage on PATH.
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

(defun org-toggle-iimage-in-org ()
	"display images in your org file"
	(interactive)
	(if (face-underline-p 'org-link)
		(set-face-underline-p 'org-link nil)
		(set-face-underline-p 'org-link t))
	(iimage-mode ‘toggle))

(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/Documents/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/Documents/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))
