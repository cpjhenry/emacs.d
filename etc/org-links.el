;;; org-links.el --- Teach org-mode to follow new protocols
;;; commentary:

;;; code:
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

;;; org-links.el ends here
