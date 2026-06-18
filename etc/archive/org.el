;; Org-mode
(unless (boundp 'org-directory) (setopt org-directory "~/Documents/org/"))
(defvar org-agenda-file (concat org-directory "daily.org") "Default agenda file.")
(setopt org-agenda-files (list org-agenda-file)
	org-default-notes-file (concat org-directory "notes.org")
	org-id-locations-file (concat user-emacs-directory "var/org-id-locations"))
(defvar org-generic-id-locations-file)
(setq org-generic-id-locations-file (expand-file-name "org-generic-id-locations"
				    (expand-file-name "var/" user-emacs-directory)))

(use-package org
  :ensure nil
  :demand t
  :custom
  (org-ctrl-k-protect-subtree t)
  (org-element-use-cache nil)
  (org-ellipsis "·")
  (org-fold-catch-invisible-edits 'smart)
  (org-footnote-auto-adjust t)
  (org-footnote-define-inline t)
  (org-hidden-keywords nil) ;'(title subtitle author date)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native entities))
  (org-list-allow-alphabetical t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-log-done 'time)
  (org-log-repeat nil)
  (org-log-state-notes-into-drawer nil)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-special-ctrl-a/e t)
  (org-startup-folded 'content) ; fold children content all
  (org-startup-indented nil)
  (org-startup-shrink-all-tables t)

  (org-startup-with-inline-images t)
  (org-cycle-inline-images-display t)
  (org-image-actual-width t); '(300)

  (org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (org-use-sub-superscripts '{})

  (org-auto-align-tags nil)
  (org-tags-column 0)

  (org-agenda-include-diary t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-todo-ignore-scheduled t)
  (org-agenda-use-time-grid nil)
  (org-agenda-window-setup 'only-window)

  (org-agenda-custom-commands
   '(("P" "Project List" ((tags "PROJECT")))
     ("O" "Office" ((agenda)(tags-todo "OFFICE")))
     ("W" "Weekly Plan" ((agenda)(todo "TODO")(tags "PROJECT")))
     ("H" "Home NA Lists" ((agenda)(tags-todo "HOME")(tags-todo "COMPUTER")))))

  (org-export-with-author t)
  (org-export-with-broken-links t)
  (org-export-with-date t)
  (org-export-with-section-numbers nil)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts t)
  (org-export-with-tables t)
  (org-export-with-toc nil)
  (org-export-with-timestamps t)

  (org-export-date-timestamp-format "%Y-%m-%d")
  (org-export-time-stamp-file t)

  (org-ascii-text-width 72)
  (org-ascii-inner-margin 2)
  (org-ascii-quote-margin 4)
  (org-ascii-headline-spacing '(0 . 1))

  (org-latex-compiler "xelatex")
  (org-latex-pdf-process (list
    (concat "latexmk -" org-latex-compiler " -recorder -synctex=1 -bibtex-cond %b")))

  (org-md-headline-style 'atx)

  (org-tags-exclude-from-inheritance '("PROJECT"))
  (org-todo-keywords '((sequence "TODO" "DONE")))
  (org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
  (org-emphasis-alist
   '(("*" bold)
     ("**" bold)
     ("/" italic)
     ("_" italic)
     ("=" (:background "maroon" :foreground "white"))
     ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
     ("+" (:strike-through t))))

  (org-capture-templates
   '( ;; https://github.com/sprig/org-capture-extension
     ("p" "Protocol" entry (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n%i\n")
     ("L" "Protocol Link" entry (file+headline org-default-notes-file "Inbox")
      "* %?[[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")))

  :bind ( ("C-c a"  . org-agenda-list)
	  ("C-c k"  . org-capture)
	  ("C-c l"  . org-store-link)
	  :map org-mode-map
	  ([remap backward-paragraph] . my/org-backward-paragraph)
	  ([remap forward-paragraph] . my/org-forward-paragraph)
	  ("S-<return>" . org-open-link-at-point-external)
	  ("M-<f4>"     . org-speed-command-help)
	  ("M-["        . org-backward-heading-same-level)
	  ("M-]"        . org-forward-heading-same-level)
	  ("C-M-["      . org-back-to-top-level-heading) ;outline-up-heading
	  ("C-M-]"      . my/org-end-of-subtree)
	  ("C-c o c"    . org-check-misformatted-subtree)
	  ("C-c o r"    . org-mode-restart)
	  ("C-c o t"    . org-toggle-link-display)
	  :map org-agenda-mode-map
	  ("q"          . org-agenda-exit))

  :config
  (require 'org-tempo) ; C-c C-,
  (require 'org-protocol) ; org-capture
  (load "org-functions" nil 'nomessage)
  (load "org-links" nil 'nomessage)

  (set-face-underline 'org-ellipsis nil)
  (which-key-alias "C-c o" "org")
  (which-key-alias "C-c o c" "misformatted subtree")

  ;; Replace `org-return' with a DWIM variant for list handling.
  ;; Also, incidentally, opens org-links full-window.
  (use-package org-return
    :ensure nil
    :after org
    :bind (:map org-mode-map
		([remap org-return] . org-return-dwim)))

  ;; fix 'org-edit-special'
  (defun cpj/org-edit-special-disable-visual-fill-column (&rest _)
    "Disable `visual-fill-column-mode' in Org special edit buffers."
    (when (bound-and-true-p visual-fill-column-mode)
      (visual-fill-column-mode -1)))
  (advice-add 'org-edit-special :after
              #'cpj/org-edit-special-disable-visual-fill-column)

  ;; org-emphasis
  (keymap-set org-mode-map "A-b" 'org-emphasize-bold)
  (defun org-emphasize-bold ()
    "Bold current word."
    (interactive)
    (unless (region-active-p) (mark-whole-word))
    (org-emphasize ?*))

  (keymap-set org-mode-map "A-i" 'org-emphasize-italic)
  (defun org-emphasize-italic ()
    "Italicize current word."
    (interactive)
    (unless (region-active-p) (mark-whole-word))
    (org-emphasize ?/))

  ;; alternative implementation of 'org-support-shift-select'
  (keymap-set org-mode-map "S-<left>" nil)    ; clear needed keys
  (keymap-set org-mode-map "S-<right>" nil)
  (keymap-set org-mode-map "S-<up>" nil)
  (keymap-set org-mode-map "S-<down>" nil)

  (keymap-set org-mode-map "C-S-<left>" nil)  ; do the same for left-word
  (keymap-set org-mode-map "C-S-<right>" nil) ; and right-word
  (keymap-set org-mode-map "C-S-<up>" nil)    ; and for good measure...
  (keymap-set org-mode-map "C-S-<down>" nil)  ; likewise

  (keymap-set org-mode-map "S-<home>" 'org-shiftleft) ; re-assign shifts
  (keymap-set org-mode-map "S-<end>" 'org-shiftright) ; to more logical keys
  (keymap-set org-mode-map "S-<prior>" 'org-shiftup)
  (keymap-set org-mode-map "S-<next>" 'org-shiftdown)

  ;; fix 'Ctrl-a' binding in org-mode
  (org-remap org-mode-map
             #'back-to-indentation-or-beginning-of-line
             #'org-beginning-of-line)

  ;; tweak behaviour of M-up and M-down
  (add-to-list 'org-metaup-hook
	       (lambda () (interactive) (org-transpose-paragraphs -1)))
  (add-to-list 'org-metadown-hook
	       (lambda () (interactive) (org-transpose-paragraphs 1)))

  ;; (if (featurep 'visual-fill-column)
  ;;     (add-hook 'org-mode-hook 'visual-fill-column-mode--disable))

  ;; primarily for cbc-mode, but also useful for other org files in view-mode
  (with-eval-after-load 'view
    (define-key view-mode-map (kbd "[") 'org-previous-link)
    (define-key view-mode-map (kbd "]") 'org-next-link)
    (define-key view-mode-map (kbd "RET") 'goto-address-at-point))

  ;; Automatically hide Org comment blocks.
  (use-package org-comment-placeholder ; usr/
    :ensure nil
    :hook (org-mode . org-comment-placeholder-mode))

  ;; Hide inline Org footnotes.
  (use-package org-hide-inline-footnotes ; usr/
    :ensure nil
    :hook (org-mode . org-hide-inline-footnotes-mode))

  ;; Visually indent Org quote/verse blocks and prettify delimiters.
  (use-package org-quote-indent ; usr/
    :ensure nil
    :hook (org-mode . org-quote-indent-mode))

  ;; Display certain LaTeX-style macros as nicer strings.
  (use-package org-macro-display ; usr/
    :ensure nil
    :hook (org-mode . org-macro-display-mode))

  ;; Replace org-table characters with box-drawing Unicode glyphs.
  (use-package org-pretty-table ; opt/
    :ensure nil
    :hook   (org-mode . org-pretty-table-mode)
    :config (ignore))

  ;; Report paragraph count and memorization days.
  (use-package org-rehearsal ; usr/
    :ensure nil
    :demand t
    :custom (org-rehearsal-auto-enable-directories
	     (delq nil (list ritual-directory)))
    :bind ( :map org-mode-map
	    ("C-c o m" . org-rehearsal-report))
    :hook (org-mode . org-rehearsal-enable-maybe))

  ;; Create an Org export buffer with paragraphs shortened to LIMIT characters.
  (use-package org-paragraph-preview ; usr/
    :ensure nil
    :demand t
    :custom (org-paragraph-preview-latex-header (concat org-directory "latexhdr.org"))
            (org-paragraph-preview-latex-directives
	     '("\\ritual"
               "\\nopgnos"))
    :bind ( :map org-mode-map
	    ("C-c o p" . org-paragraph-preview)))

  ;; Preview Org LaTeX documents using plain-text exporter.
  (use-package org-plain-latex-preview ; usr/
    :ensure nil)

  ;; Ispell should not check code blocks in org mode
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+begin_example " . "#\\+end_example$"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE " . "#\\+END_EXAMPLE$"))

  ;; custom entities
  (add-to-list 'org-entities-user '("textnumero" "\\textnumero" nil "&numero;" "No." "No." "№"))

  ;; Use Outline mode for `org-entities-help' so Org entities are shown
  ;; literally rather than interpreted, while still allowing heading folding.
  (defun cpj/org-entities-help-outline-cleanup (&rest _)
    "Make `org-entities-help' easier to browse with Outline folding."
    (let ((inhibit-read-only t))
      (flush-blank-lines (point-min) (point-max)))
    (outline-mode)
    (setq-local truncate-lines t)
    (outline-cycle-buffer)
    (view-mode 1))
  (advice-add 'org-entities-help :after
              #'cpj/org-entities-help-outline-cleanup)

  ;; When `org-agenda-include-diary' is enabled, Agenda creates
  ;; a temporary "diary" buffer but does not always register it for
  ;; automatic cleanup. Add it to `org-agenda-new-buffers' so it is
  ;; removed when the Agenda exits.
  (defun cpj/org-agenda-register-diary-buffer ()
    "Register diary buffer for cleanup when Org Agenda exits."
    (when-let* ((buf (get-buffer "diary")))
      (add-to-list 'org-agenda-new-buffers buf)))
  (add-hook 'org-agenda-finalize-hook
            #'cpj/org-agenda-register-diary-buffer)

  (require 'ox-latex)
  (add-to-list 'org-latex-classes '("letter" "\\documentclass{letter}") t)
  (add-to-list 'org-latex-classes '("memoir" "\\documentclass{memoir}"
				    ("\\chapter{%s}" . "\\chapter*{%s}")
				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")
				    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	       			    t)

  (defun cpj/org-latex-export-as-latex-cleanup-windows (&rest _)
    "Clean up window layout after `org-latex-export-as-latex'."
    (when (called-interactively-p 'any)
      (delete-other-windows)))
  (advice-add 'org-latex-export-as-latex :after
              #'cpj/org-latex-export-as-latex-cleanup-windows)

  (require 'ox-md)

  ;; fix org-table-convert-region menu entry
  (define-key org-tbl-menu [Convert\ Region]
	      '(menu-item "Convert Region" org-table-convert-region
                :enable
                (or (org-region-active-p)
                    (not (org-at-table-p 'any)))))

  ;; fix table.el error
  ;; https://github.com/doomemacs/doomemacs/issues/6980
  (defun myfunc/check_table_p (oldfunc) (funcall oldfunc t))
  (advice-add 'org-at-table-p :around 'myfunc/check_table_p)

  ;; org-mode configuration used to end here, but moved to end of
  ;; section so that all incremental functions could re-add their
  ;; pieces, and not hose my org config when running eval.
  ;; FIXME - need to address this.

;; org-mode packages
(use-package org-autoexport ; #+auto_export:
  :disabled
  :after org
  :hook (org-mode . org-autoexport-mode))

(use-package org-chef ; manage recipes in org-mode
  :if *natasha*
  :demand t
  :after org
  :config
  (defvar org-chef-recipe-book "~/Documents/Recipes/cookbook.org" "Default recipe book.")
  (add-to-list 'org-capture-templates '("c" "Cookbook" entry (file org-chef-recipe-book)
					"%(org-chef-get-recipe-from-url)" :empty-lines 0) t)
  ;; HACK · Adjust spacing in `org-chef-recipe-to-org-element' after upgrading ('pre-' to 'post-').
  (add-to-list 'org-capture-templates '("m" "Manual Cookbook" entry (file org-chef-recipe-book)
					"* %^{Recipe title: }
:PROPERTIES:\n:provenance:\n:source-url:\n:servings:\n:prep-time:\n:cook-time:\n:ready-in:\n:END:
** Ingredients%?\n\n** Directions\n\n** Notes\n") t))

(use-package org-cliplink ; insert org-mode links from the clipboard
  :after org
  :bind ( :map org-mode-map
	  ("C-c o k" . my/org-cliplink))
  :config
  (defun my/org-cliplink ()
    "Takes a URL from the clipboard and inserts an org-mode link with the
title of a page found by the URL into the current buffer."
    (interactive)
    (org-cliplink-insert-transformed-title
     (org-cliplink-clipboard-content)     ;take the URL from the CLIPBOARD
     (lambda (url title)
       (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
	      (clean-title (cond
		;; if the host is github.com, cleanup the title
		((string= (url-host parsed-url) "github.com")
		 (replace-regexp-in-string "GitHub - .*: \\(.*\\)" "\\1" title))
		;; otherwise keep the original title
		(t title))))
	 ;; forward the title to the default org-cliplink transformer
	 (org-cliplink-org-mode-link-transformer url clean-title))))))

(use-package org-contrib ; use ':ignore:' tag to exclude heading (but not content) from export
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-download ; org-download-yank
  :if *natasha*
  :after org
  :custom (org-download-heading-lvl nil)
	  (org-download-image-org-width 925))

(use-package org-drill ; aka 'flashcard' mode
  :if *natasha*
  :disabled
  :after org)

;; NOTE: If org-gcal starts syncing archive files, remove the archive
;; files and rebuild org-generic-id-locations.
(use-package org-gcal
  :ensure t
  :custom
  (org-gcal-fetch-file-alist `((,user-gmail . ,org-gcal-file)))
  (org-gcal-notify-p nil)
  (org-gcal-recurring-events-mode 'top-level)
  (plstore-cache-passphrase-for-symmetric-encryption t)
  (org-gcal-remove-api-cancelled-events nil)
  (org-gcal-update-cancelled-events-with-todo nil)
  :config
  (add-to-list 'org-agenda-files org-gcal-file t)

  (defun calfw-gcal ()
  "Fetch Google Calendar events, then display calfw."
  (interactive)
  (deferred:$
    (org-gcal-fetch)
    (deferred:nextc it
      (lambda (_)
        (calfw))))) )

(use-package org-ref ; setup bibliography, cite, ref, and label org-mode links
  :if *natasha*
  :disabled
  :after org
  :init
  (define-key org-mode-map (kbd "C-=") 'org-ref-insert-link-menu))

(use-package ox-epub)
(use-package ox-gemini))

;; END OF ORG-MODE CONFIGURATION
