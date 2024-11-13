;; (unless *w32* (use-package pdf-tools
;; 	:load-path  "site-lisp/pdf-tools/lisp"
;; 	:magic ("%PDF" . pdf-view-mode)
;; 	:config (pdf-tools-install :no-query) ))

;; https://jonathanabennett.github.io/blog/2019/05/29/writing-academic-papers-with-org-mode/
(unless *w32* (use-package pdf-tools
   :pin manual ;; manually update
   :config
   ;; initialise
   (pdf-tools-install)
   ;; open pdfs scaled to fit width
   (setq-default pdf-view-display-size 'fit-width)
   ;; use normal isearch
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
   :custom
   (pdf-annot-activate-created-annotations t "automatically annotate highlights")))
