;; https://jonathanabennett.github.io/blog/2019/05/29/writing-academic-papers-with-org-mode/
(use-package pdf-tools
	:if	*natasha*
	:custom	(pdf-annot-activate-created-annotations t)
	:bind (	:map pdf-view-mode-map
		("C-s" . isearch-forward)
		("h" . pdf-annot-activate-created-annotations)
		("t" . pdf-annot-add-text-annotation)
		("D" . pdf-annot-delete))
	:mode	(("\\.pdf$" . pdf-view-mode))
	:hook	((pdf-view-mode) . (lambda () (cua-mode 0)))
	:config	(pdf-tools-install)
		(setq-default pdf-view-display-size 'fit-width))
