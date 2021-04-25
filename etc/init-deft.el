(use-package deft
	:bind (("<f8>" . deft))
	:commands (deft deft-open-file deft-new-file-named)
	:config
	(setq deft-directory "~/notes/"
		  deft-recursive t
		  deft-extensions '("md" "txt" "org" "tex")
		  deft-use-filter-string-for-filename nil
		  deft-use-filename-as-title nil
		  deft-markdown-mode-title-level 1
		  deft-file-naming-rules '((noslash . "-")
							       (nospace . "-")
								   (case-fn . downcase)))
