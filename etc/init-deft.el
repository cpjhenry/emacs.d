; Deft Configuration
; https://jblevins.org/projects/deft/

; Deft is an Emacs mode for quickly browsing, filtering, and editing
; directories of plain text notes, inspired by Notational Velocity. It was
; designed for increased productivity when writing and taking notes by making
; it fast and simple to find the right file at the right time and by automating
; many of the usual tasks such as creating new files and saving files.

(use-package deft
	:bind (("<f8>" . deft))
	:commands (deft deft-open-file deft-new-file-named)
	:config
	(setq deft-directory "~/Documents/Notes/"
		  deft-recursive nil
		  deft-extensions '("md" "markdown" "txt" "text" "org" "tex")
		  deft-use-filter-string-for-filename nil
		  deft-use-filename-as-title t
		  deft-markdown-mode-title-level 1
		  ) )
