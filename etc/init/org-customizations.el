;;; org-customizations --- templates et al.

;;; Commentary:
;;; Code:
(setq	org-tags-exclude-from-inheritance '("PROJECT")
	org-todo-keywords '((sequence "TODO" "DONE"))
	org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold)) )
	org-emphasis-alist '(
	("*" bold)
	("**" bold)
	("/" italic)
	("_" italic)
	("=" (:background "maroon" :foreground "white"))
	("~" (:background "deep sky blue" :foreground "MidnightBlue"))
	("+" (:strike-through t)))

	org-agenda-custom-commands '(
	("P" "Project List"	((tags "PROJECT")))
	("O" "Office"		((agenda)(tags-todo "OFFICE")))
	("W" "Weekly Plan"	((agenda)(todo "TODO")(tags "PROJECT")))
	("H" "Home NA Lists"((agenda)(tags-todo "HOME")(tags-todo "COMPUTER"))))

	org-capture-templates '(
	("c" "Cookbook" entry (file "~/Documents/Recipes/cookbook.org")
	"%(org-chef-get-recipe-from-url)" :empty-lines 1)
	("m" "Manual Cookbook" entry (file "~/Documents/Recipes/cookbook.org")
	"* %^{Recipe title: }\n:PROPERTIES:\n:provenance:\n:source-url:\n:servings:\n:prep-time:\n:cook-time:\n:ready-in:\n:END:\n** Ingredients\n%?\n** Directions\n\n\n** Notes\n\n")

	;; https://benadha.com/notes/how-i-manage-my-reading-list-with-org-mode/
	("i" "📥 Inbox" entry (file "~/Documents/org/inbox.org") "* %?\n  %i\n" :prepend t)
	("j" "📔 Journal" entry (file+datetree "~/Documents/org/journal.org") "* %? %^G\nEntered on %U\n  %i\n")
	("b" "📑 Bookmark" entry (file "~/Documents/org/bookmarks.org") "* %? %^g\n  %i\n" :prepend t)
	("s" "🛒 Shopping List" entry (file+headline "~/Documents/org/shoppinglist.org" "SHOPPING LIST") "* TODO %?\n  %i\n" :prepend t)

	;; https://github.com/rexim/org-cliplink
	("K" "Cliplink capture task" entry (file "") "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)
	))
