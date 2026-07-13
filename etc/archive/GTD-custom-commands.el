  ;; GTD custom commands
  (org-agenda-custom-commands
   '(("P" "Project List"
      ((tags "PROJECT")))
     ("O" "Office"
      ((agenda)
       (tags-todo "OFFICE")))
     ("W" "Weekly Plan"
      ((agenda)
       (todo "TODO")
       (tags "PROJECT")))
     ("H" "Home NA Lists"
      ((agenda)
       (tags-todo "HOME")
       (tags-todo "COMPUTER")))))
