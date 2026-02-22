;;; banish-from-M-x.el --- Summary
;;; commentary:

;; Is there a command you want to banish from the #Emacs M-x menu?
;; Here's how you do it for the `nix-flake-update` function, as an example:

;;; code:
(setopt read-extended-command-predicate #'command-completion-default-include-p)
(put 'nix-flake-update 'completion-predicate #'ignore)

;;; banish-from-M-x.el ends here
