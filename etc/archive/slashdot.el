(defun /. () "/."
  (interactive)
  (switch-to-buffer "*/.*")
  (shell-command "slashdot-mode" (current-buffer))
  (org-mode)
  (view-mode)
  (if (featurep 'jinx) (jinx-mode -1))
  (goto-char (point-min)))
