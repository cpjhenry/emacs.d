;;; lean-emacs.el --- Small editing and navigation enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; This package collects a number of small editing, navigation,
;; and buffer-management enhancements used throughout my Emacs
;; configuration.
;;
;; The concept of consolidating these utilities into a separate
;; "lean-emacs" module was inspired by an article entitled
;; "Lean Emacs Config" by Vishesh:
;;
;;   https://vishesh.github.io/emacs/editors/2023/01/25/lean-emacs-config.html
;;
;; The original article was no longer available when this file
;; was created and does not appear to have been preserved in the
;; Internet Archive. Any code in this file reflects local
;; modifications and maintenance.

;;; Code:
(defun back-to-indentation-or-beginning-of-line (&optional arg)
  "Move point to indentation, or to bol if already at indentation.

With prefix ARG, operate on the ARGth line forward (like
`move-beginning-of-line')."

  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (forward-line (1- arg)))
  (let ((indent-pos (save-excursion (back-to-indentation) (point))))
    (if (= (point) indent-pos)
        (beginning-of-line)
      (goto-char indent-pos))))

(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun kill-region-or-thing-at-point (beg end)
  "If a region is active kill it, or kill the thing (word/symbol) at point."
  (interactive "r")
  (unless (region-active-p)
    (save-excursion
      (setq beg (re-search-backward "\\_<" nil t))
      (setq end (re-search-forward "\\_>" nil t))))
  (kill-ring-save beg end))

(defun open-line-below ()
  "Start a new line below the current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Start a new line above the current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun fast-file-view-mode ()
  "Make the buffer read-only and disable font-lock and other bells and
whistles for faster viewing."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (font-lock-mode -1))

(defun large-find-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (fast-file-view-mode)))

(provide 'lean-emacs)

;;; lean-emacs.el ends here

; LocalWords:  anzu Vishesh bol ARGth
