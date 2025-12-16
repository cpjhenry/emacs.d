;;; filesandbuffers.el --- FILE/BUFFER functions

;;; Commentary:

;;; Code:

(if (< emacs-major-version 29)(defun scratch-buffer ()
"Switch to the *scratch* buffer.
	If the buffer doesn't exist, create it first."
	(interactive)
	(pop-to-buffer-same-window (get-scratch-buffer-create))))

(defun new-empty-buffer ()
"Create new empty buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled")))
		(switch-to-buffer buf)
		(funcall (and default-major-mode))
		(setq buffer-offer-save t) ))

(defun new-markdown-buffer ()
"Create new empty `markdown-mode' buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.md")))
		(switch-to-buffer buf)
		(markdown-mode)
		(setq buffer-offer-save t) ))

(defun new-org-buffer ()
"Create new empty `org-mode' buffer."
	(interactive)
	(let ((buf (generate-new-buffer "untitled\.org")))
		(switch-to-buffer buf)
		(org-mode)
		(setq buffer-offer-save t) ))

(defun copy-current-to-temp-buffer ()
  "Copy the current buffer or region, create temp buffer, paste it there."
  (interactive)
  (let ((beg (point-min)) (end (point-max)))
    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))

    (kill-ring-save beg end)
    (switch-to-buffer (make-temp-name ""))
    (yank))

  ;; clean up section markers, so text folds correctly.
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward "" nil t) ; group separator
	(replace-match "")))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (kill-dired-buffers))

(defun quit-window-kill ()
  "Kill when quitting."
  (interactive)
  (quit-window t))

(defun shell-command-on-buffer ()
"Asks for a command and execute it in inferior shell with current buffer as input."
	(interactive)
	(shell-command-on-region (point-min) (point-max)
	(read-shell-command "Shell command on buffer: ")))

;; http://xahlee.info/emacs/emacs/emacs_auto_save.html
(defun save-all-unsaved ()
"Save all unsaved files. No ask."
	(interactive)
	(save-some-buffers t))

;; https://emacs.stackexchange.com/questions/3116/how-to-display-a-message-in-echo-area-only
(defun echo-and-ignore-message-buffer (message)
	(let ((prev-msg-log-max message-log-max))
	(unwind-protect (progn (setq message-log-max nil)
		(message message))
	(setq message-log-max prev-msg-log-max))))

(defun shell-other-frame ()
  "Open a `shell' in a new frame."
  (interactive)
  (let ((default-directory "~")
	(buf (shell)))
    (switch-to-buffer-other-frame buf)))

(defun buf-to-LF()
	(interactive)
	(set-buffer-file-coding-system 'utf-8-unix)
	(set-buffer-modified-p nil))


;; modeline functions
;; https://jiewawa.me/2024/10/useful-emacs-commands-for-reading/
(defun kill-modeline ()
	(setq-local mode-line-format nil))

(defun restore-modeline ()
	(kill-local-variable 'mode-line-format))

(defun toggle-modeline () "Toggle modeline."
	(interactive)
	(if (null mode-line-format) (restore-modeline)
	(kill-modeline)))


;; macOS frame functions
(defun ns-raise-emacs ()
"Raise Emacs."
	(ns-do-applescript "tell application \"Emacs\" to activate"))

(defun ns-raise-emacs-with-frame (frame)
"Raise Emacs and select the provided frame."
	(with-selected-frame frame
	(when (display-graphic-p)
		(ns-raise-emacs)
		(toggle-frame-maximized))))


;; misc. functions

(defun set-window-width (n)
"Set the selected window's width as 'N'."
	(adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

;; FIXME doesn't work. Perhaps set to 0?
(defun set-full-frame () "Set the selected window to full-frame."
	(interactive)
	(set-window-width (window-width)))

(defun set-80-columns ()
"Set the selected window to 80 columns."
	(interactive)
	(set-window-width 80))

(defun toggle-fill-column ()
"Toggle `fill-column' values between 32 and 70."
    (interactive)
    (setq fill-column (if (= fill-column 70) 32 70))
	;; three values: (setq fill-column (if (= fill-column 8) 4 (if (= fill-column 4) 2 8)))
	(message "'fill-column' set to: %s" fill-column))

(defun fill-max-column ()
"Set `fill-column' to maximum width."
	(interactive)
	(setq fill-column 0)
	(message "Maximum width."))

(defun toggle-fill-column-center ()
  "Toggle fill-column-center when in visual-fill-column-mode."
  (interactive)
  (if (bound-and-true-p visual-fill-column-mode) (progn
     (if (bound-and-true-p visual-fill-column-center-text)
	(progn	(setq visual-fill-column-center-text nil)
		(visual-fill-column-mode -1))
		(progn	(setq visual-fill-column-center-text t)))
		(visual-fill-column-adjust))
	(message "`visual-fill-column-mode' not enabled.")))

;; See: https://emacs.stackexchange.com/questions/81361/how-to-switch-to-a-buffer-from-terminal-with-a-unique-partial-name
(defun switch-to-buffer-matching (regular-expression)
  "Switch to the first buffer that matches REGULAR-EXPRESSION."
	(switch-to-buffer (-find (-compose (-partial #'string-match-p regular-expression) #'buffer-name)
		(buffer-list))))

(defun preview-html () "Render buffer as HTML."
	(interactive)
	(shr-render-buffer (current-buffer)))

(defun eval-r (b e) "Evaluate region."
	(interactive "r")
	(eval-region b e)
	(deactivate-mark)
	(message "Region evaluated."))

(defun turn-off-cursor () "Hides cursor locally."
	(interactive)
	(setq-local cursor-type nil))

(defun toggle-cursor-off/on () "Toggle cursor visibility."
       (interactive)
       (if (bound-and-true-p cursor-type)
	   (setq-local cursor-type nil)
	 (setq-local cursor-type t)))

(defun my/agenda () "Load `org-agenda' file."
	(interactive)
	(find-file org-agenda-file))

(defun my/init () "Load init-file."
	(interactive)
	(find-file user-init-file))

(defun my/backward-paragraph () "Backward paragraph."
	(interactive)
	(backward-paragraph)
	(recenter-top-bottom))

(defun my/forward-paragraph () "Forward paragraph."
	(interactive)
	(forward-paragraph)
	(recenter-top-bottom))

(defun my/backward-page () "Backward page."
	(interactive)
	(backward-page)
	(recenter-top-bottom)
	(beginning-of-line))

(defun my/forward-page () "Forward page."
	(interactive)
	(next-line)
	(forward-page)
	(recenter-top-bottom)
	(beginning-of-line))

(defun my/outline-previous-heading ()
	(interactive)
	(outline-previous-heading)
	(recenter-top-bottom))

(defun my/outline-next-heading ()
	(interactive)
	(outline-next-heading)
	(recenter-top-bottom))

(defun my/View-scroll-line-backward () "Scroll line backward, jump to new top of screen."
	(interactive)
	(View-scroll-line-backward)
	(move-to-window-line-top-bottom))


;; org-mode functions

(defun org-edit-special-no-fill () "Call a special editor for the element at point; turn off fill."
	(interactive)
	(org-edit-special)
	(if (featurep 'visual-fill-column) (visual-fill-column-mode -1)))


;; DIRED functions

(defun kill-dired-buffers ()
	(interactive)
	(mapc (lambda (buffer)
		(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
		(kill-buffer buffer)))
		(buffer-list)))

(defun rename-file-and-buffer (new-name)
"Renames both current buffer and file it's visiting to NEW-NAME."
	(interactive "sNew name: ")
	(let ((name (buffer-name))
		(filename (buffer-file-name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	(if (get-buffer new-name)
		(message "A buffer named '%s' already exists!" new-name)
	(progn
		(rename-file filename new-name 1)
		(rename-buffer new-name)
		(set-visited-file-name new-name)
		(set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
"Moves both current buffer and file it's visiting to DIR."
	(interactive "DNew directory: ")
	(let* ((name (buffer-name))
		(filename (buffer-file-name))
		(dir
		(if (string-match dir "\\(?:/\\|\\\\)$")
		(substring dir 0 -1) dir))
		(newname (concat dir "/" name)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	(progn
		(copy-file filename newname 1)
		(delete-file filename)
		(set-visited-file-name newname)
		(set-buffer-modified-p nil) t))))

;; dired extension "% s"
;; https://social.tchncs.de/@stackeffect/113431684014013180
(defun my-substspaces (str)
	(subst-char-in-string ?\s ?_ str))

(defun my-dired-substspaces (&optional arg)
"Rename all marked (or next ARG) files so that spaces are replaced with underscores."
	(interactive "P")
	(dired-rename-non-directory #'my-substspaces "Rename by substituting spaces" arg))


;; iBuffer functions

(defun ibuffer-advance-motion (direction)
	(forward-line direction)
	(beginning-of-line)
	(if (not (get-text-property (point) 'ibuffer-filter-group-name)) t
		(ibuffer-skip-properties '(ibuffer-filter-group-name) direction)
		nil))

(defun ibuffer-previous-line (&optional arg)
"Move backwards ARG lines, wrapping around the list if necessary."
	(interactive "P")
	(or arg (setq arg 1))
	(let (err1 err2) (while (> arg 0)
		(cl-decf arg)
		(setq   err1 (ibuffer-advance-motion -1)
			err2 (if (not (get-text-property (point) 'ibuffer-title)) t
				(goto-char (point-max))
				(beginning-of-line)
				(ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name) -1)
					nil)))
		(and err1 err2)))

(defun ibuffer-next-line (&optional arg)
"Move forward ARG lines, wrapping around the list if necessary."
	(interactive "P")
	(or arg (setq arg 1))
	(let (err1 err2) (while (> arg 0)
		(cl-decf arg)
		(setq   err1 (ibuffer-advance-motion 1)
			err2 (if (not (get-text-property (point) 'ibuffer-summary)) t
				(goto-char (point-min))
				(beginning-of-line)
				(ibuffer-skip-properties '(ibuffer-summary ibuffer-filter-group-name
					ibuffer-title) 1)
					nil)))
		(and err1 err2)))

(defun ibuffer-next-header ()
	(interactive)
	(while (ibuffer-next-line)))

(defun ibuffer-previous-header ()
	(interactive)
	(while (ibuffer-previous-line)))

(defun ibuffer-ido-find-file (file &optional wildcards)
"Like `ido-find-file', but default to the directory of the buffer at point."
	(interactive
	(let ((default-directory
		(let ((buf (ibuffer-current-buffer)))
		(if (buffer-live-p buf)
			(with-current-buffer buf default-directory)
			default-directory))))
		(list (ido-read-file-name "Find file: " default-directory) t)))
	(find-file file wildcards))


;; scrolling
(defun window-half-height ()	(max 1 (/ (1- (window-height (selected-window))) 2)))
(defun scroll-up-half ()	(interactive) (scroll-up (window-half-height)))
(defun scroll-down-half ()	(interactive) (scroll-down (window-half-height)))


;; web browsing
(defun elpher:eww-browse-url (original url &optional new-window) "Handle gemini links."
	(cond ((string-match-p "\\`\\(gemini\\|gopher\\)://" url) (elpher-go url))
		(t (funcall original url new-window))))

(defun eww-unfill-paragraph () "Re-flow text in eww buffer."
	(interactive)
	(read-only-mode -1)
	(unfill-paragraph)
	(read-only-mode))

;; https://old.reddit.com/r/emacs/comments/17h4h4k/how_to_preview_buffer_with_html_in_ewwbrowser/
(defun eww-render-buffer ()
  "Render the current buffer in EWW."
  (interactive)
  (let* ((html (buffer-substring-no-properties (point-min) (point-max)))
    (source (buffer-name))
    (buf (generate-new-buffer (concat "eww: " source))))
    (with-current-buffer buf
      (insert html)
      (goto-char (point-min))
      (eww-display-html 'utf-8 source nil nil buf))
    (switch-to-buffer buf)))


;; https://vishesh.github.io/emacs/editors/2023/01/25/lean-emacs-config.html
;; (see bindings for: "C-a" "C-w" "M-w" "M-j")

(defun back-to-indentation-or-beginning-of-line ()
  "Move cursor to beginning of line, taking indentation into account."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

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

(defun match-paren (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun open-line-below ()
  "Starts a new line below the current line."
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun open-line-above ()
  "Starts a new line above the current line."
  (interactive)
  (move-beginning-of-line 1)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun grep-completing-read ()
"Execute grep/ripgrep search using `completing-read'."
	(interactive)
	(let* ((default-term (if (region-active-p)
			(substring-no-properties (buffer-substring (mark) (point)))
			(thing-at-point 'symbol)))
		(term (read-string "search for: " default-term))
		(execute-search (lambda (term) (if (executable-find "rg")
			(process-lines "rg" "--line-number" term)
			(process-lines "git" "grep" "-niH" "-e" term))))
		(results (funcall execute-search term))
		(line-list (split-string (completing-read "results: " results) ":"))
		(rfile (car line-list))
		(rlnum (string-to-number (car (cdr line-list)))))
	(find-file rfile)
	(goto-line rlnum)
	(recenter)))

(defun fast-file-view-mode ()
  "Make the buffer read-only and disable font-lock and other bells and whistles for faster viewing."
  (interactive)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (fundamental-mode)
  (font-lock-mode -1)
  (when (boundp 'anzu-mode) (anzu-mode -1)))

(defun large-find-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    (fast-file-view-mode)))

;; https://www.emacswiki.org/emacs/DeletingWhitespace
(defun whack-whitespace (arg)
"Delete all white space from point to the next word. With prefix ARG
delete across newlines as well. The only danger in this is that you
don't have to actually be at the end of a word to make it work. It
skips over to the next whitespace and then whacks it all to the next
word."
(interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

;; https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically
;; usage: (custom/reset-var 'somevar)
(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
	(set symbl (eval (car (get symbl 'standard-value)))))

;; https://old.reddit.com/r/emacs/comments/1im8etu/set_fill_column_to_longest_line_in_buffer/
(defun longest-line-in-buffer ()
  (save-excursion
    (goto-char (point-min))
    (let ((max-len 0)
          (curr-len 0))
      (cl-loop when (eobp)
	         return (max curr-len max-len)
               if (= (char-after) ?\n)
                 do (setq max-len (max curr-len max-len))
                   and do (setq curr-len 0)
               else
                 do (cl-incf curr-len)
               do (forward-char 1))
                 )))

;; https://www.emacswiki.org/emacs/download/misc-cmds.el
(defun goto-longest-line (beg end &optional msgp)
  "Go to the first of the longest lines in the region or buffer.
If the region is active, it is checked.
If not, the buffer (or its restriction) is checked.

Returns a list of three elements:

 (LINE LINE-LENGTH OTHER-LINES LINES-CHECKED)

LINE is the first of the longest lines measured.
LINE-LENGTH is the length of LINE.
OTHER-LINES is a list of other lines checked that are as long as LINE.
LINES-CHECKED is the number of lines measured.

Interactively, a message displays this information.

If there is only one line in the active region, then the region is
deactivated after this command, and the message mentions only LINE and
LINE-LENGTH.

If this command is repeated, it checks for the longest line after the
cursor.  That is *not* necessarily the longest line other than the
current line.  That longest line could be before or after the current
line.

To search only from the current line forward, not throughout the
buffer, you can use `C-SPC' to set the mark, then use this
\(repeatedly)."

  (interactive
   (if (or (not mark-active)  (not (< (region-beginning) (region-end))))
       (list (point-min) (point-max))
     (if (< (point) (mark))
         (list (point) (mark) 'MSGP)
       (list (mark) (point) 'MSGP))))
  (when (and (not mark-active)  (= beg end)) (error "The buffer is empty"))
  (when (and mark-active  (> (point) (mark))) (exchange-point-and-mark))
  (when (< end beg) (setq end  (prog1 beg (setq beg  end))))
  (when (eq 'goto-longest-line last-command)
    (forward-line 1) (setq beg  (point)))
  (goto-char beg)
  (when (eobp) (error "End of buffer"))
  (cond ((<= end (save-excursion (goto-char beg) (forward-line 1) (point)))
         (let ((inhibit-field-text-motion  t))  (beginning-of-line))
         (when (and (> emacs-major-version 21)  (require 'hl-line nil t))
           (let ((hl-line-mode  t))  (hl-line-highlight))
           (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
         (let ((lineno  (line-number-at-pos))
               (chars   (let ((inhibit-field-text-motion  t))
                          (save-excursion (end-of-line) (current-column)))))
           (message "Only line %d: %d chars" lineno chars)
           (let ((visible-bell  t))  (ding))
           (setq mark-active  nil)
           (list lineno chars nil 1)))
        (t
         (let* ((start-line                 (line-number-at-pos))
                (max-width                  0)
                (line                       start-line)
                (inhibit-field-text-motion  t)
                long-lines col)
           (when (eobp) (error "End of buffer"))
           (while (and (not (eobp))  (< (point) end))
             (end-of-line)
             (setq col  (current-column))
             (when (>= col max-width)
               (setq long-lines  (if (= col max-width)
                                     (cons line long-lines)
                                   (list line))
                     max-width   col))
             (forward-line 1)
             (setq line  (1+ line)))
           (setq long-lines  (nreverse long-lines))
           (let ((lines  long-lines))
             (while (and lines  (> start-line (car lines))) (pop lines))
             (goto-char (point-min))
             (when (car lines) (forward-line (1- (car lines)))))
           (when (and (> emacs-major-version 21)  (require 'hl-line nil t))
             (let ((hl-line-mode  t))  (hl-line-highlight))
             (add-hook 'pre-command-hook #'hl-line-unhighlight nil t))
           (when msgp
             (let ((others  (cdr long-lines)))
               (message "Line %d: %d chars%s (%d lines measured)"
                        (car long-lines) max-width
                        (concat
                         (and others
                              (format ", Others: {%s}" (mapconcat
                                                        (lambda (line) (format "%d" line))
                                                        (cdr long-lines) ", "))))
                        (- line start-line))))
           (list (car long-lines) max-width (cdr long-lines) (- line start-line))))))

;; https://emacsnotes.wordpress.com/2023/09/14/view-emacs-news-files-as-info-manual-too/
(defun view-text-file-as-info-manual ()
  "View ‘info’, ‘texi’, ‘org’, ‘md’ and 'NEWS' files as ‘Info’ manual."
  (interactive)
  (require 'rx)
  (require 'ox-texinfo)
  (when (buffer-file-name)
    (let* ((org-export-with-broken-links 'mark)
           (ext (file-name-extension (buffer-file-name))))
      (cond
       ;; A NEWS files
       ((string-match "NEWS" (file-name-nondirectory (buffer-file-name)))
        (with-current-buffer
            ;; NEWS files are likely to be in read-only directories.
            ;; So make a copy with an `.org' extension.  Most NEWS
            ;; file are `outline-mode' files with `org' like heading
            ;; structure.  Many of the recent files like ORG-NEWS are
            ;; proper `org' files.
            (find-file-noselect
             (make-temp-file
              (format "%s---" (file-name-nondirectory (buffer-file-name))) nil ".org"
              (buffer-substring-no-properties (point-min) (point-max))))
          (org-with-wide-buffer
           ;; `ox-texinfo' export fails if a headline ends with a
           ;; period (= ".").  So, strip those terminating periods.
           (goto-char (point-min))
           (while (re-search-forward (rx (and bol
                                              (one-or-more "*")
                                              " "
                                              (one-or-more any)
                                              (group ".")
                                              eol))
                                     (point-max) t)
             (replace-match "" t t nil 1))
           (goto-char (point-min))
           (while nil
             ;; TODO: If a NEWS file contains text which resemble a
             ;; LaTeX fragment, the `ox-texinfo' export wouldn't
             ;; succeed.  So, enclose the LaTeX fragment with Org's
             ;; verbatim `=' marker.
             )
           (save-buffer 0)
           (info (org-texinfo-export-to-info)))))
       ;; A `.info' file
       ((or (string= "info" ext))
        (info (buffer-file-name)))
       ;; A `.texi' file
       ((or (string= "texi" ext))
        (info (org-texinfo-compile (buffer-file-name))))
       ;; An `.org' file
       ((or (derived-mode-p 'org-mode)
            (string= "org" ext))
        (info (org-texinfo-export-to-info)))
       ;; A `.md' file
       ((or (derived-mode-p 'markdown-mode)
            (string= "md" ext))
        (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
          (apply #'call-process "pandoc" nil standard-output nil
                 `("-f" "markdown"
                   "-t" "org"
                   "-o" ,org-file-name
                   ,(buffer-file-name)))
          (with-current-buffer (find-file-noselect org-file-name)
            (info (org-texinfo-export-to-info)))))
       (t (user-error "Don't know how to convert `%s' to an `info' file"
                      (buffer-file-name)))))))

;;; filesandbuffers.el ends here
; LocalWords:  filesandbuffers
