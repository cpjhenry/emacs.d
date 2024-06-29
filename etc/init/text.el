;; text functions

(defun buf-to-LF()
	(interactive)
	(set-buffer-file-coding-system 'utf-8-unix)
	(set-buffer-modified-p nil))

(defun insert-tab-char ()
	"Insert a tab char. (ASCII 9, \t)"
	(interactive)
	(insert "\t"))

(defun unfill-paragraph ()
	"Takes a multi-line paragraph and makes it into a single line of text."
	(interactive)
	;(if (visual-line-mode) (visual-line-mode -1))

	(let ((beg (point-min)) (end (point-max)) (fill-column (point-max)))
	(when (region-active-p)
		(setq beg (region-beginning))
		(setq end (region-end)))
	(fill-region beg end))

	;(visual-line-mode)
	)

(defun insert-iso-date ()
	"Insert ISO-formatted date."
	(interactive)
	(insert (format-time-string "%Y-%m-%d")))

(defun insert-date ()
	"Insert European-formatted date."
	(interactive)
	(insert (format-time-string "%-d %B %Y")))

(defun markdown-preview-file ()
	"Run Marked on the current file and revert the buffer"
	(interactive)
	(shell-command (format "open -a /Applications/Marked\\ 2.app %s"
		(shell-quote-argument (buffer-file-name)) ))
	(let ((buffer "*Shell Command Output*")) (and (get-buffer buffer)
		(kill-buffer buffer))) )

(defun flush-blank-lines (start end)
	"Remove blank lines in a buffer."
	(interactive "r")
	(flush-lines "^\\s-*$" start end nil))

(defun delete-duplicate-words ()
	"Delete duplicate words via `query-replace-regexp'."
	(interactive nil text-mode)
	(save-excursion
		(goto-char (point-min))
		(query-replace-regexp "\\(\\b\\w+\\b\\)\\W+\\1\\b" "\\1")))

(defun replace-smart-quotes (beg end)
	"Replace 'smart quotes' in buffer or region with ASCII quotes."
	(interactive "r")
	(format-replace-strings '(
		("\x201C" . "\"")
		("\x201D" . "\"")
		("\x2018" . "'")
		("\x2019" . "'") ) nil beg end)
	(message "Smart quotes replaced."))

;; https://www.emacswiki.org/emacs/ReplaceGarbageChars
;; FIXME scan only region if exists...
(defun replace-garbage-chars ()
	"Replace goofy MS and other garbage characters with Latin1 equivalents."
	(interactive) (save-excursion				;save the current point
	(replace-string "΄" "\"" nil (point-min) (point-max))
	(replace-string "“" "\"" nil (point-min) (point-max))
	(replace-string "’" "'" nil (point-min) (point-max))
	(replace-string "“" "\"" nil (point-min) (point-max))
	(replace-string "”" "\"" nil (point-min) (point-max))
	(replace-string "—" "--" nil (point-min) (point-max)) ; multi-byte
	(replace-string "" "'" nil (point-min) (point-max))
	(replace-string "" "'" nil (point-min) (point-max))
	(replace-string "" "\"" nil (point-min) (point-max))
	(replace-string "" "\"" nil (point-min) (point-max))
	(replace-string "" "\"" nil (point-min) (point-max))
	(replace-string "" "\"" nil (point-min) (point-max))
	(replace-string "‘" "\"" nil (point-min) (point-max))
	(replace-string "’" "'" nil (point-min) (point-max))
	(replace-string "¡\"" "\"" nil (point-min) (point-max))
	(replace-string "¡­" "..." nil (point-min) (point-max))
	(replace-string "" "..." nil (point-min) (point-max))
	(replace-string "" " " nil (point-min) (point-max)) ; M-SPC
	(replace-string "" "`" nil (point-min) (point-max))  ; \221
	(replace-string "" "'" nil (point-min) (point-max))  ; \222
	(replace-string "" "``" nil (point-min) (point-max))
	(replace-string "" "''" nil (point-min) (point-max))
	(replace-string "" "*" nil (point-min) (point-max))
	(replace-string "" "--" nil (point-min) (point-max))
	(replace-string "" "--" nil (point-min) (point-max))
	(replace-string " " " " nil (point-min) (point-max)) ; M-SPC
	(replace-string "¡" "\"" nil (point-min) (point-max))
	(replace-string "´" "\"" nil (point-min) (point-max))
	(replace-string "»" "<<" nil (point-min) (point-max))
	(replace-string "Ç" "'" nil (point-min) (point-max))
	(replace-string "È" "\"" nil (point-min) (point-max))
	(replace-string "é" "e" nil (point-min) (point-max)) ;; &eacute;
	(replace-string "ó" "-" nil (point-min) (point-max))

	;; mine
	(replace-string "•" "-" nil (point-min) (point-max))
	(replace-string "…" "..." nil (point-min) (point-max))
	(replace-string "&#38;" "&" nil (point-min) (point-max))
	(replace-string "&#39;" "'" nil (point-min) (point-max))
	(message "Garbage in, garbage out.") ))

;; https://emacs.stackexchange.com/questions/51629/add-paragraph-numbers
(defun number-paragraphs (&optional takefirst)
"Numbers resp. renumber paragraphs.

If starting from already numbered, take that value as offset."
	(interactive "*P")
	(let ((counter 0)
		(last 0))
		(when  (looking-at "\\([0-9]+\\)\. ")
			(setq counter (car (read-from-string  (match-string-no-properties 1))))
			(forward-paragraph))
		(while (and (forward-paragraph) (< last (point)))
			(setq last (copy-marker (point)))
			(backward-paragraph)
			(skip-chars-forward " \t\r\n\f")
			(when (looking-at "[0-9]+\. ")
				(delete-region (match-beginning 0) (match-end 0)))
			(insert (format "%s. " (1+ counter)))
			(setq counter (1+ counter))
			(goto-char last))))

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

(defun mark-from-beginning-of-buffer ()
	"Marks the region from the beginning of the buffer to point."
	(interactive)
	(push-mark (point-min) nil t))

(defun undo-yank (arg)
  "Undo the yank you just did.  Really, adjust just-yanked text
like \\[yank-pop] does, but in the opposite direction."
  (interactive "p")
  (yank-pop (- arg)))

;; https://speechcode.com/blog/narrow-to-focus/
(defun narrow-to-focus (start end)
"If the region is active, narrow to region, marking it (and only
it) for the future.  If the mark is not active, narrow to the
region that was the most recent focus."
	(interactive "r")
	(cond ((use-region-p)
	(remove-overlays (point-min) (point-max) 'focus t)
	(let ((overlay (make-overlay start end)))
		(overlay-put overlay 'focus t)
		(narrow-to-region start end)))
	(t (let ((focus
		(seq-find (lambda (o) (overlay-get o 'focus))
		(overlays-in (point-min) (point-max)))))
	(when focus
		(narrow-to-region (overlay-start focus)
		(overlay-end focus)))))))
	;(define-key global-map "\C-xnf" 'narrow-to-focus)

;; eww functions

(defun eww-reddit-redirect (url)
	"Redirect reddit.com to old.reddit.com automatically."
	(replace-regexp-in-string "https://www.reddit.com" "https://old.reddit.com" url))
	(setq eww-url-transformers '(eww-remove-tracking eww-reddit-redirect))

;; prog-mode functions

(defun align-equals (begin end)
	(interactive "r")
	(align-regexp begin end "\\(\\s-*\\)=" 1 1))
