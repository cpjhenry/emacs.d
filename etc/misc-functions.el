;;; misc.el --- Miscellaneous functions
;;; commentary:
;; My miscellaneous routines moved to: cpjhelp.el.

;;; code:
;; Compatibility settings
(when (< emacs-major-version 28) (defalias 'show-paren-local-mode 'show-paren-mode))

;; https://www.emacswiki.org/emacs/DisabledCommands

(defun enable-all-commands ()
"Enable all commands, reporting on which were disabled."
	(interactive)
	(with-output-to-temp-buffer "*Commands that were disabled*"
	(mapatoms (function (lambda (symbol)
		(when (get symbol 'disabled)
			(put symbol 'disabled nil)
			(prin1 symbol)
			(princ "\n")))))))

(defun enable-me (&rest args)
"Called when a disabled command is executed. Enable it and re-execute it."
	(put this-command 'disabled nil)
	(message "You typed %s.  %s was disabled.  It ain't no more."
	(key-description (this-command-keys)) this-command)
	(sit-for 0)
	(call-interactively this-command))


;; https://svn.red-bean.com/repos/kfogel/trunk/.emacs

;; Setting modes based on filenames:
(add-to-list 'auto-mode-alist '("\\.pl$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . perl-mode))


;;; On-demand help panels for obscure topics. ;;;

(defun kf-display-something-maybe-big (contents &optional title)
  "Display string CONTENTS in a buffer named TITLE."
  (let ((buf (get-buffer-create (or title "*STUFF*")))
        (win nil)
        (lines nil))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (insert contents)
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
    (setq win (display-buffer buf))
    (when (> lines (window-text-height win))
      (select-window win))
	(help-mode) )))

(defmacro kf-gen-displayer (txt-sym fn-doc-str buf-name &optional fn-alias)
  "Generate an interactive function with the same symbol name as TXT-SYM,
whose doc string is FN-DOC-STR, and that when invoked displays TXT-SYM
in a buffer named BUF-NAME using `display-buffer'."
  (declare (indent 2))
  `(progn
     (defun ,txt-sym ()
       ,fn-doc-str
       (interactive)
       (kf-display-something-maybe-big ,txt-sym ,buf-name))
     (when (or (not (boundp ',fn-alias)) (not (eq nil ,fn-alias)))
       (defalias ',fn-alias ',txt-sym))))


(defun kf-htmlegalize-region (b e)
  "Replace \"&\", \"<\", and \">\" with their HTML escape codes, from B to E.
Is there anything else that should be done to escape HTML?"
  (interactive "r")
  (save-excursion
    (let ((em (copy-marker e)))
      (goto-char b)
      (while (search-forward "&" em t)
        (replace-match "&amp;" nil t))
      (goto-char b)
      (while (search-forward "<" em t)
        (replace-match "&lt;" nil t))
      (goto-char b)
      (while (search-forward ">" em t)
        (replace-match "&gt;" nil t))
      )))

(defun kf-anchor-set (name)
  "Set a standard HTML named anchor at point.
This assumes you are inside the attr area of an HTML element opening tag."
  (interactive "MAnchor name: ")
  (insert (format " id=\"%s\" title=\"%s\"" name name)))

(defun kf-highlight-region (b e)
  "Highlight the region assuming an HTML+CSS \"highlight\" class."
  (interactive "r")
  (save-excursion
    (setq e (copy-marker e))
    (goto-char b)
    (insert "<span class=\"highlight\" >")
    (goto-char e)
    (insert "</span>")))

(defun kf-paragraphize (&optional no-fill)
  "Put paragraph tags around the paragraph at point.
Refill to compensate for the tags, unless prefix arg NO-FILL is non-nil."
  (interactive "P")
  (let* ((markup-flavor (kf-markup-flavor))
         (open-tag
          (cond
           ((eq markup-flavor 'xml) "<para>")
           (t                       "<p>")))
         (close-tag
          (cond
           ((eq markup-flavor 'xml) "</para>")
           (t                       "</p>"))))
    (save-excursion
      (forward-paragraph -1)
      (forward-line 1)
      (forward-word 1)
      (forward-word -1)
      (insert open-tag)
      (forward-paragraph 1)
      (forward-char -1)
      (insert close-tag)
      (unless no-fill (kf-fill-paragraph nil)))))


(defun kf-quoted-printable-decode (b e)
  "Soft-decode the quoted-printable-encoded text from B to E.

Just convert certain common quoted-printable codes to roughly
corresponding ASCII characters suitable for plaintext documents.
(This is different from `quoted-printable-decode-region', which
would actually interpret the quoted-printable text and insert
whatever Unicode characters it specified.)

Example text:

   We won=E2=80=99t be eating the meteorite as proposed.=C2=A0 Instead,
   we (=3D=3D just us, not you) would like to request:

   =E2=80=A2 Yams or nightshades;=C2=A0

   =E2=80=A2 Anything involving beryllium.

   Thank =E2=80=98you=E2=80=99 for your =E2=80=9Ctime=E2=80=9D.
"
  (interactive "r")
  (mapcar (lambda (rplc)
            (let ((qp-code     (car rplc))
                  (replacement (cdr rplc)))
              (goto-char b)
              (replace-string qp-code replacement nil b e)))
          '(
            ("=3D"            . "=")
            ("=E2=80=98"      . "'")
            ("=E2=80=99"      . "'")
            ("=E2=80=9C"      . "\"")
            ("=E2=80=9D"      . "\"")
            ("=C2=A0"         . " ")
            ("=E2=80=A2"      . "*")
            ;; more to come
            ))
  ;; now handle the =\n\\s-* thing
  )


;;; Insertion helpers for characters not in my usual input methods.

(defun kf-checkbox (parg)
  "Insert a checkbox.
With one prefix arg, insert a checked checkbox.
With two prefix args, insert an x'ed checkbox."
  (interactive "P")
  (let ((prefix (car parg)))
    (cond
     ((not prefix)  (insert ?☐)) ; 9744
     ((= prefix 4)  (insert ?☑)) ; 9745
     ((= prefix 16) (insert ?☒)) ; 9746
     (t (error "What do you want me to put in that checkbox?")))))

(defun kf-arrow (type)
  "Insert an arrow of TYPE, where type is a single letter:
    - \"[u]p\"
    - \"[d]own\"
    - \"[l]eft\"
    - \"[r]ight\"
    - \"[h]orizontal double arrow\"
    - \"[v]ertical double arrow\""
  (interactive
   "cArrow type ([u]p, [d]own, [l]eft, [r]ight, [h]oriz, [v]ert): ")
    (insert (cdr (assoc type '((?u . ?↑)
                               (?d . ?↓)
                               (?l . ?←)
                               (?r . ?→)
                               (?h . ?↔)
                               (?v . ?↕)
                               )))))


(defun kf-reverse-lines-region (b e)
  "Reverse the order of lines containing B (inclusive) to E (exclusive)."
  (interactive "r")
  ;; There are two ways to do this: the Emacs way, and the easy way.
  ;; We're going to do it the easy way.
  (save-excursion
    (let ((lines ())
          (b (progn (goto-char b) (beginning-of-line) (point)))
          (e (progn (goto-char e) (beginning-of-line) (point))))
      (goto-char b)
      (while (< (point) e)
        (setq lines
              (cons
               (buffer-substring (point) (progn (forward-line 1) (point)))
               lines)))
      (delete-region b e)
      (mapcar 'insert lines))))

(defun kf-reverse-words (b e)
  "Reverse the order of words in the region from B to E."
  (interactive "*r")
  (apply 'insert
         (reverse (split-string (delete-and-extract-region b e) "\\b"))))

;; From https://www.emacswiki.org/emacs/RandomizeWords
(defun randomize-region (beg end)
  "Randomize the order of words in region."
  (interactive "*r")
  (let ((all (mapcar
              (lambda (w) (if (string-match "\\w" w)
                              ;; Randomize words,
                              (cons (random) w)
                            ;; keep everything else in order.
                            (cons -1 w)))
              (split-string
               (delete-and-extract-region beg end) "\\b")))
        words sorted)
    (mapc (lambda (x)
            ;; Words are numbers >= 0.
            (unless (> 0 (car x))
              (setq words (cons x words))))
          all)
    ;; Random sort!
    (setq sorted (sort words
                       (lambda (a b) (< (car a) (car b)))))
    (mapc
     'insert
     ;; Insert using original list, `all',
     ;; but pull *words* from randomly-sorted list, `sorted'.
     (mapcar (lambda (x)
               (if (> 0 (car x))
                   (cdr x)
                 (prog1 (cdar sorted)
                   (setq sorted (cdr sorted)))))
             all))))


;;; Various kinds of auto-insertion. ;;;;
(defun kf-format-time-string (time-val)
  "Return a date string formatted from TIME-VAL the way I usually want."
  (format-time-string "%Y-%m-%d %H:%M:%S %Z" time-val))

(defun kf-insert-date (&optional thorough)
  "Insert the current date (with day-of-week and time-of-day if THOROUGH).
If there is only whitespace or nothing between point and the first
column, then prepend asterisk + space and post-pend colon + space."
  (interactive "P")
  (let* ((decorate nil)
         (span (buffer-substring-no-properties
                (point) (save-excursion (beginning-of-line) (point)))))
    (save-match-data
      (when (string-match "^\\s-*$" span)
        (setq decorate t)))
    (insert (format-time-string (format "%s%s%%Y-%%m-%%d%s%s"
                                        (if decorate "* " "")
                                        (if thorough "%A, " "")
                                        (if thorough " (%H:%M:%S)" "")
                                        (if decorate ": " ""))))
    (when thorough
      ;; Position cursor on the start of the time portion, since
      ;; that's what's most likely to need editing right now.
      (re-search-backward "([0-9]")
      (forward-char 1))))


;; https://olddeuteronomy.github.io/post/emacs-startup-screen/
(defun tec/welcome-page ()
    (interactive)
    (let* ((buffer-today (get-buffer-create "*today*"))
           (buffer-calendar "*Calendar*")
           (buffer-agenda "*Org Agenda*")
           (buffer-diary "*Fancy Diary Entries*"))
      ;; Call calendar first to obtain the current date
      ;; required to display the diary.
      (calendar)
      (diary)
      (org-agenda-list)
      ;; Fill and show the Today Events buffer.
      ;; NOTE: requires `fortune' and `calendar' command line utilities.
      (switch-to-buffer buffer-today)
      (call-process "fortune" nil buffer-today)
      (insert "\n")
      (call-process "calendar" nil buffer-today)
      (goto-char 0)
      (toggle-truncate-lines)
      ;; Maximize the Today Events window
      (delete-other-windows)
      ;; Show Agenda in the lower left quadrant.
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer (get-buffer buffer-agenda))
      (split-window-horizontally)
      ;; Try to show Diary in the lower right quadrant.
      (other-window 1)
      (if (get-buffer buffer-diary)
          ;; If Diary exists then show it ...
          (switch-to-buffer (get-buffer buffer-diary))
        ;; ... else show the scratch buffer.
        (let* ((buffer-scratch (switch-to-buffer (get-buffer "*scratch*"))))
          (goto-char (point-max))
          (insert (format-time-string "\n;; No diary entries for %A %d %b")))
        )
      ;; Go back to the Today Events buffer.
      (other-window -2)
      (split-window-horizontally)
      ;; Show Calendar in the upper left quadrant.
      (switch-to-buffer (get-buffer buffer-calendar))
      ))

;;; misc.el ends here
