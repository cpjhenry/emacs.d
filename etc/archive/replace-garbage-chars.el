(defun replace-garbage-chars ()
  "Replace goofy MS and other garbage characters with Latin1 equivalents."
  (interactive)
  (let ((beg (point-min))
	(end (point-max)))
    (when (region-active-p)
      (setq beg (region-beginning))
      (setq end (region-end)))
    (save-excursion ;save the current point
      (replace-string "΄" "'" nil beg end)
      (replace-string "‘" "'" nil beg end)
      (replace-string "’" "'" nil beg end)
      (replace-string "“" "\"" nil beg end)
      (replace-string "”" "\"" nil beg end)
      (replace-string "" "'" nil beg end)
      (replace-string "" "'" nil beg end)
      (replace-string "" "\"" nil beg end)
      (replace-string "" "\"" nil beg end)
      (replace-string "" "\"" nil beg end)
      (replace-string "" "\"" nil beg end)
      (replace-string "‘" "\"" nil beg end)
      (replace-string "’" "'" nil beg end)
      (replace-string "¡\"" "\"" nil beg end)
      (replace-string "¡­" "..." nil beg end)
      (replace-string "" "..." nil beg end)
      (replace-string "" " " nil beg end) ; M-SPC
      (replace-string "" "`" nil beg end)  ; \221
      (replace-string "" "'" nil beg end)  ; \222
      (replace-string "" "``" nil beg end)
      (replace-string "" "''" nil beg end)
      (replace-string "" "*" nil beg end)
      (replace-string "" "--" nil beg end)
      (replace-string "" "--" nil beg end)
      (replace-string "¡" "\"" nil beg end)
      (replace-string "´" "\"" nil beg end)
      (replace-string "»" "<<" nil beg end)
      (replace-string "Ç" "'" nil beg end)
      (replace-string "È" "\"" nil beg end)
      (replace-string "é" "e" nil beg end) ;; &eacute;
      (replace-string "ó" "-" nil beg end)

      ;; mine
      (replace-string "•" "-" nil beg end)
      (replace-string "–" "--" nil beg end)
      (replace-string "—" "---" nil beg end) ; multi-byte
      (replace-string "…" "..." nil beg end)
      (replace-string "&#x27;""'" nil beg end)
      (replace-string "&#38;" "&" nil beg end)
      (replace-string "&#39;" "'" nil beg end)

      (message "Garbage in, garbage out.") )))
