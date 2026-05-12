(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char (codepoint 12) as line.
URL `http://xahlee.info/emacs/emacs/emacs_show_form_feed_as_line.html'
Created: 2018-08-30
Version: 2024-05-24"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table 12
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))
