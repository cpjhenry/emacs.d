;;; org-pretty-table.el --- Pretty table borders with box-drawing glyphs -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2023 Matus Goljer
;; Copyright (C) 2026 cpj

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Adapted and extended by: cpj
;; Keywords: faces, org, tables
;; URL: https://github.com/Fuco1/org-pretty-table
;; Package-Requires: ((org "9") (emacs "24.4"))
;; Version: 2.0.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or at your option any later version.

;;; Commentary:

;; Originally written by Matus Goljer.
;;
;; This version substantially modernizes the rendering architecture
;; using overlays, extends support to table.el-style grid tables, and
;; integrates more cleanly with contemporary Org-mode
;; display environments.

;; `org-pretty-table-mode' displays Org and table.el-style ASCII table
;; borders using Unicode box-drawing glyphs. The underlying buffer
;; text is not changed: ordinary Org table syntax and plain ASCII grid
;; tables remain intact for editing, alignment, export, version
;; control, and interoperability with other tools.
;;
;; Rendering is performed using owned overlays rather than text
;; properties, allowing the mode to coexist more reliably with modern
;; Org-mode display environments, including font-lock, jit-lock,
;; overlays, visual wrapping, and folding systems.
;;
;; This version walks table lines directly instead of depending on
;; regexp match state.
;;
;; The mode also extends selected Org-style navigation behaviour to
;; table.el-style grid tables.  TAB and S-TAB move between cells,
;; while sentence motion commands operate on the current cell rather
;; than treating wrapped table contents as ordinary prose.

;;; Code:
(require 'org)
(require 'org-table)
(require 'jit-lock)
(require 'seq)

(defgroup org-pretty-table nil
  "Replace Org table characters with box-drawing Unicode glyphs."
  :group 'org
  :prefix "org-pretty-table-")

(defcustom org-pretty-table-charset "┌┐└┘┬┤┴├┼─│"
  "Charset used to draw Org table borders.

The value is a string of length 11.  Characters are read in this order:

  0 upper-left corner
  1 upper-right corner
  2 lower-left corner
  3 lower-right corner
  4 down-facing T
  5 left-facing T
  6 up-facing T
  7 right-facing T
  8 cross
  9 horizontal bar
 10 vertical bar"
  :group 'org-pretty-table
  :type '(choice (const :tag "Single horizontal lines" "┌┐└┘┬┤┴├┼─│")
                 (const :tag "Double horizontal lines" "╒╕╘╛╤╡╧╞╪═│")
                 (string :tag "Custom charset")))

(defcustom org-pretty-table-disable-while-editing nil
  "When non-nil, remove pretty table display from the current table.

This is usually unnecessary, because Org table alignment works on buffer
text rather than display glyphs.  The option is provided for older Emacs or
Org combinations where display properties disturb visual editing."
  :group 'org-pretty-table
  :type 'boolean)

(defconst org-pretty-table--dash 45
  "Character code for hyphen-minus, used in Org table hlines.")

(defconst org-pretty-table--plus 43
  "Character code for plus sign, used in Org table hlines.")

(defconst org-pretty-table--pipe 124
  "Character code for vertical bar, used in Org table borders.")

(defsubst org-pretty-table--char (index)
  "Return character at INDEX in `org-pretty-table-charset' as a string."
  (char-to-string (aref org-pretty-table-charset index)))

(defsubst org-pretty-table-ul-corner ()
  "Return upper-left corner character as a string."
  (org-pretty-table--char 0))

(defsubst org-pretty-table-ur-corner ()
  "Return upper-right corner character as a string."
  (org-pretty-table--char 1))

(defsubst org-pretty-table-ll-corner ()
  "Return lower-left corner character as a string."
  (org-pretty-table--char 2))

(defsubst org-pretty-table-lr-corner ()
  "Return lower-right corner character as a string."
  (org-pretty-table--char 3))

(defsubst org-pretty-table-df-t ()
  "Return down-facing T character as a string."
  (org-pretty-table--char 4))

(defsubst org-pretty-table-lf-t ()
  "Return left-facing T character as a string."
  (org-pretty-table--char 5))

(defsubst org-pretty-table-uf-t ()
  "Return up-facing T character as a string."
  (org-pretty-table--char 6))

(defsubst org-pretty-table-rf-t ()
  "Return right-facing T character as a string."
  (org-pretty-table--char 7))

(defsubst org-pretty-table-cross ()
  "Return cross character as a string."
  (org-pretty-table--char 8))

(defsubst org-pretty-table-hb ()
  "Return horizontal bar character as a string."
  (org-pretty-table--char 9))

(defsubst org-pretty-table-vb ()
  "Return vertical bar character as a string."
  (org-pretty-table--char 10))

(defun org-pretty-table--valid-charset-p ()
  "Return non-nil when `org-pretty-table-charset' has the expected length."
  (= (length org-pretty-table-charset) 11))

;; detects grids found in table.el
(defun org-pretty-table--grid-hline-p (&optional position)
  "Return non-nil when POSITION, or point, is on a grid-table hline."
  (save-excursion
    (when position
      (goto-char position))
    (goto-char (line-beginning-position))
    (skip-chars-forward " \t" (line-end-position))
    (looking-at-p "\\+[-+]+\\+[ \t]*$")))

(defun org-pretty-table--table-line-p (&optional position)
  "Return non-nil when POSITION, or point, is on an Org table line."
  (save-excursion
    (when position
      (goto-char position))
    (goto-char (line-beginning-position))
    (skip-chars-forward " \t" (line-end-position))
    (or (eq (char-after) org-pretty-table--pipe)
        (org-pretty-table--grid-hline-p))))

(defun org-pretty-table-at-table-p ()
  "Return non-nil when point is on an Org table line.

This function is kept for compatibility with the original package."
  (org-pretty-table--table-line-p))

(defun org-pretty-table--line-above-table-p ()
  "Return non-nil when the line above point is an Org table line."
  (save-excursion
    (and (= (forward-line -1) 0)
         (org-pretty-table--table-line-p))))

(defun org-pretty-table--line-below-table-p ()
  "Return non-nil when the line below point is an Org table line."
  (save-excursion
    (and (= (forward-line 1) 0)
         (org-pretty-table--table-line-p))))

(defun org-pretty-table--put-display (beg end glyph)
  "Display text from BEG to END as GLYPH using an owned overlay."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'display glyph)
    (overlay-put ov 'org-pretty-table t)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'evaporate t)))

(defun org-pretty-table--put-horizontal-display (beg end)
  "Display text from BEG to END as horizontal box-drawing bars."
  (org-pretty-table--put-display
   beg end
   (make-string (- end beg) (aref org-pretty-table-charset 9))))

(defun org-pretty-table--clear-overlays (start end)
  "Remove `org-pretty-table-mode' overlays between START and END."
  (dolist (ov (overlays-in start end))
    (when (overlay-get ov 'org-pretty-table)
      (delete-overlay ov))))

(defun org-pretty-table-unpropertize-region (start end)
  "Remove box-drawing overlays between START and END."
  (org-pretty-table--clear-overlays start end))

(defun org-pretty-table-unpropertize-table ()
  "Remove box-drawing overlays from the Org table at point."
  (when (org-at-table-p)
    (org-pretty-table--clear-overlays (org-table-begin) (org-table-end))))

(defun org-pretty-table--char-left-p (char)
  "Return non-nil when the character before point is CHAR."
  (and (> (point) (line-beginning-position))
       (eq (char-before) char)))

(defun org-pretty-table--char-right-p (char)
  "Return non-nil when the character after point is CHAR."
  (eq (char-after (1+ (point))) char))

(defun org-pretty-table--same-column-char-above-p (char)
  "Return non-nil when the same visual column above point contain CHAR."
  (let ((column (current-column)))
    (save-excursion
      (and (= (forward-line -1) 0)
           (move-to-column column)
           (eq (char-after) char)))))

(defun org-pretty-table--same-column-char-below-p (char)
  "Return non-nil when the same visual column below point contain CHAR."
  (let ((column (current-column)))
    (save-excursion
      (and (= (forward-line 1) 0)
           (move-to-column column)
           (eq (char-after) char)))))

(defun org-pretty-table--vertical-connector-above-p ()
  "Return non-nil when a vertical table connector appears above point."
  (and (org-pretty-table--line-above-table-p)
       (org-pretty-table--same-column-char-above-p org-pretty-table--pipe)))

(defun org-pretty-table--vertical-connector-below-p ()
  "Return non-nil when a vertical table connector appears below point."
  (and (org-pretty-table--line-below-table-p)
       (org-pretty-table--same-column-char-below-p org-pretty-table--pipe)))

(defun org-pretty-table--display-for-pipe ()
  "Return the box-drawing glyph appropriate for a pipe at point."
  (let ((left  (org-pretty-table--char-left-p org-pretty-table--dash))
        (right (org-pretty-table--char-right-p org-pretty-table--dash))
        (above (org-pretty-table--line-above-table-p))
        (below (org-pretty-table--line-below-table-p)))
    (cond
     ((and right above below) (org-pretty-table-rf-t))
     ((and left above below)  (org-pretty-table-lf-t))
     ((and left (not above))  (org-pretty-table-ur-corner))
     ((and left (not below))  (org-pretty-table-lr-corner))
     ((and right (not above)) (org-pretty-table-ul-corner))
     ((and right (not below)) (org-pretty-table-ll-corner))
     (t (org-pretty-table-vb)))))

(defun org-pretty-table--display-for-plus ()
  "Return the box-drawing glyph appropriate for a plus sign at point."
  (let ((left  (org-pretty-table--char-left-p org-pretty-table--dash))
        (right (org-pretty-table--char-right-p org-pretty-table--dash))
        (above (org-pretty-table--vertical-connector-above-p))
        (below (org-pretty-table--vertical-connector-below-p)))
    (cond
     ((and left right above below) (org-pretty-table-cross))
     ((and left right below)       (org-pretty-table-df-t))
     ((and left right above)       (org-pretty-table-uf-t))
     ((and right above below)      (org-pretty-table-rf-t))
     ((and left above below)       (org-pretty-table-lf-t))
     ((and right below)            (org-pretty-table-ul-corner))
     ((and left below)             (org-pretty-table-ur-corner))
     ((and right above)            (org-pretty-table-ll-corner))
     ((and left above)             (org-pretty-table-lr-corner))
     (t (org-pretty-table-cross)))))

(defun org-pretty-table--fontify-at-point ()
  "Apply pretty table display to the table character at point.

Return the buffer position from which scanning should continue."
  (let ((beg (point))
        (char (char-after)))
    (cond
     ((eq char org-pretty-table--dash)
      (if (looking-at "-+")
          (let ((dash-beg (match-beginning 0))
                (dash-end (match-end 0)))
            (when (memq (char-after dash-end)
                        (list org-pretty-table--plus
                              org-pretty-table--pipe))
              (org-pretty-table--put-horizontal-display dash-beg dash-end))
            dash-end)
        (1+ beg)))
     ((eq char org-pretty-table--pipe)
      (org-pretty-table--put-display
       beg (1+ beg)
       (org-pretty-table--display-for-pipe))
      (1+ beg))
     ((eq char org-pretty-table--plus)
      (org-pretty-table--put-display
       beg (1+ beg)
       (org-pretty-table--display-for-plus))
      (1+ beg))
     (t
      (1+ beg)))))

(defun org-pretty-table--fontify-line (line-beg line-end)
  "Apply pretty table display to table characters between LINE-BEG and LINE-END."
  (goto-char line-beg)
  (while (< (point) line-end)
    (let ((char (char-after)))
      (if (or (eq char org-pretty-table--dash)
              (eq char org-pretty-table--plus)
              (eq char org-pretty-table--pipe))
          (goto-char (min line-end (org-pretty-table--fontify-at-point)))
        (forward-char 1)))))

(defun org-pretty-table-propertize-region (start end)
  "Apply pretty Org table display properties between START and END."
  (when (and org-pretty-table-mode
             (org-pretty-table--valid-charset-p))
    (org-pretty-table-refresh start end)))

(defun org-pretty-table-refresh (&optional start end)
  "Refresh pretty table display between START and END.

When called interactively, refresh the whole buffer."
  (interactive)
  (let ((beg (or start (point-min)))
        (fin (copy-marker (or end (point-max)))))
    (org-pretty-table--clear-overlays beg fin)
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) fin)
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (when (org-pretty-table--table-line-p line-beg)
            (org-pretty-table--fontify-line line-beg line-end)))
        (forward-line 1)))
    (set-marker fin nil)
    nil))

(defun org-pretty-table-align (oldfun &rest args)
  "Around advice for `org-table-align'.

The advice leaves the minor mode enabled and only removes overlays
owned by `org-pretty-table-mode' from the affected table."
  (if (not org-pretty-table-mode)
      (apply oldfun args)
    (let ((was-in-table (org-at-table-p))
          beg end)
      (when was-in-table
        (setq beg (org-table-begin)
              end (org-table-end))
        (when org-pretty-table-disable-while-editing
          (org-pretty-table--clear-overlays beg end)))
      (unwind-protect
          (apply oldfun args)
        (when was-in-table
          (save-excursion
            (when (org-at-table-p)
              (setq beg (org-table-begin)
                    end (org-table-end)))
            (when (and beg end)
              (org-pretty-table--clear-overlays beg end)
              (org-pretty-table-refresh beg end))))))))

;;; Minor mode

;;;###autoload
(define-minor-mode org-pretty-table-mode
  "Display Org table borders with Unicode box-drawing glyphs."
  :lighter " OPT"
  :group 'org-pretty-table
  (if org-pretty-table-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq org-pretty-table-mode nil)
          (user-error "`org-pretty-table-mode' is only intended for Org buffers"))
        (unless (org-pretty-table--valid-charset-p)
          (setq org-pretty-table-mode nil)
          (user-error "`org-pretty-table-charset' must contain exactly 11 characters"))
        (jit-lock-register #'org-pretty-table-propertize-region t)
        (org-pretty-table-refresh))
    (jit-lock-unregister #'org-pretty-table-propertize-region)
    (org-pretty-table--clear-overlays (point-min) (point-max))
    (font-lock-flush)
    (font-lock-ensure)))

;;;###autoload
(defun turn-on-org-pretty-table-mode ()
  "Turn on `org-pretty-table-mode' in Org buffers."
  (when (derived-mode-p 'org-mode)
    (org-pretty-table-mode 1)))

;;;###autoload
(defun turn-off-org-pretty-table-mode ()
  "Turn off `org-pretty-table-mode'."
  (org-pretty-table-mode -1))

;;;###autoload
(define-globalized-minor-mode global-org-pretty-table-mode
  org-pretty-table-mode
  turn-on-org-pretty-table-mode
  :group 'org-pretty-table
  :predicate '(org-mode))

;; Install the advice once.  The advice itself checks the buffer-local value of
;; `org-pretty-table-mode', so it is harmless in buffers where the mode is off.
(unless (advice-member-p #'org-pretty-table-align #'org-table-align)
  (advice-add 'org-table-align :around #'org-pretty-table-align))


;; In table.el-style grid tables, make Org sentence motion behave
;; cell-wise, matching Org table expectations more closely.

;; inside table.el grid cell:
;;   org-backward-sentence → first content character of current cell
;;   org-forward-sentence  → last content character of current cell

;; elsewhere:
;;   normal Org sentence motion

(defun table-el-line-p ()
  "Return non-nil when point is on a table.el-style grid table line."
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-forward " \t" (line-end-position))
    (looking-at-p "[+|]")))

(defun table-el-cell-bounds ()
  "Return bounds of current table.el cell as (BEG . END), or nil."
  (when (and (table-el-line-p)
             (save-excursion
               (goto-char (line-beginning-position))
               (skip-chars-forward " \t" (line-end-position))
               (eq (char-after) ?|)))
    (let ((pos (point))
          beg end)
      (save-excursion
        (setq beg
              (progn
                (search-backward "|" (line-beginning-position) t)
                (1+ (point))))
        (setq end
              (progn
                (goto-char pos)
                (search-forward "|" (line-end-position) t)
                (1- (point)))))
      (cons beg end))))

(defun table-el-cell-content-bounds ()
  "Return trimmed bounds of current table.el cell as (BEG . END), or nil."
  (when-let* ((bounds (table-el-cell-bounds)))
    (let ((beg (car bounds))
          (end (cdr bounds)))
      (save-excursion
        (goto-char beg)
        (skip-chars-forward " \t" end)
        (setq beg (point))
        (goto-char end)
        (skip-chars-backward " \t" beg)
        (setq end (point))
        (cons beg end)))))

(defun org-backward-sentence-table-el-dwim (oldfun &optional arg)
  "Move to beginning of table.el cell, otherwise call OLDFUN."
  (if-let* ((bounds (table-el-cell-content-bounds)))
      (goto-char (car bounds))
    (funcall oldfun arg)))

(defun org-forward-sentence-table-el-dwim (oldfun &optional arg)
  "Move to end of table.el cell, otherwise call OLDFUN."
  (if-let* ((bounds (table-el-cell-content-bounds)))
      (goto-char (cdr bounds))
    (funcall oldfun arg)))

(unless (advice-member-p #'org-backward-sentence-table-el-dwim
                         #'org-backward-sentence)
  (advice-add 'org-backward-sentence
              :around #'org-backward-sentence-table-el-dwim))

(unless (advice-member-p #'org-forward-sentence-table-el-dwim
                         #'org-forward-sentence)
  (advice-add 'org-forward-sentence
              :around #'org-forward-sentence-table-el-dwim))


;; Extend Org TAB navigation to table.el-style grid tables by
;; making `org-cycle' move between cells instead of folding.

(defun org-pretty-table--table-el-p ()
  "Return non-nil when point is in a table.el table."
  (and (fboundp 'org-at-table.el-p)
       (org-at-table.el-p)))

(defun org-pretty-table--table-el-row-p (&optional position)
  "Return non-nil when POSITION, or point, is on a table.el row."
  (save-excursion
    (when position
      (goto-char position))
    (and (org-pretty-table--table-el-p)
         (save-excursion
           (goto-char (line-beginning-position))
           (skip-chars-forward " \t" (line-end-position))
           (memq (char-after)
                 (list org-pretty-table--pipe
                       org-pretty-table--plus))))))

(defun org-pretty-table--table-el-hline-p (&optional position)
  "Return non-nil when POSITION, or point, is on a table.el hline."
  (save-excursion
    (when position
      (goto-char position))
    (and (org-pretty-table--table-el-row-p)
         (save-excursion
           (goto-char (line-beginning-position))
           (skip-chars-forward " \t" (line-end-position))
           (eq (char-after) org-pretty-table--plus)))))

(defun org-pretty-table--first-cell-on-line ()
  "Return the first content-start position on the current line, or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (search-forward "|" (line-end-position) t)
      (skip-chars-forward " \t" (line-end-position))
      (when (< (point) (line-end-position))
        (point)))))

(defun org-pretty-table--cell-starts-on-line ()
  "Return a list of content-start positions for cells on the current line."
  (let ((starts nil)
        (line-end (line-end-position)))
    (save-excursion
      (goto-char (line-beginning-position))
      (while (search-forward "|" line-end t)
        (skip-chars-forward " \t" line-end)
        (when (< (point) line-end)
          (push (point) starts))))
    (nreverse starts)))

(defun org-pretty-table-next-cell ()
  "Move point to the next cell in a table.el table."
  (interactive)
  (unless (org-pretty-table--table-el-row-p)
    (user-error "Not on a table.el row"))
  (let ((line-end (line-end-position)))
    (if (search-forward "|" line-end t)
        (skip-chars-forward " \t" line-end)
      (let ((target
             (save-excursion
               (forward-line 1)
               (while (and (not (eobp))
                           (org-pretty-table--table-el-hline-p))
                 (forward-line 1))
               (when (org-pretty-table--table-el-row-p)
                 (org-pretty-table--first-cell-on-line)))))
        (if target
            (goto-char target)
          (user-error "No next table cell"))))))

(defun org-pretty-table-previous-cell ()
  "Move point to the previous cell in a table.el table."
  (interactive)
  (unless (org-pretty-table--table-el-row-p)
    (user-error "Not on a table.el row"))
  (let* ((pos (point))
         (starts (org-pretty-table--cell-starts-on-line))
         (prev (car (last (seq-filter (lambda (p) (< p pos)) starts)))))
    (cond
     (prev
      (goto-char prev))
     (t
      (let ((target
             (save-excursion
               (forward-line -1)
               (while (and (not (bobp))
                           (org-pretty-table--table-el-hline-p))
                 (forward-line -1))
               (when (org-pretty-table--table-el-row-p)
                 (let ((prev-row-starts
                        (org-pretty-table--cell-starts-on-line)))
                   (car (last prev-row-starts)))))))
        (if target
            (goto-char target)
          (user-error "No previous table cell")))))))

(defun org-pretty-table-cycle-dwim (oldfun &rest args)
  "Make `org-cycle' move by cell inside table.el tables."
  (if (org-pretty-table--table-el-p)
      (org-pretty-table-next-cell)
    (apply oldfun args)))

(unless (advice-member-p #'org-pretty-table-cycle-dwim #'org-cycle)
  (advice-add 'org-cycle :around #'org-pretty-table-cycle-dwim))

(defun org-pretty-table-shifttab-dwim (oldfun &rest args)
  "Make `org-shifttab' move backward by cell inside table.el tables."
  (if (org-pretty-table--table-el-p)
      (org-pretty-table-previous-cell)
    (apply oldfun args)))

(unless (advice-member-p #'org-pretty-table-shifttab-dwim #'org-shifttab)
  (advice-add 'org-shifttab :around #'org-pretty-table-shifttab-dwim))

(provide 'org-pretty-table)
;;; org-pretty-table.el ends here

; LocalWords:  hlines hline Matus Goljer jit Charset charset
