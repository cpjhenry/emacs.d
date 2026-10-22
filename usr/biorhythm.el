;;; biorhythm.el --- Personal chronology and biorhythm utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Personal chronology calculations based on a user's birth date,
;; including elapsed days, ordinal day-on-earth calculations, and
;; traditional physical, emotional, and intellectual biorhythms.
;;
;; Biorhythms are an exercise in mathematically rigorous whimsy:
;; the calculations are deterministic, but their correspondence
;; to human experience remains scientifically unsubstantiated.
;;
;; Commands:
;;
;;   M-x biorhythm             Display today's biorhythm.
;;   C-u M-x biorhythm         Insert today's biorhythm at point.
;;   M-x biorhythm-display     Display a 31-day SVG graph.
;;   M-x biorhythm-insert      Insert an SVG graph at point.
;;   M-x biorhythm-export-svg  Export the graph to an SVG file.
;;
;; The graphical functions accept optional Gregorian dates and
;; dimensions for use by other Emacs Lisp packages.

;;; Code:

(require 'calendar)
(require 'svg)

(defgroup biorhythm nil
  "Personal chronology and traditional biorhythm calculations."
  :group 'calendar)

(defcustom user-birthdate nil
  "User's birth date as a Gregorian date (MONTH DAY YEAR)."
  :type '(choice
          (const :tag "Unset" nil)
          (sexp :tag "Gregorian date"))
  :group 'biorhythm)

(defcustom biorhythm-graph-days 31
  "Number of days displayed in a biorhythm graph."
  :type '(integer 2)
  :group 'biorhythm)

(defcustom biorhythm-graph-width 500
  "Width of a biorhythm graph in pixels."
  :type '(integer 100)
  :group 'biorhythm)

(defcustom biorhythm-graph-height 250
  "Height of a biorhythm graph in pixels."
  :type '(integer 100)
  :group 'biorhythm)

(defconst biorhythm-cycles
  '((Phy 23 "red")
    (Emo 28 "blue")
    (Int 33 "#008000"))
  "Biorhythm cycle names, periods, and graph colours.")

;;; Personal chronology

(defun days-since-birthdate (&optional date)
  "Return elapsed days since `user-birthdate' to Gregorian DATE."
  (unless user-birthdate
    (user-error "Set `user-birthdate' first"))
  (- (calendar-absolute-from-gregorian
      (or date (calendar-current-date)))
     (calendar-absolute-from-gregorian user-birthdate)))

(defun days-on-earth (&optional date)
  "Return ordinal day on Earth for Gregorian DATE."
  (1+ (days-since-birthdate date)))

(defun weeks-on-earth (&optional date)
  "Return complete weeks elapsed since birth on Gregorian DATE."
  (/ (days-since-birthdate date) 7))

(defun years-on-earth (&optional date)
  "Return elapsed mean Gregorian years since birth on DATE."
  (/ (days-since-birthdate date) 365.2425))

;;; Biorhythm calculations

(defun biorhythm--value (days period)
  "Return biorhythm value for DAYS elapsed and cycle PERIOD."
  (sin (* 2 pi (/ (float days) period))))

(defun biorhythm-values (&optional date)
  "Return biorhythm values for Gregorian DATE.

DATE defaults to today.  Return an alist mapping Phy, Emo,
and Int to floating-point values between -1 and 1."
  (let ((days (days-since-birthdate date)))
    (mapcar
     (lambda (cycle)
       (cons (car cycle)
             (biorhythm--value days (cadr cycle))))
     biorhythm-cycles)))

(defun biorhythm-string (&optional date fontify)
  "Return formatted biorhythm indicators for Gregorian DATE.

DATE defaults to today.  A plus sign indicates an ascending
cycle, a minus sign a descending cycle, and an exclamation
mark a zero crossing before the following day.

When FONTIFY is non-nil, colour each indicator to match its
corresponding SVG curve."
  (let ((days (days-since-birthdate date)))
    (mapconcat
     (lambda (cycle)
       (let* ((name (car cycle))
              (period (cadr cycle))
              (colour (caddr cycle))
              (angle (* 2 pi (/ (float days) period)))
              (today (sin angle))
              (tomorrow (biorhythm--value (1+ days) period))
              (pct (round (* 100 today)))
              (crossing-p
               (not (eq (>= today 0)
                        (>= tomorrow 0))))
              (direction
               (cond
                (crossing-p "!")
                ((>= (cos angle) 0) "+")
                (t "-")))
              (label (format "%s=%+04d%%%s"
                             name pct direction)))
         (if fontify
             (propertize label 'face `(:foreground ,colour))
           label)))
     biorhythm-cycles
     " ")))

(defun biorhythm (&optional insert)
  "Show today's biorhythm.

With prefix argument INSERT, insert the result at point."
  (interactive "P")
  (let ((result (biorhythm-string)))
    (if insert
        (insert "- " result "\n")
      (message "%s" result))))

;;; SVG graphing

(defun biorhythm-svg (&optional date days width height)
  "Return an SVG biorhythm graph centred on Gregorian DATE.

DAYS specifies the number of displayed days.
WIDTH and HEIGHT specify the SVG dimensions in pixels.

The result is an SVG DOM suitable for `svg-image' or export."
  (let* ((date (or date (calendar-current-date)))
         (days (or days biorhythm-graph-days))
         (width (or width biorhythm-graph-width))
         (height (or height biorhythm-graph-height))
         (margin-left 55)
         (margin-right 25)
         (margin-top 15)
         (margin-bottom 35)
         (plot-width (- width margin-left margin-right))
         (plot-height (- height margin-top margin-bottom))
         (mid-y (+ margin-top (/ plot-height 2.0)))
         (half-height (/ plot-height 2.0))
         (centre (/ (1- days) 2.0))
         (first-day (- (calendar-absolute-from-gregorian date)
                       centre))
         (birth-day (calendar-absolute-from-gregorian
                     user-birthdate))
         (svg (svg-create width height)))

    (unless (and (integerp days)
		 (> days 1)
		 (= (% days 2) 1))
      (user-error "Biorhythm graph requires an odd number of days"))

    (unless (and (> plot-width 0) (> plot-height 0))
      (user-error "Invalid biorhythm graph dimensions"))

    ;; Background.
    (svg-rectangle svg 0 0 width height
                   :fill "white")

    ;; Horizontal reference lines.
    (dolist (level '(-100 -50 0 50 100))
      (let ((y (- mid-y (* (/ level 100.0) half-height))))
        (svg-line svg margin-left y
                  (+ margin-left plot-width) y
                  :stroke (if (= level 0) "#666666" "#dddddd")
                  :stroke-width (if (= level 0) 1.5 1))
        (svg-text svg (number-to-string level)
                  :x (- margin-left 8)
                  :y (+ y 4)
                  :text-anchor "end"
                  :font-size 11
                  :fill "#555555")))

    ;; Vertical reference-date marker.
    (let ((x (+ margin-left
                (* (/ centre (float (1- days)))
                   plot-width))))
      (svg-line svg x margin-top
                x (+ margin-top plot-height)
                :stroke "#777777"
                :stroke-width 1
                :stroke-dasharray "4,4"))

    ;; Date labels at five-day intervals, always including the centre.
    (dotimes (i days)
      (let ((reference-p (= i centre)))
	(when (or (= i 0)
		  (= i (1- days))
		  (= (% i 5) 0)
		  reference-p)
	  (let* ((absolute (+ first-day i))
		 (gregorian
		  (calendar-gregorian-from-absolute
		   (round absolute)))
		 (x (+ margin-left
                       (* (/ i (float (1- days)))
			  plot-width)))
		 (month
		  (substring
		   (aref calendar-month-name-array
			 (1- (car gregorian)))
		   0 3)))
            (svg-text
             svg
             (format "%d %s" (cadr gregorian) month)
             :x x
             :y (+ margin-top plot-height 20)
             :text-anchor "middle"
             :font-size 10
             :font-weight (if reference-p "bold" "normal")
             :fill (if reference-p "#222222" "#555555"))))))

    ;; Three biorhythm curves.
    (dolist (cycle biorhythm-cycles)
      (let* ((period (cadr cycle))
             (colour (caddr cycle))
             (points nil))
        (dotimes (i (* 4 (1- days)))
          (let* ((offset (/ i 4.0))
                 (absolute (+ first-day offset))
                 (elapsed (- absolute birth-day))
                 (value (biorhythm--value elapsed period))
                 (x (+ margin-left
                       (* (/ offset (float (1- days)))
                          plot-width)))
                 (y (- mid-y (* value half-height))))
            (push (cons x y) points)))

        ;; Include the final endpoint.
        (let* ((elapsed (- (+ first-day (1- days))
                           birth-day))
               (value (biorhythm--value elapsed period)))
          (push
           (cons (+ margin-left plot-width)
                 (- mid-y (* value half-height)))
           points))

        (svg-polyline svg (nreverse points)
                      :fill "none"
                      :stroke colour
                      :stroke-width 2.2)))

    ;; Mark the values on the reference date.
    (let* ((elapsed (days-since-birthdate date))
           (x (+ margin-left
                 (* (/ centre (float (1- days)))
                    plot-width))))
      (dolist (cycle biorhythm-cycles)
        (let* ((value (biorhythm--value
                       elapsed (cadr cycle)))
               (y (- mid-y (* value half-height))))
          (svg-circle svg x y 4
                      :fill (caddr cycle)
                      :stroke "white"
                      :stroke-width 1.5))))

    svg))

;;; Text presentation

(defun biorhythm-legend (&optional date)
  "Return a coloured biorhythm legend for Gregorian DATE."
  (biorhythm-string date t))

;;; Presentation

(defun biorhythm-insert (&optional date days)
  "Insert an SVG biorhythm graph at point.

DATE defaults to today and DAYS to `biorhythm-graph-days'."
  (interactive)
  (insert-image
   (svg-image (biorhythm-svg date days)
              :ascent 'center)
   "[Biorhythm graph]"))

(defun biorhythm-display (&optional date days)
  "Display an SVG biorhythm graph in a dedicated buffer.

DATE defaults to today and DAYS to `biorhythm-graph-days'."
  (interactive)
  (let* ((date (or date (calendar-current-date)))
         (buffer (get-buffer-create "*Biorhythm*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)

        ;; Title.
        (insert
         (propertize
          (format "Biorhythms — %s\n\n"
                  (calendar-date-string date))
          'face '(:height 1.3 :weight bold)))

        ;; Graph.
        (biorhythm-insert date days)

        ;; Legend.
        (insert "\n\n"
                (biorhythm-legend date)
                "\n")))
    (display-buffer buffer)))

;;; SVG export

(defun biorhythm-export-svg (filename &optional date days)
  "Export a biorhythm graph to FILENAME as SVG.

DATE defaults to today and DAYS to `biorhythm-graph-days'."
  (interactive
   (list (read-file-name "Export biorhythm SVG: "
                         nil "biorhythm.svg")))
  (let ((svg (biorhythm-svg date days)))
    (with-temp-file filename
      (svg-print svg)))
  (when (called-interactively-p 'interactive)
    (message "Biorhythm graph exported to %s" filename)))

;;; reverse calculation

(defun date-on-earth (days)
  "Return Gregorian date of ordinal day DAYS on Earth.

Birth is day 1, consistent with `days-on-earth'."
  (interactive "nDay on Earth: ")
  (unless user-birthdate
    (user-error "Set `user-birthdate' first"))
  (unless (and (integerp days) (> days 0))
    (user-error "DAYS must be a positive integer"))
  (let ((date
         (calendar-gregorian-from-absolute
          (+ (calendar-absolute-from-gregorian user-birthdate)
             (1- days)))))
    (when (called-interactively-p 'interactive)
      (message "Day %s on Earth: %s"
               (commify-number days) (calendar-date-string date)))
    date))

(provide 'biorhythm)

;;; biorhythm.el ends here

; LocalWords:  Phy dddddd
