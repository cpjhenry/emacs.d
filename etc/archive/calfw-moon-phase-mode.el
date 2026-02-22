;;; calfw-moon-phase-mode.el --- Moon phase annotations for Calfw (MELPA) -*- lexical-binding: t; -*-
;;
;; Assumes you have already defined:
;;   (setq lunar-phase-names '("🌑" "🌓" "🌕" "🌗"))
;;
;; MELPA calfw expects source data keys to be *absolute day numbers* (integers),
;; even though BEGIN/END may be passed as Gregorian lists (m d y).
;;
;; Usage:
;;   (require 'calfw-moon-phase-mode)
;;   (calfw-moon-phase-global-mode 1)
;;   (calfw-open-calendar-buffer)

(require 'cl-lib)
(require 'calendar)
(require 'lunar)
(require 'calfw)

(defgroup calfw-moon-phase nil
  "Annotate calfw calendars with lunar phases."
  :group 'calendar
  :prefix "calfw-moon-phase-")

(defcustom calfw-moon-phase-which '(new first-quarter full last-quarter)
  "Which phases to display."
  :type '(set (const new)
              (const first-quarter)
              (const full)
              (const last-quarter)))

(defcustom calfw-moon-phase-prefix " "
  "String inserted before the moon icon."
  :type 'string)

(defcustom calfw-moon-phase-face 'default
  "Face applied to the moon icon."
  :type 'face)

(defcustom calfw-moon-phase-cache-enabled t
  "Whether to cache computed phase names per date."
  :type 'boolean)

(defvar calfw-moon-phase--cache (make-hash-table :test 'equal))

(defun calfw-moon-phase-clear-cache ()
  "Clear cached moon phase computations."
  (interactive)
  (clrhash calfw-moon-phase--cache))

(defun calfw-moon-phase--icon-list ()
  "Return the lunar phase icon list from `lunar-phase-names`."
  (unless (and (boundp 'lunar-phase-names)
               (listp lunar-phase-names)
               (= (length lunar-phase-names) 4)
               (cl-every #'stringp lunar-phase-names))
    (user-error "Expected `lunar-phase-names` = (\"🌑\" \"🌓\" \"🌕\" \"🌗\")"))
  lunar-phase-names)

(defun calfw-moon-phase--cache-key (date)
  "DATE is (month day year)."
  (list (nth 0 date) (nth 1 date) (nth 2 date)))

(defun calfw-moon-phase--phase-name (date)
  "Return phase name string for DATE (month day year), optionally cached."
  (let ((key (calfw-moon-phase--cache-key date)))
    (if (and calfw-moon-phase-cache-enabled (gethash key calfw-moon-phase--cache))
        (gethash key calfw-moon-phase--cache)
      (let* ((res  (lunar-phase date))  ;; => (PHASE-NAME ILLUMINATION)
             (name (car res)))
        (when calfw-moon-phase-cache-enabled
          (puthash key name calfw-moon-phase--cache))
        name))))

(defun calfw-moon-phase--phase-symbol (phase-name)
  "Map `lunar-phase`'s phase name string to a phase symbol."
  (cond
   ((and phase-name (string-match-p "New Moon" phase-name)) 'new)
   ((and phase-name (string-match-p "First Quarter" phase-name)) 'first-quarter)
   ((and phase-name (string-match-p "Full Moon" phase-name)) 'full)
   ((and phase-name (string-match-p "Last Quarter" phase-name)) 'last-quarter)
   (t nil)))

(defun calfw-moon-phase--icon-for (phase-symbol)
  "Return icon string for PHASE-SYMBOL using `lunar-phase-names` ordering."
  (let ((icons (calfw-moon-phase--icon-list)))
    (pcase phase-symbol
      ('new           (nth 0 icons))
      ('first-quarter (nth 1 icons))
      ('full          (nth 2 icons))
      ('last-quarter  (nth 3 icons))
      (_ nil))))

(defun calfw-moon-phase--annotation-for-date (date)
  "Return a propertized moon icon string or nil for DATE (m d y)."
  (let* ((pname (calfw-moon-phase--phase-name date))
         (psym  (calfw-moon-phase--phase-symbol pname)))
    (when (and psym (memq psym calfw-moon-phase-which))
      (let ((icon (calfw-moon-phase--icon-for psym)))
        (when (and (stringp icon) (> (length icon) 0))
          (propertize (concat calfw-moon-phase-prefix icon)
                      'face calfw-moon-phase-face))))))

(defun calfw-moon-phase--to-gregorian (x)
  "Convert X to a Gregorian date list (month day year).
X may be:
- Gregorian list (m d y)
- absolute day number
- vector [m d y]
- wrapper like ((m d y))"
  (cond
   ((numberp x)
    (calendar-gregorian-from-absolute x))
   ((and (vectorp x) (= (length x) 3)
         (numberp (aref x 0)) (numberp (aref x 1)) (numberp (aref x 2)))
    (list (aref x 0) (aref x 1) (aref x 2)))
   ((and (consp x) (consp (car x)) (null (cdr x)))
    (calfw-moon-phase--to-gregorian (car x)))
   ((and (consp x) (= (length x) 3) (cl-every #'numberp x))
    x)
   (t
    (error "calfw-moon-phase: unexpected date/range value: %S" x))))

(defun calfw-moon-phase--data (begin end)
  "Calfw source :data callback.

IMPORTANT (MELPA calfw): the returned alist must be keyed by *absolute day
numbers* (integers), not Gregorian date lists.

BEGIN/END may be Gregorian lists (m d y) or absolute integers."
  (let* ((b  (calfw-moon-phase--to-gregorian begin))
         (e  (calfw-moon-phase--to-gregorian end))
         (ab (calendar-absolute-from-gregorian b))
         (ae (calendar-absolute-from-gregorian e))
         (lo (if (< ab ae) ab ae))
         (hi (if (< ab ae) ae ab))
         (a lo)
         (ret nil))
    (while (<= a hi)
      (let* ((date (calendar-gregorian-from-absolute a)) ;; (m d y) for lunar-phase
             (ann  (calfw-moon-phase--annotation-for-date date)))
        (when ann
          (push (cons a (list ann)) ret)))
      (setq a (1+ a)))
    (nreverse ret)))

(defun calfw-moon-phase--make-source ()
  "Create a fresh calfw-source for moon annotations."
  (make-calfw-source
   :name "Moon"
   :color "Gray50"
   :data #'calfw-moon-phase--data))

(defun calfw-moon-phase--ensure-in-list (sources)
  "Add our Moon source to SOURCES unless a \"Moon\" source is already present."
  (let* ((sources (if (listp sources) sources nil))
         (already (cl-find-if (lambda (s)
                                (and (calfw-source-p s)
                                     (string= (calfw-source-name s) "Moon")))
                              sources)))
    (if already
        sources
      (cons (calfw-moon-phase--make-source) sources))))

(defun calfw-moon-phase--advice-open (orig-fn &rest args)
  "Around-advice for `calfw-open-calendar-buffer` to inject :annotation-sources."
  (let* ((plist (copy-sequence args))
         (existing (plist-get plist :annotation-sources))
         (updated  (calfw-moon-phase--ensure-in-list existing)))
    (setq plist (plist-put plist :annotation-sources updated))
    (apply orig-fn plist)))

(defun calfw-moon-phase-rewire-existing-moon-sources ()
  "Rewrite any already-installed \"Moon\" source objects to use current data fn.
Useful if you had old sources in existing calfw buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (boundp 'calfw-component) calfw-component
                 (fboundp 'calfw-cp-get-model)
                 (fboundp 'calfw--model-get-annotation-sources))
        (let* ((model (calfw-cp-get-model calfw-component))
               (as (calfw--model-get-annotation-sources model)))
          (dolist (src (if (listp as) as nil))
            (when (and (calfw-source-p src)
                       (string= (calfw-source-name src) "Moon"))
              (setf (calfw-source-data src) #'calfw-moon-phase--data))))))))

;;;###autoload
(define-minor-mode calfw-moon-phase-global-mode
  "Global mode: add moon phase icons to Calfw via :annotation-sources.
Assumes `lunar-phase-names` is (\"🌑\" \"🌓\" \"🌕\" \"🌗\")."
  :global t
  :init-value nil
  :lighter " 🌙"
  :group 'calfw-moon-phase
  (if calfw-moon-phase-global-mode
      (progn
        (calfw-moon-phase-clear-cache)
        (advice-add 'calfw-open-calendar-buffer :around #'calfw-moon-phase--advice-open))
    (advice-remove 'calfw-open-calendar-buffer #'calfw-moon-phase--advice-open)))

;;;###autoload
(defun calfw-moon-phase-hard-reset ()
  "Remove and re-add the calfw-open-calendar-buffer advice and rewire existing buffers."
  (interactive)
  (advice-remove 'calfw-open-calendar-buffer #'calfw-moon-phase--advice-open)
  (calfw-moon-phase-clear-cache)
  (when calfw-moon-phase-global-mode
    (advice-add 'calfw-open-calendar-buffer :around #'calfw-moon-phase--advice-open))
  (calfw-moon-phase-rewire-existing-moon-sources)
  (message "calfw-moon-phase: reset complete"))

(provide 'calfw-moon-phase-mode)
;;; calfw-moon-phase-mode.el ends here
