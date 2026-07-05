;;; biorhythm.el --- Personal chronology and biorhythm utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides simple personal chronology calculations based on a user's
;; birth date, including elapsed days since birth, ordinal day-on-earth
;; calculations, and traditional biorhythm indicators.
;;
;; Biorhythms were popularized during the twentieth century as a means
;; of representing recurring physical, emotional, and intellectual
;; cycles. While not supported by modern scientific evidence, they
;; remain of historical and recreational interest.
;;
;; Example output:
;;
;;   Phy=+087%! Emo=-012%- Int=+043%+
;;
;; where '+' indicates an ascending cycle, '-' a descending cycle,
;; and '!' a cycle crossing the zero line (a traditional "critical day").
;;
;; The package also provides helper functions for reporting elapsed
;; lifetime measurements such as days since birth and day-on-earth
;; counts.

;;; Code:
(require 'calendar)
(defcustom user-birthdate nil
  "User's birth date.

The value is stored as a Gregorian date list in the form (MONTH DAY YEAR),
as used internally by Emacs calendar functions.  Interactive date entry
may respect `calendar-date-style', but the stored value uses Emacs'
calendar date representation."
  :type '(choice
          (const :tag "Unset" nil)
          (sexp :tag "Gregorian date"))
  :group 'calendar)

(defun days-since-birthdate (&optional date)
  "Return elapsed days since `user-birthdate'."
  (- (calendar-absolute-from-gregorian
      (or date (calendar-current-date)))
     (calendar-absolute-from-gregorian user-birthdate)))

(defun days-on-earth (&optional date)
  "Return ordinal day on Earth since `user-birthdate'."
  (1+ (days-since-birthdate date)))

(defun weeks-on-earth (&optional date)
  "Return the number of complete weeks elapsed since `user-birthdate'.

DATE is a Gregorian date in (MONTH DAY YEAR) form and defaults
to the current date."
  (/ (days-since-birthdate date) 7))

(defun years-on-earth (&optional date)
  "Return elapsed years since `user-birthdate'.

DATE is a Gregorian date in (MONTH DAY YEAR) form and defaults
to the current date.  A year is the mean Gregorian year of
365.2425 days."
  (/ (days-since-birthdate date) 365.2425))

(defun biorhythm-string ()
  "Return today's biorhythm as a string.

Set variable `user-birthdate' in format `(MONTH DAY YEAR)'."
  (let* ((diff (days-since-birthdate))
         (rhythms '((Phy . 23)
                    (Emo . 28)
                    (Int . 33))))
    (mapconcat
     #'identity
     (mapcar
      (lambda (r)
        (let* ((name (car r))
               (period (cdr r))
               (angle (* 2 pi diff (/ 1.0 period)))
               (next-angle (* 2 pi (1+ diff) (/ 1.0 period)))
               (today (sin angle))
               (tomorrow (sin next-angle))
               (pct (round (* 100 today)))
               (crossing-p (not (eq (>= today 0)
                                    (>= tomorrow 0))))
               (dir (cond
                     (crossing-p "!")
                     ((>= (cos angle) 0) "+")
                     (t "-"))))
          (format "%s=%+04d%%%s" name pct dir)))
      rhythms)
     " ")))

(defun biorhythm (&optional insert)
  "Show today's biorhythm.

Uses `user-birthdate' as the birth date.  `user-birthdate' should be
a Gregorian date list in the form (MONTH DAY YEAR), as used internally
by Emacs calendar functions.  Display follows the user's calendar
settings where possible.

With prefix argument INSERT, insert the result at point instead of
displaying it in the echo area."
  (interactive "P")
  (let ((s (biorhythm-string)))
    (if insert
        (insert "- " s "\n")
      (message "%s" s))))

(provide 'biorhythm)

;;; biorhythm.el ends here
