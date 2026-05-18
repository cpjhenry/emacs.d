;; Biorhythms
;; https://rosettacode.org/wiki/Biorhythms
(defvar birthdate)
(defun biorhythm ()
  "Show today's biorhythm. Set variable `birthdate' in format MDY."
  (interactive)
  (let* ((diff (abs (- (string-to-number (calendar-astro-date-string birthdate))
		       (string-to-number (calendar-astro-date-string)))))
         (rhyt '(23 28 33))
         (perc (mapcar (lambda (x) (round (* 100 (sin
               (* 2 pi diff (/ 1.0 x)))))) rhyt)))
    (message "age: %i  physical: %i%%  emotional: %i%%  intellectual: %i%%"
             diff (car perc) (cadr perc) (caddr perc))))
