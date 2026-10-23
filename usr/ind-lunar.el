;;; ind-lunar.el --- Lunar zodiac calculations -*- lexical-binding: t; -*-

;;; Commentary:

;; Calculate the Moon's geocentric tropical ecliptic longitude
;; and its position within the twelve zodiacal signs.
;;
;; Uses a low-precision lunar orbit model with perturbation terms.
;; Suitable for daily zodiacal displays, but not for precise
;; astronomical ephemerides or ingress predictions.
;;
;; No network access or external dependencies.

;;; Code:

(require 'calendar)
(require 'cal-dst)

(defconst ind-lunar--signs
  [("Aries"       . "♈")
   ("Taurus"      . "♉")
   ("Gemini"      . "♊")
   ("Cancer"      . "♋")
   ("Leo"         . "♌")
   ("Virgo"       . "♍")
   ("Libra"       . "♎")
   ("Scorpio"     . "♏")
   ("Sagittarius" . "♐")
   ("Capricorn"   . "♑")
   ("Aquarius"    . "♒")
   ("Pisces"      . "♓")]
  "Tropical zodiacal signs and their symbols.")

(defun ind-lunar--normalize (angle)
  "Normalize ANGLE to the interval [0, 360)."
  (mod angle 360.0))

(defun ind-lunar--sin (angle)
  "Return sine of ANGLE expressed in degrees."
  (sin (* float-pi (/ angle 180.0))))

(defun ind-lunar--cos (angle)
  "Return cosine of ANGLE expressed in degrees."
  (cos (* float-pi (/ angle 180.0))))

(defun ind-lunar--atan2 (y x)
  "Return angle of vector (X,Y), in degrees."
  (* (/ 180.0 float-pi) (atan y x)))

(defun ind-lunar-longitude (jd)
  "Calculate approximate lunar tropical longitude for Julian date JD.

JD is a Julian Day expressed as a floating-point number.
The result is an angle in degrees in the interval [0, 360).

The calculation uses a simplified lunar orbit with the principal
perturbations due to the Sun."
  (let* ((d (- jd 2451543.5))

         ;; Lunar orbital elements.
         (node (ind-lunar--normalize
                (- 125.1228 (* 0.0529538083 d))))
         (inclination 5.1454)
         (perigee (ind-lunar--normalize
                   (+ 318.0634 (* 0.1643573223 d))))
         (eccentricity 0.054900)
         (mean-anomaly
          (ind-lunar--normalize
           (+ 115.3654 (* 13.0649929509 d))))

         ;; Solve Kepler's equation approximately.
         (eccentric-anomaly
          (+ mean-anomaly
             (* (/ 180.0 float-pi)
                eccentricity
                (ind-lunar--sin mean-anomaly)
                (+ 1.0
                   (* eccentricity
                      (ind-lunar--cos mean-anomaly))))))

         (x (- (ind-lunar--cos eccentric-anomaly)
               eccentricity))
         (y (* (sqrt (- 1.0 (* eccentricity eccentricity)))
               (ind-lunar--sin eccentric-anomaly)))

         (true-anomaly (ind-lunar--atan2 y x))
         (argument (+ true-anomaly perigee))

         ;; Ecliptic rectangular coordinates.
         (xe (- (* (ind-lunar--cos node)
                   (ind-lunar--cos argument))
                (* (ind-lunar--sin node)
                   (ind-lunar--sin argument)
                   (ind-lunar--cos inclination))))
         (ye (+ (* (ind-lunar--sin node)
                   (ind-lunar--cos argument))
                (* (ind-lunar--cos node)
                   (ind-lunar--sin argument)
                   (ind-lunar--cos inclination))))

         (longitude
          (ind-lunar--normalize
           (ind-lunar--atan2 ye xe)))

         ;; Solar orbital elements.
         (solar-perigee
          (ind-lunar--normalize
           (+ 282.9404 (* 0.0000470935 d))))
         (solar-anomaly
          (ind-lunar--normalize
           (+ 356.0470 (* 0.9856002585 d))))
         (solar-longitude
          (ind-lunar--normalize
           (+ solar-perigee solar-anomaly
              (* 1.915 (ind-lunar--sin solar-anomaly))
              (* 0.020 (ind-lunar--sin (* 2 solar-anomaly))))))

         ;; Lunar mean longitude and elongation.
         (mean-longitude
          (ind-lunar--normalize
           (+ node perigee mean-anomaly)))
         (elongation
          (ind-lunar--normalize
           (- mean-longitude solar-longitude)))
         (argument-latitude
          (ind-lunar--normalize
           (+ mean-longitude (- node)))))

    ;; Principal lunar longitude perturbations.
    (ind-lunar--normalize
     (+ longitude
        (* -1.274
           (ind-lunar--sin
            (- mean-anomaly (* 2 elongation))))
        (* 0.658
           (ind-lunar--sin (* 2 elongation)))
        (* -0.186
           (ind-lunar--sin solar-anomaly))
        (* -0.059
           (ind-lunar--sin
            (- (* 2 mean-anomaly)
               (* 2 elongation))))
        (* -0.057
           (ind-lunar--sin
            (- mean-anomaly
               (* 2 elongation)
               solar-anomaly)))
        (* 0.053
           (ind-lunar--sin
            (+ mean-anomaly (* 2 elongation))))
        (* 0.046
           (ind-lunar--sin
            (- (* 2 elongation) solar-anomaly)))
        (* 0.041
           (ind-lunar--sin
            (- mean-anomaly solar-anomaly)))
        (* -0.035
           (ind-lunar--sin elongation))
        (* -0.031
           (ind-lunar--sin
            (+ mean-anomaly solar-anomaly)))
        (* -0.015
           (ind-lunar--sin
            (- (* 2 argument-latitude)
               (* 2 elongation))))
        (* 0.011
           (ind-lunar--sin
            (- mean-anomaly
               (* 4 elongation))))))))

(defun ind-lunar-zodiac (jd)
  "Return lunar zodiacal information for Julian date JD.

The result is a plist containing :sign, :symbol, :degree,
:minute, and :longitude."
  (let* ((longitude (ind-lunar-longitude jd))
         (index (floor (/ longitude 30.0)))
         (position (- longitude (* index 30.0)))
         (minutes (floor (+ (* position 60.0) 0.5)))
         (degree (/ minutes 60))
         (minute (% minutes 60)))

    ;; Handle rounding across a sign boundary.
    (when (= degree 30)
      (setq index (mod (1+ index) 12)
            degree 0
            minute 0))

    (let ((sign (aref ind-lunar--signs index)))
      (list :sign (car sign)
            :symbol (cdr sign)
            :degree degree
            :minute minute
            :longitude longitude))))

(defun ind-format-lunar-zodiac (jd)
  "Format lunar zodiacal position for Julian date JD."
  (let ((zodiac (ind-lunar-zodiac jd)))
    (format "%s %s %d°%02d′" ; ☽
	    (plist-get zodiac :symbol)
            (plist-get zodiac :sign)
            (plist-get zodiac :degree)
            (plist-get zodiac :minute))))

(defun ind-lunar-local-noon (&optional date)
  "Return lunar zodiacal position at local noon on DATE.

DATE is a Gregorian date in the form (MONTH DAY YEAR).
When omitted, use today's date."
  (let* ((date (or date (calendar-current-date)))
         (absolute (calendar-absolute-from-gregorian date))
         (jd (+ (calendar-astro-from-absolute absolute) 0.5))
         (zone (/ (float (car (current-time-zone
                              (encode-time
                               (list 0 0 12
                                     (nth 1 date)
                                     (nth 0 date)
                                     (nth 2 date))))))
                  86400.0)))
    (ind-format-lunar-zodiac (+ jd (- zone)))))

(provide 'ind-lunar)

;;; ind-lunar.el ends here
