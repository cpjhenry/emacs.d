;;; tibdate.el --- Tibetan calendar conversion via tibdate  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provide an Emacs Lisp interface to the `tibdate' command, which uses
;; @hnw/date-tibetan to convert between Gregorian dates and dates in the
;; Tibetan Phugpa calendar.
;;
;; Gregorian dates use the standard Emacs calendar representation:
;;
;;     (MONTH DAY YEAR)
;;
;; Tibetan dates are represented as:
;;
;;     (CYCLE YEAR MONTH LEAP-MONTH DAY LEAP-DAY)
;;
;; LEAP-MONTH and LEAP-DAY are boolean values.

;;; Code:

(require 'calendar)
(require 'seq)
(require 'subr-x)

(defgroup tibdate nil
  "Tibetan calendar conversion."
  :group 'calendar)

(defcustom tibdate-program "tibdate"
  "Executable used for Tibetan calendar conversion.

This may be either an executable name found in `exec-path' or an
absolute filename."
  :type 'string
  :group 'tibdate)

(defun tibdate--program ()
  "Return the full filename of `tibdate-program'.

Signal a user error when the executable cannot be found."
  (or (executable-find tibdate-program)
      (user-error "Cannot find tibdate executable: %s"
                  tibdate-program)))

(defun tibdate--call (&rest arguments)
  "Run `tibdate-program' with ARGUMENTS and return its output.

Signal an error if the command exits unsuccessfully."
  (with-temp-buffer
    (let ((status
           (apply #'process-file
                  (tibdate--program)
                  nil
                  (list t t)
                  nil
                  arguments)))
      (unless (and (integerp status) (zerop status))
        (error "tibdate failed%s%s"
               (if (integerp status)
                   (format " with status %d" status)
                 "")
               (if (string-empty-p (string-trim (buffer-string)))
                   ""
                 (format ": %s" (string-trim (buffer-string))))))
      (string-trim (buffer-string)))))

(defun tibdate--valid-gregorian-date-p (date)
  "Return non-nil when DATE is a valid Gregorian calendar date."
  (and (pcase date
         (`(,month ,day ,year)
          (and (integerp month)
               (integerp day)
               (integerp year)))
         (_ nil))
       (calendar-date-is-valid-p date)))

(defun tibdate-from-gregorian (date)
  "Return the Tibetan date corresponding to Gregorian DATE.

DATE must have the standard Emacs calendar form:

    (MONTH DAY YEAR)

The returned value has the form:

    (CYCLE YEAR MONTH LEAP-MONTH DAY LEAP-DAY)

LEAP-MONTH and LEAP-DAY are either non-nil or nil."
  (unless (tibdate--valid-gregorian-date-p date)
    (user-error "Invalid Gregorian date: %S" date))
  (pcase-let ((`(,month ,day ,year) date))
    (let* ((output
            (tibdate--call
             (format "%04d-%02d-%02d" year month day)))
           (fields (split-string output "|" t)))
      (unless (and (= (length fields) 6)
                   (seq-every-p
                    (lambda (field)
                      (string-match-p "\\`[0-9]+\\'" field))
                    fields))
        (error "Unexpected tibdate output: %S" output))
      (pcase-let
          ((`(,cycle ,tibetan-year ,tibetan-month
                    ,leap-month ,tibetan-day ,leap-day)
            (mapcar #'string-to-number fields)))
        (list cycle
              tibetan-year
              tibetan-month
              (= leap-month 1)
              tibetan-day
              (= leap-day 1))))))

(defun tibdate-losar (cycle year)
  "Return the Gregorian date of Losar in Tibetan CYCLE and YEAR.

CYCLE is a Rabjung cycle number.  YEAR is a year from 1 through
60 within that cycle.

The result has the standard Emacs calendar form:

    (MONTH DAY YEAR)"
  (unless (and (integerp cycle) (> cycle 0))
    (user-error "Invalid Tibetan cycle: %S" cycle))
  (unless (and (integerp year) (<= 1 year 60))
    (user-error "Invalid Tibetan year: %S" year))
  (let ((output
         (tibdate--call
          "--losar"
          (number-to-string cycle)
          (number-to-string year))))
    (unless
        (string-match
         (rx string-start
             (group (+ digit)) "-"
             (group (= 2 digit)) "-"
             (group (= 2 digit))
             string-end)
         output)
      (error "Unexpected tibdate output: %S" output))
    (let ((date
           (list
            (string-to-number (match-string 2 output))
            (string-to-number (match-string 3 output))
            (string-to-number (match-string 1 output)))))
      (unless (calendar-date-is-valid-p date)
        (error "tibdate returned an invalid Gregorian date: %S"
               date))
      date)))

(provide 'tibdate)

;;; tibdate.el ends here

; LocalWords:  tibdate hnw tibetan losar
