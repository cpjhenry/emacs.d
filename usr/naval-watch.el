;;; naval-watch.el --- Traditional naval watches and bells -*- lexical-binding: t; -*-

;;; Commentary:

;; Determine the traditional naval watch and most recently sounded
;; ship's bells using local civil time.
;;
;; The dog watches divide 16:00–20:00 into two two-hour watches.
;; Bell calculations follow the traditional British convention.
;;
;; Examples:
;;
;;   (naval-watch)
;;   (naval-watch 16)
;;   (naval-watches (current-time))
;;   (naval-bells (current-time))

;;; Code:
(defun naval-watches (&optional time)
  "Return the traditional naval watch for local TIME.

TIME defaults to the current time.  The two dog watches
divide the period from 16:00 to 20:00."
  (let ((hour (decoded-time-hour (decode-time time))))
    (cond
     ((< hour 4)  "Middle Watch")
     ((< hour 8)  "Morning Watch")
     ((< hour 12) "Forenoon Watch")
     ((< hour 16) "Afternoon Watch")
     ((< hour 18) "First Dog Watch")
     ((< hour 20) "Last Dog Watch")
     (t           "First Watch"))))

(defun naval-bells (&optional time)
  "Return the most recently sounded ship's bells for local TIME.

TIME defaults to the current time.  Uses the traditional
British convention for the Last Dog Watch."
  (let* ((dt (decode-time time))
         (hour (decoded-time-hour dt))
         (minute (decoded-time-minute dt)))
    (cond
     ;; Eight bells at the end of a four-hour watch.
     ((and (= (mod hour 4) 0) (< minute 30))
      8)
     ;; Last Dog Watch: four, one, two, three bells.
     ((and (>= hour 18) (< hour 20))
      (pcase (+ (* (- hour 18) 2) (/ minute 30))
        (0 4)
        (1 1)
        (2 2)
        (3 3)))
     ;; Ordinary watches, including the First Dog Watch.
     (t (+ (* (mod hour 4) 2)
           (/ minute 30))))))

;;;###autoload
(defun naval-watch (&optional time)
  "Display the traditional naval watch and bells for local TIME.

TIME defaults to the current time.  An integer from 0 to 23
specifies that hour today, with minutes and seconds set to zero.

When called interactively, display the result and copy it
to the kill ring."
  (interactive)
  (let* ((time (cond
                ((null time) (current-time))
                ((and (integerp time) (<= 0 time 23))
                 (let ((dt (decode-time)))
                   (setf (decoded-time-hour dt) time
                         (decoded-time-minute dt) 0
                         (decoded-time-second dt) 0)
                   (encode-time dt)))
                (t time)))
         (watch (naval-watches time))
         (bells (naval-bells time))
         (result (format "%s; %d %s"
                         watch bells
                         (if (= bells 1) "bell" "bells"))))
    (when (called-interactively-p 'interactive)
      (kill-new result)
      (message "%s" result))
    result))

(provide 'naval-watch)

;;; naval-watch.el ends here
