;;; buddhist-observation.el --- Display Buddhist observances -*- lexical-binding: t; -*-

;; Author: cpj <cn914@ncf.ca>
;; Keywords: calendar, religion, multimedia
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display explanatory and devotional material for Theravāda Buddhist
;; observances calculated by `moon-holidays.el'.
;;
;; `moon-holidays.el' remains responsible for calendrical calculation.
;; This package associates those calculated dates with descriptive
;; records and related resources such as chant texts and recordings.
;;
;; The principal commands are:
;;
;;   M-x buddhist-observation-today
;;   M-x buddhist-observation-display
;;   M-x buddhist-observation-stop-audio
;;
;; The observation buffer provides buttons for opening texts, playing
;; associated audio, and uses the macOS `afplay' utility for
;; audio playback.
;;
;; Audio playback uses the macOS `afplay' utility.

;; The principal Theravāda Buddhist observances are presented in
;; Gregorian calendar order, following the modern Thai Buddhist Era
;; (B.E.) civil calendar, whose year begins on 1 January.
;;
;; Historically, however, the religious cycle forms a different
;; narrative. In the traditional lunisolar reckoning, the observances
;; associated with the Three Jewels unfold as:
;;
;;   Vesak   — the Buddha
;;   Asalha  — the Dhamma
;;   Magha   — the Sangha
;;
;; This sequence reflects the progression from the Enlightenment of
;; the Buddha, to the proclamation of the Dhamma in the First Sermon,
;; and finally to the gathering of the enlightened Sangha.
;;
;; The adoption of 1 January as the beginning of the Thai civil
;; Buddhist year preserves the traditional observances but changes
;; their apparent order within the calendar to:
;;
;;   Magha
;;   Vesak
;;   Asalha
;;
;; The package therefore distinguishes between calendrical calculation
;; (determining when an observance occurs) and its symbolic or
;; theological interpretation.

;; The order of presentation is therefore a matter of calendar
;; convention rather than religious precedence.

;;; Code:

(require 'button)
(require 'calendar)
(require 'moon-holidays)
(require 'subr-x)

(defgroup buddhist-observation nil
  "Display Buddhist observances and associated resources."
  :group 'calendar
  :prefix "buddhist-observation-")

(defcustom buddhist-observation-directory
  (expand-file-name "~/Documents/Buddhist/")
  "Directory containing Buddhist observation texts and recordings."
  :type 'directory
  :group 'buddhist-observation)

(defcustom buddhist-observation-audio-player "afplay"
  "Program used to play Buddhist observation audio files.

The default, `afplay', is supplied with macOS."
  :type 'string
  :group 'buddhist-observation)

(defconst buddhist-observation--buffer-name
  "*Buddhist Observation*"
  "Name of the Buddhist observation display buffer.")

(defconst buddhist-observation--audio-process-name
  "buddhist-observation-audio"
  "Name assigned to the Buddhist observation audio process.")

(defvar buddhist-observation-resources
  '((morning-chant
     :title "Short Morning Chant"
     :text "TBC Morning Chant - short.pdf"
     :audio "TBCMorningChanting.mp3"
     :description
     "Dedication of Offerings and Preliminary Homage."))
  "Resources associated with Buddhist observances.

Each entry has the form:

  (KEY :title TITLE
       :text TEXT-FILE
       :audio AUDIO-FILE
       :description DESCRIPTION)

TEXT-FILE and AUDIO-FILE are interpreted relative to
`buddhist-observation-directory'.")

(defvar buddhist-observation-data
  '((magha
     :calendar-name "Magha"
     :title "Māgha Pūjā"
     :month 2
     :offset 0
     :aspect "Sangha"
     :description
     "Commemorates the spontaneous gathering of the Buddha's \
disciples and the teaching of the principles of the Dhamma."
     :resources (morning-chant))

    (vesak
     :calendar-name "Vesak"
     :title "Vesākha Pūjā"
     :month 5
     :offset 0
     :aspect "Buddha"
     :description
     "Commemorates the birth, awakening, and final passing of the \
Buddha."
     :resources (morning-chant))

    (asalha
     :calendar-name "Asalha"
     :title "Āsāḷha Pūjā"
     :month 7
     :offset 0
     :aspect "Dhamma"
     :description
     "Commemorates the Buddha's first discourse, the setting in \
motion of the Wheel of Dhamma, and the arising of the Sangha."
     :resources (morning-chant))

    (vassa
     :calendar-name "Vassa"
     :title "Vassa"
     :month 7
     :offset 1
     :aspect "Rains Retreat"
     :description
     "Marks the beginning of the traditional three-month rains \
retreat following Āsāḷha Pūjā."
     :resources (morning-chant))

    (pavarana
     :calendar-name "Pavarana"
     :title "Pavāraṇā"
     :month 10
     :offset 0
     :aspect "Conclusion of Vassa"
     :description
     "Marks the conclusion of the rains retreat and the occasion \
on which members of the monastic community invite admonition from \
one another."
     :resources (morning-chant)))

  "Descriptive records for Theravāda Buddhist observances.

Each record has the form:

  (KEY :calendar-name NAME
       :title TITLE
       :month MONTH
       :offset OFFSET
       :aspect ASPECT
       :description DESCRIPTION
       :resources RESOURCE-KEYS)

MONTH identifies the Gregorian month whose first full moon
determines the observance.

OFFSET is the number of days after that full moon.  Vassa, for
example, has an offset of one day from Āsāḷha Pūjā.")

(defvar-local buddhist-observation--current-key nil
  "Key of the observance displayed in the current buffer.")

(defvar-local buddhist-observation--current-date nil
  "Gregorian date displayed in the current observation buffer.")

(defun buddhist-observation-get (key)
  "Return the observation record identified by KEY."
  (cdr (assq key buddhist-observation-data)))

(defun buddhist-observation-resource-get (key)
  "Return the resource record identified by KEY."
  (cdr (assq key buddhist-observation-resources)))

(defun buddhist-observation-resource-file (filename)
  "Return the absolute resource path for FILENAME."
  (expand-file-name filename buddhist-observation-directory))

(defun buddhist-observation-date (key year)
  "Return the Gregorian date of observation KEY in YEAR.

Return nil if KEY has no valid observation record or no relevant
full moon can be calculated."
  (when-let* ((record (buddhist-observation-get key))
              (month (plist-get record :month))
              (offset (plist-get record :offset))
              (full-moon
               (moon-holidays-first-full-moon month year)))
    (calendar-gregorian-from-absolute
     (+ offset
        (calendar-absolute-from-gregorian full-moon)))))

(defun buddhist-observation-for-date (date)
  "Return the observation key associated with Gregorian DATE.

Return nil when DATE is not one of the configured Buddhist
observances."
  (let ((year (calendar-extract-year date)))
    (catch 'observation
      (dolist (entry buddhist-observation-data)
        (let ((key (car entry)))
          (when (equal date
                       (buddhist-observation-date key year))
            (throw 'observation key))))
      nil)))

(defun buddhist-observation-today-key ()
  "Return the Buddhist observation key for today, or nil."
  (buddhist-observation-for-date
   (calendar-current-date)))

(defun buddhist-observation--read-key ()
  "Read and return a Buddhist observation key."
  (let ((choices
         (mapcar
          (lambda (entry)
            (let* ((key (car entry))
                   (record (cdr entry))
                   (title (plist-get record :title)))
              (cons title key)))
          buddhist-observation-data)))
    (cdr
     (assoc-string
      (completing-read "Buddhist observance: "
                       choices nil t)
      choices))))

(defun buddhist-observation--ensure-resource-file (filename)
  "Return the absolute path for resource FILENAME.

Signal a user error if the file cannot be read."
  (let ((file
         (buddhist-observation-resource-file filename)))
    (unless (file-readable-p file)
      (user-error "Resource file is not readable: %s" file))
    file))

(defun buddhist-observation-open-file (filename)
  "Open Buddhist observation resource FILENAME."
  (find-file
   (buddhist-observation--ensure-resource-file filename)))

(defun buddhist-observation-audio-process ()
  "Return the current Buddhist observation audio process.

Return nil if no live audio process exists."
  (let ((process
         (get-process
          buddhist-observation--audio-process-name)))
    (and (process-live-p process)
         process)))

(defun buddhist-observation-play-audio (filename)
  "Play Buddhist observation audio FILENAME.

FILENAME is interpreted relative to
`buddhist-observation-directory'."
  (interactive
   (list
    (file-relative-name
     (read-file-name
      "Audio file: "
      buddhist-observation-directory
      nil t nil
      (lambda (file)
        (or (file-directory-p file)
            (string-match-p
             (rx "." (or "mp3" "m4a" "wav" "aiff")
                 string-end)
             file))))
     buddhist-observation-directory)))
  (unless (executable-find
           buddhist-observation-audio-player)
    (user-error "Audio player is not available: %s"
                buddhist-observation-audio-player))
  (let ((file
         (buddhist-observation--ensure-resource-file
          filename)))
    (when-let ((process
                (buddhist-observation-audio-process)))
      (delete-process process))
    (let ((process
           (start-process
            buddhist-observation--audio-process-name
            nil
            buddhist-observation-audio-player
            file)))
      (set-process-query-on-exit-flag process nil)
      (message "Playing %s"
               (file-name-nondirectory file)))))

(defun buddhist-observation-stop-audio ()
  "Stop the current Buddhist observation audio playback."
  (interactive)
  (if-let ((process
            (buddhist-observation-audio-process)))
      (progn
        (delete-process process)
        (message "Buddhist observation audio stopped"))
    (user-error "No Buddhist observation audio is playing")))

(defun buddhist-observation--insert-heading (heading character)
  "Insert HEADING underlined with CHARACTER."
  (insert heading "\n"
          (make-string (string-width heading)
                       character)
          "\n"))

(defun buddhist-observation--insert-resource-button
    (label filename action)
  "Insert a resource button displaying LABEL.

FILENAME is passed to ACTION when the button is activated."
  (insert-text-button
   label
   'follow-link t
   'help-echo
   (buddhist-observation-resource-file filename)
   'action
   (lambda (_button)
     (funcall action filename))))

(defun buddhist-observation--insert-resource (key)
  "Insert the Buddhist observation resource identified by KEY."
  (when-let ((resource
              (buddhist-observation-resource-get key)))
    (let ((title (plist-get resource :title))
          (description
           (plist-get resource :description))
          (text (plist-get resource :text))
          (audio (plist-get resource :audio)))
      (insert "\n")
      (buddhist-observation--insert-heading title ?-)
      (when description
        (insert description "\n\n"))
      (when text
        (buddhist-observation--insert-resource-button
         "Open chant text"
         text
         #'buddhist-observation-open-file))
      (when (and text audio)
        (insert "    "))
      (when audio
        (buddhist-observation--insert-resource-button
         "Play chant"
         audio
         #'buddhist-observation-play-audio))
      (when audio
        (insert "    ")
        (insert-text-button
         "Stop audio"
         'follow-link t
         'help-echo "Stop the current chant recording"
         'action
         (lambda (_button)
           (buddhist-observation-stop-audio))))
      (insert "\n"))))

(defun buddhist-observation--render (key date)
  "Render observation KEY for Gregorian DATE in the current buffer."
  (let* ((record (buddhist-observation-get key))
         (title (plist-get record :title))
         (calendar-name
          (plist-get record :calendar-name))
         (aspect (plist-get record :aspect))
         (description
          (plist-get record :description))
         (resources
          (plist-get record :resources)))
    (buddhist-observation--insert-heading title ?=)
    (insert (calendar-date-string date) "\n")
    (when calendar-name
      (insert "Calendar: " calendar-name "\n"))
    (when aspect
      (insert "Aspect: " aspect "\n"))
    (insert "\n")
    (when description
      (insert description "\n"))
    (dolist (resource resources)
      (buddhist-observation--insert-resource resource))))

(defvar-keymap buddhist-observation-mode-map
  :doc "Keymap for `buddhist-observation-mode'."
  "g" #'buddhist-observation-refresh
  "p" #'buddhist-observation-play-current-audio
  "s" #'buddhist-observation-stop-audio
  "o" #'buddhist-observation-open-current-text
  "q" #'quit-window)

(define-derived-mode buddhist-observation-mode special-mode
  "Buddhist Observation"
  "Major mode for displaying Buddhist observances."
  (setq-local truncate-lines nil)
  (button-mode 1))      ; Enable TAB/RET navigation of text buttons.

(defun buddhist-observation-display (key &optional year)
  "Display Buddhist observation KEY for YEAR.

When called interactively, prompt for an observation.  YEAR
defaults to the current Gregorian year."
  (interactive
   (list (buddhist-observation--read-key)
         (calendar-extract-year
          (calendar-current-date))))
  (let* ((year
          (or year
              (calendar-extract-year
               (calendar-current-date))))
         (date
          (buddhist-observation-date key year)))
    (unless date
      (user-error "Cannot calculate %s for %d"
                  key year))
    (with-current-buffer
        (get-buffer-create
         buddhist-observation--buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (buddhist-observation-mode)
        (setq buddhist-observation--current-key key
              buddhist-observation--current-date date)
        (buddhist-observation--render key date)
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun buddhist-observation-today ()
  "Display today's Buddhist observance.

Signal a user error if today is not one of the configured
observances."
  (interactive)
  (let ((key
         (buddhist-observation-today-key)))
    (unless key
      (user-error
       "Today is not a configured Buddhist observance"))
    (buddhist-observation-display
     key
     (calendar-extract-year
      (calendar-current-date)))))

(defun buddhist-observation-refresh ()
  "Refresh the current Buddhist observation buffer."
  (interactive)
  (unless (and buddhist-observation--current-key
               buddhist-observation--current-date)
    (user-error "No Buddhist observation is displayed"))
  (let ((key buddhist-observation--current-key)
        (date buddhist-observation--current-date)
        (inhibit-read-only t))
    (erase-buffer)
    (buddhist-observation--render key date)
    (goto-char (point-min))))

(defun buddhist-observation--current-resource ()
  "Return the first resource for the displayed observation."
  (when-let* ((record
               (buddhist-observation-get
                buddhist-observation--current-key))
              (key
               (car
                (plist-get record :resources))))
    (buddhist-observation-resource-get key)))

(defun buddhist-observation-play-current-audio ()
  "Play the first audio resource for the displayed observation."
  (interactive)
  (if-let* ((resource
             (buddhist-observation--current-resource))
            (audio
             (plist-get resource :audio)))
      (buddhist-observation-play-audio audio)
    (user-error
     "This observation has no associated audio")))

(defun buddhist-observation-open-current-text ()
  "Open the first text resource for the displayed observation."
  (interactive)
  (if-let* ((resource
             (buddhist-observation--current-resource))
            (text
             (plist-get resource :text)))
      (buddhist-observation-open-file text)
    (user-error
     "This observation has no associated text")))

(provide 'buddhist-observation)

;;; buddhist-observation.el ends here

; LocalWords:  buddhist afplay TBCMorningChanting Vesākha wav aiff
