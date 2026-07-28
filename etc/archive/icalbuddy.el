(defun daily-info--shell-string (command)
  "Return trimmed output of shell COMMAND, or nil if empty."
  (let ((s (string-trim
            (shell-command-to-string command))))
    (unless (string-empty-p s)
      s)))

(defconst daily-info-birthday-command
  (concat
   "ssh bullwinkle "
   "\"/usr/local/bin/icalBuddy -nc -df '%RD' "
   "-ic Birthdays eventsFrom:yesterday to:today+21\" "
   "2>/dev/null | sed -e "
   "\"s/'s Birthday//;"
   "s/ (age.*)//;"
   "s/, today//;"
   "s/ from now//\""))

(defun daily-info--birthday-summary ()
  "Return upcoming birthday summary from remote icalBuddy."
  (daily-info--shell-string daily-info-birthday-command))


    (when-let* ((birthdays (daily-info--birthday-summary)))
      (insert "\n")
      (insert (string-trim-right birthdays))
      (insert "\n"))
