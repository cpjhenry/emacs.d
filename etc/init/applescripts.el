(defun sw-pp () "Make iTunes either pause or play"
	(interactive)
	(setq apscript "
		tell application \"Music\"
		if player state is paused then play
		else pause
		end if
		end tell
		")
	(do-applescript apscript))
