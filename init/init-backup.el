;;; init-backup.el --- Setup for backup and autosave files  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yves Blusseau

;; Author: Yves Blusseau <90z7oey02@sneakemail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;;----------------------------------------------------------------------------
;; Backup
;;----------------------------------------------------------------------------
(defvar backup-directory (expand-file-name "backups" user-emacs-directory))

(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

(setq backup-directory-alist
      `(("." . ,backup-directory)))

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      vc-make-backup-files t            ; backup even files managed by a version control system
      )

;;----------------------------------------------------------------------------
;; Auto save
;;----------------------------------------------------------------------------
;; Becareful trailing / is needed for auto-save-directory
(defvar auto-save-directory (expand-file-name "auto-save/" user-emacs-directory))
(if (not (file-exists-p auto-save-directory))
    (make-directory auto-save-directory t))

(setq auto-save-file-name-transforms
      `((".*" ,auto-save-directory t)))

(setq auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Auto save remote files locally
(setq tramp-auto-save-directory
      (expand-file-name "auto-save" user-emacs-directory))


(provide 'init-backup)
;;; init-backup.el ends here
