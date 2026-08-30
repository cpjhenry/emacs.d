;;; scratchpads.el --- create scratchpads -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Keywords: convenience

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

;; Quickly create temp scratch buffer

;;; Code:

(require 'cl-lib)

(defun scratchpads--list-major-modes ()
  "List all the major modes.
Inspired from ieure/scratch-el.  Naïve probably."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-alist))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun scratchpads--select-mode ()
  "Select an appropriate major mode."
  (if current-prefix-arg
      (intern (concat (completing-read
                       "Major Mode: "
                       (scratchpads--list-major-modes)
                       nil t nil nil)))
    major-mode))

;;;###autoload
(defun scratchpads-new-scratchpad (mode)
  "Create a new *scratch* buffer for the MODE."
  (interactive (list (scratchpads--select-mode)))
  (let ((buf (generate-new-buffer "*scratch*")))
    (pop-to-buffer buf)
    (funcall mode)))

(provide 'scratchpads)
;;; scratchpads.el ends here
