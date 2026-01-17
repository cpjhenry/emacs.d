;;; fill-para.el --- more flexible fill-paragraph    -*- lexical-binding: t; -*-

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

;;; commentary:

;; https://mbork.pl/2026-01-12_Making_fill-paragraph_more_flexible
;; qv. https://sachachua.com/blog/2025/09/emacs-cycle-through-different-paragraph-formats-all-on-one-line-wrapped-max-one-sentence-per-line-one-sentence-per-line/
;; for further deets.

;;; code:
(require 'cl-lib)

(defvar fill-paragraph-state nil
  "The way the paragraph was filled the last time.")

;; From https://github.com/purcell/unfill/blob/master/unfill.el#L39
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

(defun fill-paragraph-rotate ()
  "Fill the current paragraph in one of three ways.
First, it fills the paragraph semantically, then, unfills it, and
finally, fills it in the traditional way."

  (interactive)
  (unless (eq last-command this-command)
    (setq fill-paragraph-state nil))
  (let (deactivate-mark)
    (cl-case fill-paragraph-state
      ('fill-paragraph-semlf
       (call-interactively 'unfill-paragraph)
       (setq fill-paragraph-state 'unfill-paragraph))
      ('unfill-paragraph
       (call-interactively 'fill-paragraph)
       (setq fill-paragraph-state 'fill-paragraph))
      (t
       (call-interactively 'fill-paragraph-semlf)
       (setq fill-paragraph-state 'fill-paragraph-semlf)))))

(provide 'fill-para)
;;; fill-para.el ends here
