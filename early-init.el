;;; early-init.el --- Early startup configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file runs before init.el.  Keep it small and limited to settings that
;; need to take effect before package loading, frame creation, or expensive
;; startup work.

;; Do not disable `site-start.el' under emacs-plus. emacs-plus uses it
;; to initialize build-specific variables such as
;; `ns-emacs-plus-version'.

;;; Code:
(require 'subr-x)
(require 'warnings)

(defvar native-comp-speed)
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-async-query-on-exit)
(defvar native-comp-deferred-compilation-deny-list)
(defvar comp-deferred-compilation-deny-list)

(declare-function startup-redirect-eln-cache "startup" (cache-directory))

;;;; Frame appearance

;; Use an undecorated maximized frame from the beginning.
(add-to-list 'default-frame-alist '(undecorated . t))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Prevent the glimpse of unstyled Emacs by disabling these UI elements early.
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font.  Inhibiting implied resize can noticeably reduce startup time.
(setq frame-inhibit-implied-resize t)

;; Avoid expensive font-cache compaction during startup.
(setq inhibit-compacting-font-caches t)

;;;; Native compilation

(setq native-comp-speed 2
      native-comp-async-report-warnings-errors nil
      native-comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

;; Keep native compilation output under ~/.emacs.d/var/.
(defconst cpj/eln-cache-directory
  (expand-file-name "var/cache/eln-cache/" user-emacs-directory)
  "Directory for native compilation cache files.")

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache cpj/eln-cache-directory))

;; Quitting Emacs while native compilation is in progress can leave zero-byte
;; *.eln or *.eln.tmp files behind.  Delete those during startup.
(when-let* ((find-exec (executable-find "find")))
  (when (file-directory-p cpj/eln-cache-directory)
    (call-process find-exec nil nil nil cpj/eln-cache-directory
                  "\\("
                  "-name" "*.eln"
                  "-o"
                  "-name" "*.eln.tmp"
                  "\\)"
                  "-size" "0"
                  "-delete")))

;;;; Warnings

;; Suppress warnings from older files without a lexical-binding cookie.
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))

;;;; Garbage collection

(defun cpj/restore-gc-cons-threshold ()
  "Restore garbage collection thresholds after startup."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))

;; Defer garbage collection during startup, then restore a reasonable threshold.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook #'cpj/restore-gc-cons-threshold 105)

;;; early-init.el ends here
; LocalWords:  dir eln tmp
