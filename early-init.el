;;; early-init.el --- cpj -*- flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc); -*-
;;; commentary:

;;; code:
(add-to-list 'default-frame-alist '(undecorated . t))

;; Don't show Emacs frame until initialized
;(add-to-list 'initial-frame-alist '(visibility . nil))
;(add-hook 'after-init-hook (lambda () (make-frame-visible)))

;; https://github.com/jimeh/.emacs.d/blob/master/early-init.el
;; Native-Comp
(setq native-comp-speed 2
      native-comp-async-report-warnings-errors nil
      native-comp-async-query-on-exit t)

;; Prevent native-compiling .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (if (boundp 'native-comp-deferred-compilation-deny-list)
      (setq native-comp-deferred-compilation-deny-list deny-list)
    (setq comp-deferred-compilation-deny-list deny-list)))

(when (or (boundp 'comp-eln-load-path) (boundp 'native-comp-eln-load-path))
  (let ((eln-cache-dir (expand-file-name "cache/eln-cache/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))

    (if (boundp 'native-comp-eln-load-path)
        (setcar native-comp-eln-load-path eln-cache-dir)
      (setcar comp-eln-load-path eln-cache-dir))
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
        "-name" "*.eln" "-size" "0" "-delete" "-or"
        "-name" "*.eln.tmp" "-size" "0" "-delete"))))

;; Disable Emacs 27's automatic package.el initialization before the init.el
;; file is loaded. I use straight.el instead of package.el.
;(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; font compacting
(setq inhibit-compacting-font-caches t)

;; caching nonsense
(startup-redirect-eln-cache (concat user-emacs-directory "var/cache"))

;; suppress lexical cookie
(setq warning-suppress-log-types '((missing-lexbind-cookie))
      warning-suppress-types '((missing-lexbind-cookie)))

;; Defer garbage collection further back in the startup process
;(setq gc-cons-threshold most-positive-fixnum)

;; in early-init.el
;; https://old.reddit.com/r/emacs/comments/1jtja9s/emacs_startup_time_doesnt_matter/
(defun restore-gc-cons-threshold ()
  (setq gc-cons-threshold (* 16 1024 1024)
    gc-cons-percentage 0.1))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook #'restore-gc-cons-threshold 105)

(provide 'early-init)
;;; early-init.el ends here

; LocalWords:  checkdoc flycheck
