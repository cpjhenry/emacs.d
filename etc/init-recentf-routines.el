;; recentf routines
(require 'recentf)

(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
      (let ((display-count (buffer-local-value 'buffer-display-count buf)))
        (if (> display-count 0) display-count nil)))))

(defsubst keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed."
  (if (recentf-keep-default-predicate file)
      (file-was-visible-p file)))

(let ((r-list recentf-list))
  (defsubst keep-default-old-and-visible-recentf-p (file)
    "Decide whether to keep file in recentf-list.
Return non-nil if recentf would, by default, keep FILE, and
either FILE name was loaded from recentf file on disk or FILE
has been displayed in this session."
    (if (recentf-keep-default-predicate file)
      (or (member file r-list)
      (file-was-visible-p file)))))

;; When a buffer is closed, remove the associated file from the recentf
;; list if (1) recentf would have, by default, removed the file, or
;; (2) the buffer was never displayed.  This is useful because, for
;; example, CEDET opens a lot of files in the background to generate
;; its tags database, etc.
(setq recentf-keep '(keep-default-old-and-visible-recentf-p))
