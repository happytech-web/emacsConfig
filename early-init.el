;;; early-init.el --- Early startup optimizations -*- lexical-binding: t; -*-

;; Do not auto-load package.el. We use straight.el in init.el.
(setq package-enable-at-startup nil)

;; Improve startup performance during init.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      inhibit-compacting-font-caches t
      read-process-output-max (* 1024 1024))

;; Temporarily disable file-name handlers during startup.
(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Keep initial UI minimal to reduce startup work.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here
