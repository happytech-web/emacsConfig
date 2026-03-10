;;; early-init.el --- Early init for straight.el -*- lexical-binding: t; -*-

;; Prevent package.el from auto-initializing before init.el.
(setq package-enable-at-startup nil)

;; Startup GC optimization.
(setq gc-cons-threshold most-positive-fixnum)

;; Keep startup UI minimal before theme loads.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Reduce frame resize flicker during startup.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
