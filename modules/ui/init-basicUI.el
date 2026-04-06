;; font config ---------------------------------------------------
(defvar efs/default-font-size 130)
(defvar efs/default-variable-font-size 150)

(defun ll/setup-fonts (&optional frame)
  "Setup fonts. Works for daemon and new frames."
  (with-selected-frame (or frame (selected-frame))
    ;; English / default
    (set-face-attribute 'default nil
                        :font "Fira Code"
                        :height efs/default-font-size)

    ;; Fixed pitch (code, tables, etc.)
    (set-face-attribute 'fixed-pitch nil
                        :font "Fira Code"
                        :height efs/default-font-size)

    ;; Variable pitch (org prose)
    (set-face-attribute 'variable-pitch nil
                        :font "LXGW WenKai"
                        :height efs/default-variable-font-size
                        :weight 'regular)

    ;; CJK fallback
    (set-fontset-font t 'han (font-spec :family "LXGW WenKai"))

    ;; (optional) emoji fallback
    ;; (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
    ))

(add-hook 'after-init-hook #'ll/setup-fonts)
(add-hook 'after-make-frame-functions #'ll/setup-fonts)
;; font config end ------------------------------------------------


(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltips
(set-fringe-mode 10) ;give some breathing room

(menu-bar-mode -1)   ;disable the munu bar

;; set up the visible bell
(setq visible-bell t)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable blinking
(blink-cursor-mode -1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
              term-mode-hook
              shell-mode-hook
              vterm-mode-hook
              treemacs-mode-hook
              eshell-mode-hook
              pdf-view-mode-hook
              reader-mode-hook
              ))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; parenthesis completion
(electric-pair-mode 1)

(setq all-the-icons-dired-monochrome nil)  ;; 关闭单色图标模式,在文件系统中看起来更好

;; safe theme when no other themes
(load-theme 'wombat)

(provide 'init-basicUI)
;;; init-basicUI.el ends here
