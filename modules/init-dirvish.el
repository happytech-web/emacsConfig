;;; init-dirvish.el --- Dirvish setup -*- lexical-binding: t; -*-

(use-package nerd-icons
  :straight t)

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state subtree-state nerd-icons collapse file-size)
        dirvish-use-header-line 'global
        dirvish-header-line-format '(:left (path) :right (free-space))
        dirvish-mode-line-format '(:left (sort file-time " " file-size symlink) :right (omit yank index))
        dirvish-large-directory-threshold 2000)

  ;; Preserve original dirvish keybindings.
  (keymap-set dirvish-mode-map "h" #'dired-up-directory)
  (keymap-set dirvish-mode-map "l" #'dired-find-file)
  (keymap-set dirvish-mode-map "f" #'dirvish-file-info-menu)
  (keymap-set dirvish-mode-map "g" #'dirvish-quick-access)
  (keymap-set dirvish-mode-map "t" #'dirvish-layout-toggle)
  (keymap-set dirvish-mode-map "M" #'dirvish-mark-menu)
  (keymap-set dirvish-mode-map "p" #'dirvish-yank)
  (keymap-set dirvish-mode-map "P" #'dirvish-yank-menu)
  (keymap-set dirvish-mode-map "<tab>" #'dirvish-subtree-toggle)

  ;; Global entry point (same intent as original `ft` side panel).
  (keymap-global-set "C-c f t" #'dirvish-side))

(provide 'init-dirvish)
;;; init-dirvish.el ends here
