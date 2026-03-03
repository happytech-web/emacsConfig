(use-package nerd-icons)
(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state subtree-state nerd-icons collapse file-size))

  (setq dirvish-use-header-line 'global)

  (setq dirvish-header-line-format
        '(:left (path) :right (free-space))
        dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

   (setq dirvish-large-directory-threshold 2000)
   (evil-define-key 'normal dirvish-mode-map
     "h" 'dired-up-directory
     "l" 'dired-find-file
     "f" 'dirvish-file-info-menu
     "g" 'dirvish-quick-access
     "t" 'dirvish-layout-toggle
     "M" 'dirvish-mark-menu
     "p" 'dirvish-yank
     "P" 'dirvish-yank-menu
     "<tab>" 'dirvish-subtree-toggle)
)
(with-eval-after-load 'dirvish
  (keymap-set dirvish-mode-map "<tab>" #'dirvish-subtree-toggle))


(rune/leader-keys
  "ft" '(dirvish-side :which-key "dirvish-side"))

(provide 'init-dirvish)
;;; init-vterm.el ends here
