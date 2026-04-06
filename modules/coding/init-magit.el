(when (featurep 'transient)
  (unload-feature 'transient t))

(use-package transient
  :straight t
  :demand t)

(use-package magit
  :straight t
  :after transient
  :bind
  (:map magit-status-mode-map
        ("x" . magit-discard))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :straight t
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-side 'left)
  :config
  (diff-hl-flydiff-mode 1))

(provide 'init-magit)
;;; init-magit.el ends here
