(use-package vundo
  :straight t
  :general
  (rune/leader-keys
    "u" '(vundo :which-key "vundo"))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(provide 'init-vundo)
