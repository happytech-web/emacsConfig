(use-package go-translate
  :ensure t
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine))))

(rune/leader-keys
  "l" '(gt-do-translate :which-key "translation"))

(provide 'init-go-translation)
;;; init-go-translation.el ends here
