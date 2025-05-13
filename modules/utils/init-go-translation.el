(use-package go-translate
  :ensure t
  :general
  (rune/leader-keys
    "l" '(:ignore t :which-key "language")
    "ll" '(gt-do-translate :which-key "go translation"))
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine))))

(use-package fanyi
  :straight t
  :general
  (rune/leader-keys
    "la" '(fanyi-dwim2 :which-key "fanyi-dwim2")))


(provide 'init-go-translation)
;;; init-go-translation.el ends here
