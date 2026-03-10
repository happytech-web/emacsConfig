(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  )


(use-package tree-sitter
  :straight t
  :straight t
  :init
  (add-hook!
    (c-mode-hook
     c++-mode-hook
     python-mode-hook
     javascript-mode-hook
     typescript-mode-hook
     rust-mode-hook
     java-mode-hook
     )
    #'tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(use-package combobulate
  :straight t
  :after tree-sitter
  :hook (tree-sitter-after-on-hook . combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o"))

;; (use-package treesit
;;   :when (treesit-available-p)
;;   :init
;;   (setq major-mode-remap-alist
;;         '((c-mode . c-ts-mode)
;;           (c++-mode . c++-ts-mode)
;;           (python-mode . python-ts-mode)
;;           (javascript-mode . javascript-ts-mode)
;;           (typescript-mode . typescript-ts-mode)
;; 	  (rust-mode . rust-ts-mode)
;;           ))

;;   (setq treesit-language-source-alist
;;         '((c "https://github.com/tree-sitter/tree-sitter-c")
;;           (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;           (python "https://github.com/tree-sitter/tree-sitter-python")
;;           (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;           (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;           (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;           ))

;;   (dolist (lang treesit-language-source-alist)
;;   (unless (treesit-language-available-p (car lang))
;;     (treesit-install-language-grammar (car lang))))

;;   (setq treesit-font-lock-level 4)
;;   )

(provide 'init-treesitter)
