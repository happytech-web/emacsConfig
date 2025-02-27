;; giving a hierarchy in a file
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; lsp mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-, l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t)
  :custom
  (lsp-ui-doc-position 'bottom))


(use-package lsp-ivy)

(provide 'init-lspmode)
;;; init-lspmode.el ends here
