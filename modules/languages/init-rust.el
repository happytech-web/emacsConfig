(use-package rust-mode)

(use-package rust-mode
  :straight t
  :hook
  (rust-mode . lsp-bridge-mode))

(provide 'init-rust)
;;; init-rust.el ends here

