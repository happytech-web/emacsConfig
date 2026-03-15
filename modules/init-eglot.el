;;; init-eglot.el --- Eglot LSP setup -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure))
  :config
  (setq eglot-autoshutdown t
        eglot-sync-connect 1
        eglot-extend-to-xref t)

  (defun my/python-eglot-contact (_interactive)
    "Return the Python LSP server command."
    (cond
     ((executable-find "pyright-langserver")
      '("pyright-langserver" "--stdio"))
     ((executable-find "ruff")
      '("ruff" "server"))
     (t (user-error "No Python LSP found (need pyright-langserver or ruff)"))))

  ;; Python: prefer pyright, fallback to ruff server.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . my/python-eglot-contact))

  ;; C/C++: clangd.
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd" "--background-index" "--clang-tidy")))

  ;; Minimal global LSP keybindings.
  (keymap-global-set "C-c l r" #'eglot-rename)
  (keymap-global-set "C-c l f" #'eglot-format)
  (keymap-global-set "C-c l a" #'eglot-code-actions))

(provide 'init-eglot)
;;; init-eglot.el ends here
