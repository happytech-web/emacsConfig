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

  (defun my/eglot-apply-highlight-face ()
    "Apply a high-contrast face for eglot symbol highlight."
    (when (facep 'eglot-highlight-symbol-face)
      (set-face-attribute 'eglot-highlight-symbol-face nil
                          :inherit nil
                          :background "#5b4a2b"
                          :weight 'black
                          :underline '(:color "#50fa7b" :style line)
                          :extend t)))

  ;; Re-apply after theme changes to avoid face overrides.
  (my/eglot-apply-highlight-face)
  (advice-add 'load-theme :after (lambda (&rest _) (my/eglot-apply-highlight-face)))

  ;; Prefer tree-sitter imenu in ts-modes.
  (add-to-list 'eglot-stay-out-of 'imenu)

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
  (keymap-global-set "C-c l g" #'xref-find-definitions)
  (keymap-global-set "C-c l G" #'xref-find-references)
  (keymap-global-set "C-c l d" #'eldoc)
  (keymap-global-set "C-c l e" #'flymake-show-buffer-diagnostics)
  (keymap-global-set "C-c l r" #'eglot-rename)
  (keymap-global-set "C-c l f" #'eglot-format)
  (keymap-global-set "C-c l a" #'eglot-code-actions))

(use-package imenu-list
  :straight t
  :after eglot
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-size 40)
  :bind (("C-c l t" . imenu-list-smart-toggle)))

(use-package eldoc-mouse
  :straight t
  :bind (("C-k" . eldoc-mouse-pop-doc-at-cursor)))

(provide 'init-eglot)
;;; init-eglot.el ends here
