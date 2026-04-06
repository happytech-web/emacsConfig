;;; init-treesitter.el --- Native tree-sitter setup -*- lexical-binding: t; -*-

(when (fboundp 'treesit-available-p)
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typst "https://github.com/uben0/tree-sitter-typst")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" nil "tree-sitter-markdown-inline/src")))

  (setq major-mode-remap-alist
        (append major-mode-remap-alist
                '((python-mode . python-ts-mode)
                  (c-mode . c-ts-mode)
                  (c++-mode . c++-ts-mode)
                  (c-or-c++-mode . c-or-c++-ts-mode)
                  (rust-mode . rust-ts-mode)
                  (sh-mode . bash-ts-mode)
                  (yaml-mode . yaml-ts-mode))))

  (when (fboundp 'nix-ts-mode)
    (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode)))

  (when (fboundp 'markdown-ts-mode)
    (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode)))

  (defvar my/treesit-languages
    '(bash c cpp nix python rust typst yaml markdown markdown-inline)
    "Tree-sitter languages that should be installed.")

  (defvar my/treesit-auto-install t
    "Whether to auto-install missing tree-sitter grammars at startup.")

  (defun my/install-treesit-grammars ()
    "Install tree-sitter grammars for configured languages."
    (interactive)
    (dolist (lang my/treesit-languages)
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))

  (defun my/treesit-install-missing-grammars ()
    "Auto-install missing tree-sitter grammars."
    (when my/treesit-auto-install
      (dolist (lang my/treesit-languages)
        (unless (treesit-language-available-p lang)
          (condition-case err
              (progn
                (message "Installing tree-sitter grammar: %s" lang)
                (treesit-install-language-grammar lang))
            (error
             (message "Failed to install grammar %s: %s" lang err)))))))

  ;; Auto-install only in interactive sessions.
  (unless noninteractive
    (add-hook 'emacs-startup-hook #'my/treesit-install-missing-grammars)))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
