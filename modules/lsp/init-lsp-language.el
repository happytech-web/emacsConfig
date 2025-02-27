(use-package cc-mode
  :ensure nil
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'c-mode-map
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;;c mode custom
  "c" '(nil :which-key "compile")
  "cs" '(compile-c-file :which-key "simple compile")
  "cr" '(compile-and-run-c-file :which-key "compile and run"))

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'c++-mode-map
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;; cpp mode custom
  "c" '(nil :which-key "compile")
  "cs" '(compile-cpp-file :which-key "simple compile")
  "cr" '(compile-and-run-cpp-file :which-key "compile and run"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                  (require 'lsp-pyright)
                  (lsp-deferred))))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))


(use-package nix-mode
  :ensure t
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")

(provide 'init-lsp-language)
;;; init-lsp-language.el ends here

