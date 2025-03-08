(use-package yasnippet
  :ensure nil
  :config
  (yas-global-mode 1))

(require 'lsp-bridge)

(use-package lsp-bridge
  :ensure nil
  :init
  (global-lsp-bridge-mode)
  :general
  (general-define-key
   :keymaps 'acm-mode-map
   :states '(insert normal emacs)
   "C-j" 'acm-select-next
   "C-k" 'acm-select-prev)

  (general-define-key
   :keymaps 'lsp-bridge-mode-map
   :prefix ","
   :states '(normal motion)
   "g"  '(:ignore t :which-key "jump")
   "gg" '(lsp-bridge-find-def :which-key "find-def")
   "gG" '(lsp-bridge-find-def-other-window :which-key "find-def-other-window")
   "gb" '(lsp-bridge-find-def-return :which-key "find-def-return")
   "gt" '(lsp-bridge-find-type-def :which-key "find-type")
   "gT" '(lsp-bridge-find-type-def-other-window :which-key "find-type-other-window")
   "gi" '(lsp-bridge-find-impl :which-key "find-implementation")
   "gI" '(lsp-bridge-find-impl-other-window :which-key "find-impl-other-window")
   "gr" '(lsp-bridge-find-references :which-key "find-ref")

   "d"  '(:ignore t :which-key "doc")
   "dd" '(lsp-bridge-popup-documentation :which-key "pop-doc")
   "db" '(lsp-bridge-show-documentation :which-key "show-doc-buf")

   "r"  '(:ignore t :which-key "refactor")
   "rr" '(lsp-bridge-rename :which-key "rename")
   "ra" '(lsp-bridge-code-action :which-key "action")

   "b"  '(:ignore t :which-key "bugs")
   "bb" '(lsp-bridge-diagnostic-list :which-key "list-bugs")
   "bj" '(lsp-bridge-diagnostic-jump-next :which-key "next-bugs")
   "bk" '(lsp-bridge-diagnostic-jump-prev :which-key "prev-bugs")
   "by" '(lsp-bridge-diagnostic-copy :which-key "copy-bugs")

   "s"  '(:ignore t :which-key "symbols")
   "ss" '(lsp-bridge-workspace-list-symbol-at-point :which-key "list-symbol-at-point")
   "sa" '(lsp-bridge-workspace-list-symbols :which-key "list-all-symbols")
   )

  :custom
  (lsp-bridge-c-lsp-server 'clangd)
  (lsp-bridge-python-lsp-server 'pyright)
  (lsp-bridge-python-multi-lsp-server 'pyright_ruff)
  (lsp-bridge-nix-lsp-server 'nixd)
  (lsp-bridge-tex-lsp-server 'digestif)
  (lsp-bridge-tsdk-path "/home/happytech/.emacs.d/language-servers/ts/lib/node_modules/typescript/lib")
  :config
  (add-hook 'find-file-hook #'lsp-bridge-restart-process)
  )

(use-package vue-mode
  :hook
  (vue-mode . (lambda ()
                (lsp-bridge-mode 1)
                (run-with-idle-timer 0.1 nil #'lsp-bridge-mode 1))))
;; (use-package vue-mode
;;   :hook
;;   (vue-mode . lsp-bridge-mode))


(provide 'init-lspbridge)
;;; init-lspbridge.el ends here
