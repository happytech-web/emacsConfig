;; TODO: need rewrite
;; TODO: need rewrite
;; TODO: need rewrite

;; (use-package dap-mode
;;   ;; Uncomment the config below if you want all UI panes to be hidden by default!
;;                                         ;:custom
;;   :custom
;;   (dap-auto-configure-features '(locals expressions controls))
;;   (lsp-enable-dap-auto-configure t)
;;   :config
;;   (dap-ui-mode 1)
;;   (setq dap-auto-show-output 1)

;;   ;; Set up Node debugging
;;   (require 'dap-node)
;;   (dap-node-setup) ;; Automatically installs Node debug adapter if needed
;;   ;; Set up GDB
;;   (require 'dap-gdb-lldb)
;;   (dap-gdb-lldb-setup)

;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   ;; Bind `, l d` to `dap-hydra` for easy access
;;   (general-def
;;     :prefix ","
;;     :states 'motion
;;     :keymaps 'lsp-mode-map
;;     ;; dap
;;     "d" '(nil :which-key "dap")
;;     "dr" '(dap-debug :which-key "debug")
;;     "di" '(dap-breakpoint-add :which-key "add breakpoint")
;;     "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
;;     "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
;;     "dc" '(dap-breakpoint-condition :which-key "condition")
;;     "dh" '(dap-hydra t :wk "helpper map"))
;;   ;(general-define-key
;;   ; :keymaps 'lsp-mode-map
;;   ; :prefix lsp-keymap-prefix
;;   ; "d" '(dap-hydra t :wk "debugger"))
;;   )

(use-package dap-java
  :ensure nil)

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'java-mode-map
  ;; lsp
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;; test
  "t" '(nil :which-key "test")
  "tc" '(dap-java-run-test-class :which-key "run class")
  "tm" '(dap-java-run-test-method :which-key "run method")
  "tl" '(dap-java-run-last-test :which-key "run last")
  "tM" '(dap-java-debug-test-method :which-key "debug method")
  "tC" '(dap-java-debug-test-class :which-key "debug class"))

(provide 'init-dapmode)
;;; init-dapmode.el ends here
