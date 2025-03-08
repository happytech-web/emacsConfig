;; (use-package aidermacs
;;   :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
;;   :config
;;   (setq aidermacs-default-model "deepseek")
;;   (setq aidermacs-use-architect-mode t)
;;   (setq aidermacs-architect-model "o1-mini") ; default
;;   (setq aidermacs-editor-model "anthropic/claude-3-5-sonnet-20241022") ; default
;;   (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
;;   ; Ensure emacs can access *_API_KEY through .bashrc or setenv
;;   (setenv "ANTHROPIC_API_KEY" "sk-b58ca4dec82b449d9ace8f2ab89260de")
;;   ; See the Configuration section below
;;   (setq aidermacs-auto-commits t)
;;   (setq aidermacs-use-architect-mode t))
