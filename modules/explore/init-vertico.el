;;; init-vertico.el --- Vertico completion stack -*- lexical-binding: t; -*-

(use-package vertico
  :straight t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("C-r" . vertico-repeat-select))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-posframe
  :straight t
  :after vertico
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 8)
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))

(use-package vertico-multiform
  :straight nil
  :after (vertico vertico-posframe)
  :config
  (setq vertico-multiform-commands
        '(
          ;; xref 用底部 minibuffer 而非 posframe
          (xref-find-definitions (:not posframe))
          (xref-find-references  (:not posframe))
          (consult-xref           (:not posframe))
          ;; 其他命令用 posframe
          (t posframe)))
  ;; 图形界面才启用 multiform（posframe 依赖图形）
  (when (display-graphic-p)
    (vertico-multiform-mode 1)))

(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode 1))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion orderless)))))

(use-package consult
  :straight t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-c i" . consult-imenu)
 	 ("C-c s r" . consult-ripgrep))
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-eglot
  :straight t
  :after (consult eglot)
  :bind (("C-c l S" . consult-eglot-symbols)))

(use-package savehist
  :straight nil
  :init
  (savehist-mode 1))

(provide 'init-vertico)
;;; init-vertico.el ends here
