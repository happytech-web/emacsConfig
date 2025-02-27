;;   (use-package company
;;     :ensure t
;;     :after lsp-mode
;;     :hook (lsp-mode . company-mode)
;;     :bind (:map company-active-map
;;                   ("<tab>" . company-complete-selection))
;;       (:map lsp-mode-map
;;             ("<tab>" . company-indent-or-complete-common))
;;       :custom
;;       (company-minimum-prefix-length 1)
;;       (company-idle-delay 0.0)
;;       (company-show-numbers t) ; M-1, M-2 ... to select the card
;;       (company-selection-wrap-around t)
;;       (company-transformers '(company-sort-by-occurrence))) ; frequency sort

 (use-package company
   :diminish company-mode
   :general
   (general-define-key :keymaps 'company-active-map
                       "C-j" 'company-select-next
                       "C-k" 'company-select-previous)
   :bind (:map company-active-map
               ("<tab>" . company-complete-selection))
   (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
   :init
   ;; These configurations come from Doom Emacs:
   (add-hook 'after-init-hook 'global-company-mode)
   (setq company-minimum-prefix-length 1
         company-tooltip-limit 14
         company-tooltip-align-annotations t
         company-require-match 'never
         company-global-modes '(not erc-mode message-mode help-mode gud-mode)
         company-frontends
         '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
           company-echo-metadata-frontend)  ; show selected candidate docs in echo area
         company-backends '(company-capf company-files company-keywords)
         company-auto-complete nil
         company-auto-complete-chars nil
         company-dabbrev-other-buffers nil
         company-dabbrev-ignore-case nil
         company-dabbrev-downcase nil)

   :custom
   (company-show-numbers t)
   (company-selection-wrap-around t)
   (company-transformers '(company-sort-by-occurrence))
   (company-idle-delay 0.0))

(use-package company-box
    :ensure t
    :if window-system
    :hook (company-mode . company-box-mode))

(provide 'init-company)
;;; init-company.el ends here
