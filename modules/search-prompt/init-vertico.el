(use-package vertico
  :ensure t
  :general
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))


(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (;; Cleans up path when moving directories with shadowed paths syntax
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  )

(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (:map vertico-map
              ("C-r" . vertico-repeat-select)))


(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;; copied from rofie
(use-package orderless
  :straight t
  :init (require 'orderless)
  :config
  ;; Dispatchers
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern) `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "^" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "^" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
     ;; Annotations
     ((string-prefix-p "@" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "@" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ))

  ;; Remote file completion
  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))

  (defun orderless+basic-all (str table pred point)
    (or (orderless-all-completions str table pred point)
        (completion-basic-all-completions str table pred point)))

  (defun orderless+basic-try (str table pred point)
    (or (completion-basic-try-completion str table pred point)
        (orderless-try-completion str table pred point)))

  (add-to-list 'completion-styles-alist
               '(orderless+basic
                 orderless+basic-try
                 orderless+basic-all
                 "Unholy mix of Orderless and Basic."))

  ;; configuration
  (setq completion-styles '(orderless+basic)
        completion-category-defaults nil
        completion-ignore-case t
        ;; despite override in the name, orderless can still be used in find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless+basic))
                                        (eglot (styles orderless)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  )

(use-package consult
  :bind (("C-s" . consult-line))
  :general
  (rune/leader-keys
    "bj" '(consult-buffer :which-key "consult buffer")
    "fj" '(consult-recent-file :which-key "recent file")
    "fl" '(consult-locate :which-key "locat file")
    "ss" '(consult-line :which-key "search")
    "sS" '(consult-line-multi :which-key "search multi buf")
    "sr" '(consult-ripgrep :which-key "ripgrep")
    "sg" '(consult-git-grep :which-key "gitgrep")
    "sy" '(consult-yank-pop :which-key "yank history")
    "sf" '(consult-focus-lines :which-key "focus linex")
    "sF" '(consult-keep-lines :which-key "keep lines")
    "si" '(consult-imenu-multi :which-key "imenu")
    "pb" '(consult-project-buffer :which-key "consult buf")
    )
  :straight t)


(use-package savehist
  :init
  (savehist-mode))


(provide 'init-vertico)
