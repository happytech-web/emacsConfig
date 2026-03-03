;;; basic key-bindings here
(rune/leader-keys
  "ff" '(find-file :which-key "find-file")
  "bb" '(switch-to-buffer :which-key "switch buffer")
  )
;;; [recentf] recently visited files
(use-package recentf
  :after general
  :general
  (rune/leader-keys
    "fr" '(recentf-open :which-key "recent file")
    )
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
    recentf-max-saved-items 200
    recentf-exclude (list "\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
		  "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
		  "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
		  (lambda (file) (file-in-directory-p file package-user-dir))
		  (expand-file-name recentf-save-file))
    recentf-keep nil)

  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)

  ;; HACK: Text properties inflate the size of recentf's files, and there is
  ;; no purpose in persisting them (Must be first in the list!)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  ;; Add dired directories to recentf file list.
  (add-hook! dired-mode-hook
    (defun +dired--add-to-recentf-h ()
      (recentf-add-file default-directory)))
  )



;; Disable [bidirectional text] scanning for a modest performance
;; Will improve long line display performance
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-display-reordering 'left-to-right)

;; 默认：所有模式都用空格缩进，宽度 2
(setq-default indent-tabs-mode nil)
(setq-default tab-width        2)
(setq-default standard-indent  2)

;; 对于大多数编程模式，保持上面默认的 2 空格
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width        2)
            (setq-local standard-indent  2)))

;; Java 用 4 空格
(add-hook 'java-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width        4)
            (setq-local c-basic-offset   4)))  ;; c-family 缩进

;; Python 用 4 空格
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode    nil)
            (setq-local tab-width           4)
            (setq-local python-indent-offset 4)))

;; Makefile 仍然用真实的 Tab（并且保持 Tab 宽度按 8 显示或你喜欢的值）
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)
            (setq-local tab-width      8)))  ;; Makefile 里 Tab 一般是 8 列


;; [so-long] Workaround for long one-line file
(use-package so-long
  :hook ((after-init . global-so-long-mode)
	 ((so-long-mode prog-mode fundamental-mode) . +so-long-settings))
  :config
  ;; improve long line performance
  (defun +so-long-settings ()
    (setq bidi-display-reordering nil))

  ;; Saveplace should not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  )

;; [gcmh] Optimize GC
(use-package gcmh
  :straight t
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto
    gcmh-auto-idle-delay-factor 10
    gcmh-high-cons-threshold #x64000000)
  )


;; [super-save] auto save my buffer
(use-package super-save
  :after evil
  :straight t
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-exclude '("Deepseek"))
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :preface
  ;; HACK: i want to execute the command after an edit
  ;; default super-save-triggers will save buffer before an cmd
  ;; helper that runs the normal supersave logic
  (defun ll/super-save-after (&rest _)
    "Save current buffer (like `super-save-command') *after* an edit."
    (super-save-command))
  :config
  (setq super-save-remote-files nil)
  (dolist (hook '(evil-normal-state-entry-hook
		  evil-normal-state-exit-hook))
    (add-to-list 'super-save-hook-triggers hook))

  (dolist (cmd '(ace-window))
    (add-to-list 'super-save-triggers cmd))

  (dolist (cmd '(indent-for-tab-command ; <tab> to indent
		 evil-join              ; J
		 evil-delete            ; dw, d$, visual D…
                 evil-delete-line       ; dd
                 evil-delete-char       ; x
                 evil-change            ; c{motion}
                 evil-change-line       ; cc
                 evil-paste-after       ; p
                 evil-paste-before      ; P
		 evil-undo))            ; u
    (advice-add cmd :after #'ll/super-save-after))

  (super-save-mode +1)
  )


(provide 'init-basic)
