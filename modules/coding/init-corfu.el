;;; init-corfu.el --- In-buffer completion via corfu -*- lexical-binding: t; -*-

(use-package corfu
  :straight t
  :functions (corfu-move-to-minibuffer consult-completion-in-region)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-count 12)
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("M-m" . corfu-move-to-minibuffer)
              ("RET" . corfu-insert))
  :init
  (global-corfu-mode 1)
  :config
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (add-hook 'before-save-hook #'corfu-quit)
  (defun corfu-move-to-minibuffer ()
    "Move current in-buffer completion session to minibuffer."
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package cape
  :straight t
  :functions (cape-wrap-buster cape-wrap-nonexclusive)
  :init
  ;; Add common completion backends to completion-at-point.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :config
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)))

(provide 'init-corfu)
;;; init-corfu.el ends here
