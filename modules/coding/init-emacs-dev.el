;;; init-emacs-dev.el --- Emacs Lisp dev helpers -*- lexical-binding: t; -*-

(use-package elisp-demos
  :straight t
  :init
  ;; Show demos in built-in *Help* buffers.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  :config
  ;; If helpful is installed, append demos there as well.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

(provide 'init-emacs-dev)
;;; init-emacs-dev.el ends here
