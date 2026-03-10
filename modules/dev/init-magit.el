(when (featurep 'transient)
  (unload-feature 'transient t))

(use-package transient
  :straight t
  :demand t)

(use-package magit
  :straight t
  :after transient
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
;;; init-magit.el ends here
