(use-package org-noter
  :ensure t
  :config
  (org-noter-enable-org-roam-integration)
  (setq org-noter-always-create-frame nil)
  )

(rune/leader-keys
  "on"  '(:ignore t :which-key "org-noter")
  "onn" '(org-noter :which-key "open-noter")
  "onk" '(org-noter-kill-session :which-key "kill-session"))



(provide 'init-org-noter)
;;; init-org-noter.el ends here
