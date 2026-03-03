(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'org-download-enable)
  (setq org-download-method 'attach))  ; 使用 org-attach 机制

;;auto display the image when open a org file
(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))

(provide 'init-org-download)
;;; init-org-download.el ends here
