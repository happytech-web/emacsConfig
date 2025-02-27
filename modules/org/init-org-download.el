(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "~/Pictures/org-download"))

;;auto display the image when open a org file
(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))

(provide 'init-org-download)
;;; init-org-download.el ends here
