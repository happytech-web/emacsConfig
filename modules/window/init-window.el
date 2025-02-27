(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . 'ace-window)))

;   (use-package edwina
;     :ensure t
;     :config
;     (setq display-buffer-base-action '(display-buffer-below-selected))
     ;; (edwina-setup-dwm-keys)
;     (edwina-mode 1))

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
         ("u" . winner-undo)
         ("U" . winner-redo))
  :config
  (winner-mode))

;(use-package golden-ratio
;  :ensure t
;  :config
;  (golden-ratio-mode 1))

(provide 'init-window)
;;; init-window.el ends here
