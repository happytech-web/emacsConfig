;; (use-package dired-single)
;; (require 'dired-single)

(use-package dired
  :straight nil
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (keymap-set dired-mode-map "h" (lambda () (interactive) (find-alternate-file "..")))
  (keymap-set dired-mode-map "l" #'dired-find-alternate-file)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" (lambda()(interactive)(find-alternate-file ".."))
  ;;   "l" dired-find-alternate-file
  ;;   ;; "h" 'dired-single-up-directory
  ;;   ;; "l" 'dired-single-buffer
  ;;   )
  )

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; "." to load dot files
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (keymap-set dired-mode-map "." #'dired-hide-dotfiles-mode))

;; (use-package dirvish
;;   :straight t)

(provide 'init-dired)
;;; init-dired.el ends here
