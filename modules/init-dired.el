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

  ;; --------------- zoxide integration ---------------
  (defun my/zoxide-list-dirs ()
    "Return directories from zoxide database."
    (unless (executable-find "zoxide")
      (user-error "zoxide not found in PATH"))
    (condition-case _err
        (let ((dirs (seq-filter
                     (lambda (s) (> (length s) 0))
                     (process-lines "zoxide" "query" "-l"))))
          (if dirs
              dirs
            (user-error "zoxide database is empty")))
      (error
       (user-error "Failed to query zoxide database"))))

  (defun my/dired-zoxide-jump (&optional other-window)
    "Jump to a zoxide directory from dired using consult/completion."
    (interactive "P")
    (let* ((cands (my/zoxide-list-dirs))
           (prompt "Zoxide jump: ")
           (target
            (if (fboundp 'consult--read)
                (consult--read
                 cands
                 :prompt prompt
                 :sort nil
                 :require-match t
                 :category 'file
                 :state (when (fboundp 'consult--file-preview)
                          (consult--file-preview)))
              (completing-read prompt cands nil t))))
      (if other-window
          (dired-other-window target)
        (dired target))))

  (keymap-set dired-mode-map "h" (lambda () (interactive) (find-alternate-file "..")))
  (keymap-set dired-mode-map "l" #'dired-find-alternate-file)
  (keymap-set dired-mode-map "z" #'consult-fd)
  (keymap-set dired-mode-map "Z" #'my/dired-zoxide-jump)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" (lambda()(interactive)(find-alternate-file ".."))
  ;;   "l" dired-find-alternate-file
  ;;   ;; "h" 'dired-single-up-directory
  ;;   ;; "l" 'dired-single-buffer
  ;;   )
  )

;; (with-eval-after-load 'dirvish
  ;; (keymap-set dirvish-mode-map "Z" #'my/dired-zoxide-jump))

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
