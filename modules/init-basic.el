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

(provide 'init-basic)
