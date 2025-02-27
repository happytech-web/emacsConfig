;; evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ; use C-g to go to normal mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; key-chord: allow press two key quickly to get a command
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

;; set delay
(setq key-chord-tow-keys-delay 0.5)

;;  'jk' to normal mode
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; disable evil and use emacs keybindings in some mode
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'init-evil)
;;; init-evil.el ends here
