;; config keybindings more convenient
(use-package general
  :config
  ;; rune/leader-keys is a variable (user difined var)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
    "s"  '(:ignore t :which-key "scale-switch")
    "st" '(counsel-load-theme :which-key "switch theme")))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ss" '(hydra-text-scale/body :which-key "scale text"))

(provide 'init-general)
;;; init-general.el ends here
