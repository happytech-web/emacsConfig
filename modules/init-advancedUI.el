;;icon
;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
;; M-x nerd-icons-install-fonts

(use-package all-the-icons
  :if (display-graphic-p))

;;theme
(use-package doom-themes
  :init (load-theme 'doom-moonlight t))

;; Set transparency of emacs
;(defun transparency (value)
;  "Sets the transparency of the frame window. 0=transparent/100=opaque"
;  (interactive "nTransparency Value 0 - 100 opaque:")
;  (set-frame-parameter (selected-frame) 'alpha value))

(set-frame-parameter nil 'alpha-background 60)

(add-to-list 'default-frame-alist '(alpha-background . 60))

;;doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(provide 'init-advancedUI)
;;; init-advancedUI.el ends here
