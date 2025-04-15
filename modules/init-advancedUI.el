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
  :init (load-theme 'doom-moonlight t)
  )

(use-package ef-themes
  :ensure t
  ;; :init (load-theme 'ef-arbutus t)
  )


(set-frame-parameter nil 'alpha-background 60)

(add-to-list 'default-frame-alist '(alpha-background . 60))

;; Set transparency of emacs
;(defun transparency (value)
;  "Sets the transparency of the frame window. 0=transparent/100=opaque"
;  (interactive "nTransparency Value 0 - 100 opaque:")
;  (set-frame-parameter (selected-frame) 'alpha value))


;;doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package nyan-mode
  :straight (nyan-mode :type git :host github :repo "zakudriver/nyan-mode")
  :hook (after-init . nyan-mode)
  :custom
  (nyan-cat-flavor       'jazz)
  (nyan-bar-length       40)
  (nyan-animate-nyancat  t)
  (nyan-wavy-trail       t)
  (nyan-animation-frames 10))


(provide 'init-advancedUI)
;;; init-advancedUI.el ends here
