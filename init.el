;;; setting load path
(defun add-subdirs-to-load-path (dir)
  "递归地将DIR及其所有子目录添加到`load-path`。
   writen by gpt-4o
   这里设置了default-dir, 但是不会影响别的函数，因为这里只是一个let"
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path (expand-file-name "modules" user-emacs-directory))
;; (add-subdirs-to-load-path "~/.emacs.d/modules")

;; startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; basic functions
(require 'init-basicUI)
;; (require 'init-package)
(require 'init-straight)
(require 'init-utils)
(require 'init-general)
(require 'init-basic)
;; (require 'init-ivy)
(require 'init-vertico)
(require 'init-advancedUI)
(require 'init-helpful)
(require 'init-evil)
(require 'init-updater)

;; dev related
(require 'init-highlight)
;; lsp mode: really heavy, need all of this
;; (require 'init-lspmode)
;; (require 'init-lsp-language)
;; (require 'init-flycheck)
;; (require 'init-company)
(require 'init-lspbridge)

;; language/workenv setup
(require 'init-cc)
(require 'init-python)
(require 'init-rust)
(require 'init-just)

;; dap mode
;; (require 'init-dapmode)
(require 'init-dape)

;; dev
(require 'init-treesitter)
(require 'init-magit)
(require 'init-direnv)
(require 'init-gc)
(require 'init-vundo)

;; terminal
(require 'init-term)
(require 'init-vterm)
(require 'init-eshell)


;; dired/file manager
(require 'init-dired)

;; window
(require 'init-window)

;; org
(require 'all-in-one)
(require 'init-org-roam)
(require 'init-org-export)
(require 'init-org-download)

;; pdf
(require 'init-pdf-tools)
(require 'init-org-noter)

;; others/works
(require 'init-go-translation)
(require 'init-leetcode)

;; tab bar
(require 'init-tab-bar)

;; ai
(require 'init-gptel)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(vue-mode rust-mode which-key vterm visual-fill-column uniline typescript-mode treemacs-projectile svg-tag-mode ros rainbow-delimiters pyvenv python-mode pdf-tools ox-hugo ox-gfm org-transclusion org-roam org-noter org-download org-bullets nix-mode magit lsp-ui lsp-pyright lsp-java lsp-ivy leetcode key-chord ivy-rich helpful go-translate general flycheck evil-nerd-commenter evil-collection eterm-256color eshell-git-prompt doom-themes doom-modeline direnv dired-open dired-hide-dotfiles counsel company-box all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
