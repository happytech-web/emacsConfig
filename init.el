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
(require 'init-dirvish)

;; window
(require 'init-window)

;; org
(require 'all-in-one)
;; (require 'init-org)
(require 'init-org-roam)
(require 'init-org-export)
(require 'init-org-download)
;; (require 'init-svg-tag)

;; pdf
(require 'init-pdf-tools)
;; (require 'init-org-noter)
;; (require 'init-emacs-reader)

;; others/works
(require 'init-go-translation)
(require 'init-leetcode)

;; tab bar
(require 'init-tab-bar)

;; ai
(require 'init-gptel)


;; rss
(require 'init-rss)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
