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


;; basic functions
(require 'init-basicUI)
(require 'init-package)
(require 'init-ivy)
(require 'init-advancedUI)
(require 'init-helpful)
(require 'init-general)
(require 'init-evil)

;; dev related
;; lsp mode: really heavy, need all of this
(require 'init-lspmode)
(require 'init-lsp-language)
(require 'init-flycheck)
(require 'init-company)

;; language/workenv setup
(require 'init-cc)
(require 'init-python)
(require 'init-rust)

;; dap mode
(require 'init-dapmode)

;; dev
(require 'init-prog-rice)
(require 'init-magit)
(require 'init-direnv)
(require 'init-gc)

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

;; pdf
(require 'init-pdf-tools)
(require 'init-org-noter)



(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "~/Pictures/org-download"))

;;auto display the image when open a org file
(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))


(use-package go-translate
  :ensure t
  :config
  (setq gt-langs '(en zh))
  (setq gt-default-translator (gt-translator :engines (gt-youdao-dict-engine))))

(rune/leader-keys
  "l" '(gt-do-translate :which-key "translation"))

(use-package leetcode
  :ensure t
  :config
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/codes/leetcode")
  (setq leetcode-prefer-language "cpp"))

(use-package svg-tag-mode
  :ensure t
  :hook org-mode
  :init
  (setq svg-lib-style-default
        '(:background "#c3bef0" :foreground "#6639a6" :padding 1 :margin 0
                      :stroke 2 :radius 5 :alignment 0.5 :width 20 :height 0.9
                      :scale 0.5 :ascent center :crop-left nil :crop-right nil
                      :collection "material" :font-family "Fira Code Retina"
                      :font-size 14 :font-weight regular))
  (setq svg-tag-action-at-point 'edit)
  (setq svg-lib-icon-collections
      '(("bootstrap" .
         "https://icons.getbootstrap.com/assets/icons/%s.svg")
        ("simple" .
         "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
        ("material" .
         "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
        ("octicons" .
         "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
        ("boxicons" .
         "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

    ;; [90%]
  (defun svg-progress-percent (value)
    (save-match-data
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                         nil :margin 0 :stroke 2 :radius 3 :padding 2 :height 0.6 :width 11 )
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0 :height 0.6)) :ascent 'center)))

 ;; [30/40]
  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))           
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :height 0.6 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :height 0.6 :margin 0)) :ascent 'center))))

  (setq svg-tag-tags
        `(
          ;; Org tags
          ;("\\(:[A-Za-z0-9]+:\\)" . ((lambda (tag) (svg-tag-make tag))))
                                        ;(":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))


          ;; red [+ans+]
          ("\\[\\+[A-Za-z0-9]+\\+\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-red :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))

          ;; yellow [*check*]
          ("\\[\\*[A-Za-z0-9]+\\*\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-lyellow :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))

          ;; green [-ok-]
          ("\\[-[A-Za-z0-9]+-\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-lgreen :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))
          ;;[#A]
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority :inverse t 
                                              :beg 2 :end -1 :margin 0 :height 0.6))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag)
                       (svg-tag-make "TODO" :face 'org-todo :inverse t
                                     :height 0.6 :margin 0))))

          ("DONE" . ((lambda (tag)
                       (svg-tag-make "DONE" :face 'org-done :inverse t
                                     :height 0.6 :margin 0))))

          ;; #+begin_src CC
          ;("\\(#\\+begin_src \\).+" . ((lambda (tag)
          ;             (svg-tag-make (concat "➫" (substring tag 8)) :face 'all-the-icons-lcyan :inverse t
          ;                           :height 0.5 :margin 0 :crop-right t))))

          ;("#\\+begin_src \\(.+\\)" . ((lambda (tag)
          ;             (svg-tag-make tag :face 'all-the-icons-lcyan :inverse nil 
          ;                           :height 0.5 :margin 0 :crop-left t))))
          ;("#\\+end_src" . ((lambda (tag)
          ;             (svg-tag-make (concat "➬" (substring tag 2)) :face 'all-the-icons-lcyan :inverse t
          ;                           :height 0.5 :margin 0))))

          ;; command
          ;(" =[^ ][^,]*[^ ]= " .((lambda (tag)
          ;                        (svg-tag-make tag :face 'info-node :inverse t
          ;                                      :height 0.5 :margin 0 :beg 1 :end -1))))
          ;; Citation of the form [cite:@Knuth:1984] 
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :height 0.6
                                                          :ascent 'center
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :height 0.6
                                                                   :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :inverse t :margin 0 :height 0.6))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :height 0.6))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :height 0.6))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :inverse t :margin 0 :face 'org-date :height 0.6))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :face 'org-date :height 0.6))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :face 'org-date :height 0.6))))

          ;; ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ))

  (svg-tag-mode t))

;; tab bar background
(set-face-attribute 'tab-bar nil
                    :foreground "#1fab89")

;; active tab
(set-face-attribute 'tab-bar-tab nil
                    :foreground "#d7fbe8")

;; inactive tab
(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "#62d2a2")

(use-package tab-bar
  :hook (window-setup . tab-bar-mode)
  :init
  (setq tab-bar-emoji "💻 ")

  ;; 修改 tab-bar-format，去掉 menu-bar
  (setq tab-bar-format
        '(tab-bar-format-emoji tab-bar-format-tabs))

;; 自定义函数来插入 emoji
  (defun tab-bar-format-emoji ()
    (propertize tab-bar-emoji 'face '(:height 1.1)))
  :config
  (setq tab-bar-separator ""
        tab-bar-show 1  ;; hide bar if less than 1 tabs open
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t)
  ;; menu bar

  ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))

  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; 我把 meow 的 indicator 也放在 tab-bar 上
  ;(setq tab-bar-format '(meow-indicator  tab-bar-format-tabs))
  (tab-bar--update-tab-bar-lines)

  ;; WORKAROUND: update tab-bar for daemon
  ;; (when (daemonp)
  ;;   (add-hook 'after-make-frame-functions
  ;;             #'(lambda (&rest _) (force-mode-line-update))))
  )

(rune/leader-keys
  "t" '(:ignore t :which-key "tab-bar")
  "th" '(tab-bar-switch-to-prev-tab :which-key "prev-tab") 
  "tl" '(tab-bar-switch-to-next-tab :which-key "next-tab") 
  "ti" '(tab-bar-new-tab :which-key "insert-new-tab") 
  "ts" '(tab-bar-select-tab-by-name :which-key "select-tab")
  "td" '(tab-bar-close-tab :which-key "close-tab")
  )

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (setq eaf-proxy-type "http")
;;   (setq eaf-proxy-host "127.0.0.1")
;;   (setq eaf-proxy-port "7897")
;;   (defalias 'browse-web #'eaf-open-browser)
;;   ;;(eaf-setup-leader-keys)
;;   ;;(eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   ;;(eaf-bind-key nil "M-q" eaf-browser-keybinding)

;;   (require 'eaf-browser)
;;   (require 'eaf-rss-reader)
;;   (require 'eaf-image-viewer)
;;   (require 'eaf-airshare)
;;   (require 'eaf-netease-cloud-music)
;;   (require 'eaf-demo)
;;   (require 'eaf-file-sender)
;;   (require 'eaf-js-video-player)
;;   ;;(require 'eaf-pdf-viewer)
;;   ;;(require 'eaf-git)
;;   (require 'eaf-terminal)
;;   ;;(require 'eaf-vue-demo)
;;   (require 'eaf-file-manager)
;;   ;;(require 'eaf-vue-tailwindcss)
;;   (require 'eaf-system-monitor)
;;   (require 'eaf-file-browser)
;;   (require 'eaf-jupyter)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-camera)
;;   (require 'eaf-markmap)
;;   ;;(require 'eaf-pyqterminal)
;;   (require 'eaf-video-player)
;;   (require 'eaf-music-player)
;;   (require 'eaf-map)
;;   (require 'eaf-mindmap)
;;   ;; key bindings and other
;;   (require 'eaf-evil)
;;   (require 'eaf-all-the-icons)
;;   ) ;; unbind, see more in the Wiki

;; (defvar path-to-popweb "~/.emacs.d/pluginTools/popweb")
;; (use-package org-transclusion
;;   :ensure t)
;; (use-package popweb
;;   :load-path path-to-popweb
;;   :config
;;   (setq popweb-url-web-window-width-scale 1.5)
;;   (setq popweb-url-web-window-height-scale 1.5)
;;   ;; Org-Roam ID link and footnote link previewer
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/org-roam"))
;;   (require 'popweb-org-roam-link)

;;   ;; LaTeX preview functionality
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/latex"))
;;   (require 'popweb-latex)
;;   (add-hook 'latex-mode-hook #'popweb-latex-mode)
;;   (add-hook 'org-mode-hook #'popweb-latex-mode)

;;   ;; Chinese-English translation popup
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/dict")) ;
;;   (require 'popweb-dict)

;;   ;; Anki note review popup
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/anki-review"))
;;   (require 'popweb-anki-review)
;;   )

;; (autoload 'LilyPond-mode "lilypond-mode")
;; (setq auto-mode-alist
;;       (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
;; (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
;; (setq load-path (append (list (expand-file-name "/usr/share/emacs/site-lisp")) load-path))

;; (defvar path-to-org-srs "~/.emacs.d/pluginTools/org-srs")
;; (defvar path-to-fsrs "~/.emacs.d/pluginTools/fsrs")

;; (add-to-list 'load-path path-to-org-srs)
;; (add-to-list 'load-path path-to-fsrs)
;; (require 'fsrs)
;; (require 'org-srs)

(use-package uniline
  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(package-selected-packages
   '(rust-mode which-key vterm visual-fill-column uniline typescript-mode treemacs-projectile svg-tag-mode ros rainbow-delimiters pyvenv python-mode pdf-tools ox-hugo ox-gfm org-transclusion org-roam org-noter org-download org-bullets nix-mode magit lsp-ui lsp-pyright lsp-java lsp-ivy leetcode key-chord ivy-rich helpful go-translate general flycheck evil-nerd-commenter evil-collection eterm-256color eshell-git-prompt doom-themes doom-modeline direnv dired-open dired-hide-dotfiles counsel company-box all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
