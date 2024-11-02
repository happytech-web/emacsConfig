;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 200)
(defvar efs/default-variable-font-size 250)

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ;disable visible scrollbar
(tool-bar-mode -1)   ;disable toolbar
(tooltip-mode -1)    ;disable tooltips
(set-fringe-mode 10) ;give some breathing room

(menu-bar-mode -1)   ;disable the munu bar

;; set up the visible bell
(setq visible-bell t)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
              term-mode-hook
              shell-mode-hook
              vterm-mode-hook
              treemacs-mode-hook
              eshell-mode-hook
              pdf-view-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;; parenthesis completion
(electric-pair-mode 1)

;; Font Configuration ------------------------------------------------------

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height efs/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "LXGW WenKai" :height efs/default-variable-font-size :weight 'regular)
(setq all-the-icons-dired-monochrome nil)  ;; 关闭单色图标模式,在文件系统中看起来更好

;; 中文字体设置
;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;  (set-fontset-font (frame-parameter nil 'font)
;charset
;                    (font-spec :family "Microsoft YaHei UI" :size 25)))

;; to check if some font match, use following code

;(dolist (font (font-family-list))
;  (when (string-match "sans" font)
;    (message "Found font: %s" font)))

(load-theme 'wombat)

;; package loading configuration

;; initialize package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; disable verify signiture
;; because gpg can't work
(setq package-check-signature nil)

;; show the options while hit C-x ...
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
    :config
    (counsel-mode 1))

;; provide more doc after command
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

;; which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; make help more helpful, add source code and some command in
;; help doc
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; config keybindings more convenient
(use-package general
  :config
  ;; rune/leader-keys is a variable (user difined var)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
;; t for toggle (change)
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

(use-package flycheck
  :ensure t
  :config
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  :hook
  (prog-mode . flycheck-mode))

;; giving a hierarchy in a file
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; lsp mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-, l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
        ("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(use-package lsp-ivy)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
                                        ;:custom
  :custom
  (dap-auto-configure-features '(locals expressions controls))
  (setq lsp-enable-dap-auto-configure t)
  :config
  (dap-ui-mode 1)
  (setq dap-auto-show-output 1)

  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  ;; Set up GDB
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)

  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  ;; Bind `, l d` to `dap-hydra` for easy access
  (general-def
    :prefix ","
    :states 'motion
    :keymaps 'lsp-mode-map
    ;; dap
    "d" '(nil :which-key "dap")
    "dr" '(dap-debug :which-key "debug")
    "di" '(dap-breakpoint-add :which-key "add breakpoint")
    "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
    "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
    "dc" '(dap-breakpoint-condition :which-key "condition")
    "dh" '(dap-hydra t :wk "helpper map"))
  ;(general-define-key
  ; :keymaps 'lsp-mode-map
  ; :prefix lsp-keymap-prefix
  ; "d" '(dap-hydra t :wk "debugger"))
  )

(defun file-name-only ()
  "Get the current buffer file name without directory."
  (file-name-nondirectory (buffer-name)))

(defun file-name-only-noext ()
  "Get the currennt buffer file name without directory and extension."
  (file-name-sans-extension (file-name-only)))

;; set compile command
;; if want to change, "M-x add-dir-local-variable" add edit dir-locals.el
;;(c++-mode . ((compile-command . (concat "g++ -g "
;;                                        (file-name-only)
;;                                         " -o "
;;                                        (file-name-only-noext)))))

(defun compile-cpp-file ()
  (interactive)
  (let* ((compile-command (format "g++ %s -g -o %s" (file-name-only) (file-name-only-noext))))

    (shell-command compile-command "*compilation*")))

(defun compile-and-run-cpp-file ()
  (interactive)
  (let* ((compile-command (format "g++ %s -g -o %s" (file-name-only) (file-name-only-noext)))
         (run-command (format "./%s" (file-name-only-noext))))

    (shell-command compile-command "*compilation*")
    (async-shell-command run-command "*cpp-output*")))

(defun compile-c-file ()
  (interactive)
  (let* ((compile-command (format "gcc %s -g -o %s" (file-name-only) (file-name-only-noext))))

    (shell-command compile-command)))

(defun compile-and-run-c-file ()
  (interactive)
  (let* ((compile-command (format "gcc %s -g -o %s" (file-name-only) (file-name-only-noext)))
         (run-command (format "./%s" (file-name-only-noext))))

    (shell-command compile-command "*compilation*")
    (async-shell-command run-command "*c-output*")))

(use-package cc-mode
  :ensure nil
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state))

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'c-mode-map
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;;c mode custom
  "c" '(nil :which-key "compile")
  "cs" '(compile-c-file :which-key "simple compile")
  "cr" '(compile-and-run-c-file :which-key "compile and run"))

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'c++-mode-map
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;; cpp mode custom
  "c" '(nil :which-key "compile")
  "cs" '(compile-cpp-file :which-key "simple compile")
  "cr" '(compile-and-run-cpp-file :which-key "compile and run"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))



  ;  (use-package pyvenv
  ;    :ensure t
  ;    :config
  ;    (setenv "WORKON_HOME" (expand-file-name "d:/dev/anaconda/envs"))
      ;; (setq python-shell-interpreter "python3")  ; （可选）更改解释器名字
  ;    (pyvenv-mode t)
      ;; （可选）如果希望启动后激活 anaconda 的 base 环境，就使用如下的 hook
                                            ;:hook
                                            ;(python-mode . (lambda () (pyvenv-workon "..")))
  ;    )

  ;  (defun my-pyvenv-workon-hook ()
  ;    "Restart lsp and update Flycheck when switching Python virtual environments."
      ;; Restart lsp-mode
  ;    (when (bound-and-true-p lsp-mode)
  ;      (lsp-restart-workspace))

      ;; Update Flycheck executables
  ;    (setq-local flycheck-python-pycompile-executable (executable-find "python"))
  ;    (setq-local flycheck-python-pyright-executable (executable-find "pyright"))
  ;    (setq-local flycheck-python-flake8-executable (executable-find "flake8"))
  ;    (setq-local flycheck-python-pylint-executable (executable-find "pylint"))

      ;; Restart Flycheck to use the new executables
  ;    (flycheck-buffer))

  ;  (add-hook 'pyvenv-post-activate-hooks 'my-pyvenv-workon-hook)

(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                  (require 'lsp-pyright)
                  (lsp-deferred))))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))

(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(use-package dap-java
  :ensure nil)

(general-def
  :prefix ","
  :states 'motion
  :keymaps 'java-mode-map
  ;; lsp
  "l" '(:keymap lsp-command-map :which-key "lsp")
  ;; dap
  "d" '(nil :which-key "dap")
  "dr" '(dap-debug :which-key "debug")
  "di" '(dap-breakpoint-add :which-key "add breakpoint")
  "dd" '(dap-breakpoint-delete :which-key "delete breakpoint")
  "dD" '(dap-breakpoint-delete-all :which-key "delete all breakpoints")
  "dc" '(dap-breakpoint-condition :which-key "condition")
  ;; test
  "t" '(nil :which-key "test")
  "tc" '(dap-java-run-test-class :which-key "run class")
  "tm" '(dap-java-run-test-method :which-key "run method")
  "tl" '(dap-java-run-last-test :which-key "run last")
  "tM" '(dap-java-debug-test-method :which-key "debug method")
  "tC" '(dap-java-debug-test-class :which-key "debug class"))

(use-package ros
  :ensure t
  :config
  (setq ros-workspaces
        (list
         (ros-dump-workspace :tramp-prefix nil :workspace "~/codes/rosWorkspace/tutorial_ws" :extends '("/opt/ros/humble/")))
        )
  )

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

;;   (use-package company
;;     :ensure t
;;     :after lsp-mode
;;     :hook (lsp-mode . company-mode)
;;     :bind (:map company-active-map
;;                   ("<tab>" . company-complete-selection))
;;       (:map lsp-mode-map
;;             ("<tab>" . company-indent-or-complete-common))
;;       :custom
;;       (company-minimum-prefix-length 1)
;;       (company-idle-delay 0.0)
;;       (company-show-numbers t) ; M-1, M-2 ... to select the card
;;       (company-selection-wrap-around t)
;;       (company-transformers '(company-sort-by-occurrence))) ; frequency sort

 (use-package company
   :diminish company-mode
   :general
   (general-define-key :keymaps 'company-active-map
                       "C-j" 'company-select-next
                       "C-k" 'company-select-previous)
   :bind (:map company-active-map
               ("<tab>" . company-complete-selection))
   (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
   :init
   ;; These configurations come from Doom Emacs:
   (add-hook 'after-init-hook 'global-company-mode)
   (setq company-minimum-prefix-length 1
         company-tooltip-limit 14
         company-tooltip-align-annotations t
         company-require-match 'never
         company-global-modes '(not erc-mode message-mode help-mode gud-mode)
         company-frontends
         '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
           company-echo-metadata-frontend)  ; show selected candidate docs in echo area
         company-backends '(company-capf company-files company-keywords)
         company-auto-complete nil
         company-auto-complete-chars nil
         company-dabbrev-other-buffers nil
         company-dabbrev-ignore-case nil
         company-dabbrev-downcase nil)

   :custom
   (company-show-numbers t)
   (company-selection-wrap-around t)
   (company-transformers '(company-sort-by-occurrence))
   (company-idle-delay 0.0))

(use-package company-box
    :ensure t
    :if window-system
    :hook (company-mode . company-box-mode))

;; rainbow bracket
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; make profectile command bind to ivy
  :custom ((projectile-completion-system 'ivy))
  ;; C-c p to use projectile command
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init

  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;; in my case: d:/codes/projects

  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; magit
;; set the git path, or emacs can not find git

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; evil-magit has merged into magit, we don't need it anymore!

;(use-package evil-magit
  ;:after magit)

;; I think I don't need it for while
;; for future using, use it to manage github related things

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;(use-package forge)

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; "H" to load dot files
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . 'ace-window)))

;   (use-package edwina
;     :ensure t
;     :config
;     (setq display-buffer-base-action '(display-buffer-below-selected))
     ;; (edwina-setup-dwm-keys)
;     (edwina-mode 1))

(use-package winner-mode
  :ensure nil
  :bind (:map evil-window-map
         ("u" . winner-undo)
         ("U" . winner-redo))
  :config
  (winner-mode))

;(use-package golden-ratio
;  :ensure t
;  :config
;  (golden-ratio-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "LXGW WenKai" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :ensure t
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; set org agenda path
  (setq org-directory "~/RoamNotes/Archive")
  (setq org-agenda-files '("work.org" "habits.org" ))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("work.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/.emacs.d/utilFiles/orgFiles/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/.emacs.d/utilFiles/orgFiles/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/.emacs.d/utilFiles/orgFiles/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/.emacs.d/utilFiles/orgFiles/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/.emacs.d/utilFiles/orgFiles/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
  )

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("cp" . "src C++"))
(add-to-list 'org-structure-template-alist '("cc" . "src C"))

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; don't ask me if i want to execute
(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our init.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/utilFiles/initFiles/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;(use-package org-modern
;  :hook (org-mode . org-modern-mode))

(require 'ox-latex)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-gfm
  :ensure t
  :after org)



(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)

  (org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)

   ;; notes menu
   ("n" "notes")

   ("no" "notes overview" plain
    (file "~/RoamNotes/Templates/noteOverview.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)

   ("nc" "course notes" plain
    (file "~/RoamNotes/Templates/courseNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ("nh" "chapter notes" plain
    (file "~/RoamNotes/Templates/chapterNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ("nk" "little knowledge" plain
    (file "~/RoamNotes/Templates/knowledgeNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ;; work menu
   ;; create stuff in tmp.org, and then refile it into work.org
   ;; notice that in this way will not create id
   ("w" "Work Todo Entries")


   ("we" "No Time" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title} %?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("ws" "Scheduled" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("wd" "Deadline" entry 
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("ww" "Scheduled & deadline" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ))

  (org-roam-dailies-capture-templates
  '(("d" "default" entry "* %<%I:%M %p>: %?"
     :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("w" "Work Todo Entries")

   ("we" "No Time" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo} %?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("ws" "Scheduled" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nSCHEDULED: %^t%?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("wd" "Deadline" entry 
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nDEADLINE: %^t%?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("ww" "Scheduled & deadline" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nSCHEDULED: %^t DEADLINE: %^t %?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
    ))


  :bind (:map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; integrate with general key, <spc> is the prefix
(rune/leader-keys
  "o"  '(:ignore t :which-key "org roam")
  "ol" '(org-roam-buffer-toggle :which-key "backlinks-buffer")
  "of" '(org-roam-node-find :which-key "find-node")
  "oi" '(org-roam-node-insert :which-key "insert-node")
  "oc" '(org-id-get-create :which-key "create-id")
  "oa" '(org-roam-alias-add :which-key "add-alias")
  "od" '(:ignore t :which-key "dailies")
  "od." '(org-roam-dailies-find-directory :which-key "daily dir")
  "odh" '(org-roam-dailies-goto-previous-note :which-key "goto-previous")
  "odl" '(org-roam-dailies-goto-next-note :which-key "goto-next")
  "odD" '(org-roam-dailies-goto-today :which-key "goto-today")
  "odY" '(org-roam-dailies-goto-yesterday :which-key "goto-yesterday")
  "odT" '(org-roam-dailies-goto-tomorrow :which-key "goto-tomorrow")
  "odS" '(org-roam-dailies-goto-date :which-key "goto-date")
  "odd" '(org-roam-dailies-capture-today :which-key "capture-today")
  "ody" '(org-roam-dailies-capture-yesterday :which-key "capture-yesterday")
  "odt" '(org-roam-dailies-capture-tomorrow :which-key "capture-tomorrow")
  "ods" '(org-roam-dailies-capture-date :which-key "capture-date")
  )

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(use-package org-noter
  :ensure t
  :config
  (org-noter-enable-org-roam-integration)
  (setq org-noter-always-create-frame nil)
  )

(rune/leader-keys
  "on"  '(:ignore t :which-key "org-noter")
  "onn" '(org-noter :which-key "open-noter")
  "onk" '(org-noter-kill-session :which-key "kill-session"))

(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "~/Pictures/org-download"))

;;auto display the image when open a org file
(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))

(use-package pdf-tools
  :defer t
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-height)
  (setq pdf-view-continuous nil) ;; Makes it so scrolling down to the bottom/top of a page doesn't switch to the next page
  ;(setq pdf-view-midnight-colors '("#ffffff" . "#121212" )) ;; I use midnight mode as dark mode, dark mode doesn't seem to work
  :general
  ;; Unbind SPC key in pdf-view-mode to avoid conflicts with general
  (general-define-key :states 'motion :keymaps 'pdf-view-mode-map
                      "C-j" 'pdf-view-next-page
                      "C-k" 'pdf-view-previous-page

                      "j" 'pdf-view-next-line-or-next-page
                      "k" 'pdf-view-previous-line-or-previous-page

                      ;; Arrows for movement as well
                      (kbd "<down>") 'pdf-view-next-line-or-next-page
                      (kbd "<up>") 'pdf-view-previous-line-or-previous-page

                      (kbd "<down>") 'pdf-view-next-line-or-next-page
                      (kbd "<up>") 'pdf-view-previous-line-or-previous-page

                      (kbd "<left>") 'image-backward-hscroll
                      (kbd "<right>") 'image-forward-hscroll

                      "H" 'pdf-view-fit-height-to-window
                      "W" 'pdf-view-fit-width-to-window
                      "=" 'pdf-view-enlarge
                      "-" 'pdf-view-shrink

                      "q" 'quit-window
                      "Q" 'kill-this-buffer
                      "g" 'revert-buffer

                      "C-s" 'isearch-forward
                      )
  )

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
  "tk" '(tab-bar-switch-to-prev-tab :which-key "prev-tab") 
  "tj" '(tab-bar-switch-to-next-tab :which-key "next-tab") 
  "ti" '(tab-bar-new-tab :which-key "insert-new-tab") 
  "ts" '(tab-bar-select-tab-by-name :which-key "select-tab")
  )

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (setq eaf-proxy-type "http")
  (setq eaf-proxy-host "127.0.0.1")
  (setq eaf-proxy-port "7897")
  (defalias 'browse-web #'eaf-open-browser)
  ;;(eaf-setup-leader-keys)
  ;;(eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;;(eaf-bind-key nil "M-q" eaf-browser-keybinding)

  (require 'eaf-browser)
  (require 'eaf-rss-reader)
  (require 'eaf-image-viewer)
  (require 'eaf-airshare)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-demo)
  (require 'eaf-file-sender)
  (require 'eaf-js-video-player)
  ;;(require 'eaf-pdf-viewer)
  ;;(require 'eaf-git)
  (require 'eaf-terminal)
  ;;(require 'eaf-vue-demo)
  (require 'eaf-file-manager)
  ;;(require 'eaf-vue-tailwindcss)
  (require 'eaf-system-monitor)
  (require 'eaf-file-browser)
  (require 'eaf-jupyter)
  (require 'eaf-markdown-previewer)
  (require 'eaf-camera)
  (require 'eaf-markmap)
  ;;(require 'eaf-pyqterminal)
  (require 'eaf-video-player)
  (require 'eaf-music-player)
  (require 'eaf-map)
  (require 'eaf-mindmap)
  ;; key bindings and other
  (require 'eaf-evil)
  (require 'eaf-all-the-icons)
  ) ;; unbind, see more in the Wiki
