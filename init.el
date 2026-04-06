;;; init.el --- Minimal Emacs config -*- lexical-binding: t; -*-

;; Keep runtime data in ~/.emacs.d/.emacs-data/
(defconst my/emacs-data-dir (expand-file-name ".emacs-data/" user-emacs-directory))
(defconst my/cache-dir (expand-file-name "cache/" my/emacs-data-dir))

(dolist (dir (list my/emacs-data-dir
                   my/cache-dir
                   (expand-file-name "backup/" my/cache-dir)
                   (expand-file-name "auto-save/" my/cache-dir)
                   (expand-file-name "auto-save/sessions/" my/cache-dir)
                   (expand-file-name "url/" my/cache-dir)
                   (expand-file-name "transient/" my/cache-dir)
                   (expand-file-name "straight/" my/emacs-data-dir)
                   (expand-file-name "eln-cache/" my/emacs-data-dir)))
  (make-directory dir t))

;; Store generated state and caches outside top-level config files.
(setq custom-file (expand-file-name "custom.el" my/emacs-data-dir)
      backup-directory-alist `(("." . ,(expand-file-name "backup/" my/cache-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" my/cache-dir) t))
      auto-save-list-file-prefix (expand-file-name "auto-save/sessions/" my/cache-dir)
      tramp-persistency-file-name (expand-file-name "tramp" my/cache-dir)
      url-history-file (expand-file-name "url/history" my/cache-dir)
      savehist-file (expand-file-name "savehist" my/cache-dir)
      recentf-save-file (expand-file-name "recentf" my/cache-dir)
      bookmark-default-file (expand-file-name "bookmarks" my/cache-dir)
      project-list-file (expand-file-name "projects" my/cache-dir)
      transient-history-file (expand-file-name "transient/history.el" my/cache-dir)
      transient-levels-file (expand-file-name "transient/levels.el" my/cache-dir)
      transient-values-file (expand-file-name "transient/values.el" my/cache-dir)
      package-user-dir (expand-file-name "elpa" my/emacs-data-dir)
      native-comp-eln-load-path (list (expand-file-name "eln-cache/" my/emacs-data-dir)))

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

;; Load local modules from ~/.emacs.d/modules/
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/coding/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/search-prompt/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/dev/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/org/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/tab-bar/" user-emacs-directory))

;; Basic UX defaults.
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-short-answers t)

;; Clipboard integration: make kill/yank work with system clipboard.
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      yank-pop-change-selection t
      kill-do-not-save-duplicates t)

;; Use wl-copy/wl-paste when available (works for both GUI and TTY on Wayland).
;; Fall back to Emacs native backend if wl-clipboard is unavailable.
(when (and (eq system-type 'gnu/linux)
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (defun my/ensure-wayland-env ()
    "Populate WAYLAND_DISPLAY/XDG_RUNTIME_DIR when Emacs is missing them."
    (let* ((uid (number-to-string (user-uid)))
           (runtime (or (getenv "XDG_RUNTIME_DIR")
                        (expand-file-name (concat "/run/user/" uid)))))
      (when (and runtime (file-directory-p runtime))
        (unless (getenv "XDG_RUNTIME_DIR")
          (setenv "XDG_RUNTIME_DIR" runtime))
        (unless (getenv "WAYLAND_DISPLAY")
          (let ((socket (car (directory-files runtime nil "^wayland-[0-9]+$"))))
            (when socket
              (setenv "WAYLAND_DISPLAY" socket)))))))
  (defun my/wl-copy (text)
    "Copy TEXT to system clipboard through wl-copy."
    (my/ensure-wayland-env)
    (ignore-errors
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "wl-copy" nil nil nil "-n"))))
  (defun my/wl-paste ()
    "Paste text from system clipboard through wl-paste."
    (my/ensure-wayland-env)
    (ignore-errors
      (with-temp-buffer
        (when (eq 0 (call-process "wl-paste" nil t nil "-n"))
          (buffer-string)))))
  (setq interprogram-cut-function #'my/wl-copy
        interprogram-paste-function #'my/wl-paste))

;; Bootstrap straight.el into ~/.emacs.d/.emacs-data/straight/
(defvar bootstrap-version)
(defvar my/straight-ready nil)
(setq straight-base-dir (expand-file-name "straight/" my/emacs-data-dir))
(let ((legacy-straight-dir (expand-file-name "straight/" user-emacs-directory)))
  (when (and (file-exists-p (expand-file-name "repos/straight.el/bootstrap.el"
                                              legacy-straight-dir))
             (not (file-exists-p (expand-file-name "repos/straight.el/bootstrap.el"
                                                   straight-base-dir))))
    (copy-directory legacy-straight-dir straight-base-dir t t t)))
(let* ((bootstrap-file (expand-file-name "repos/straight.el/bootstrap.el" straight-base-dir))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (let ((buf (ignore-errors
                 (url-retrieve-synchronously
                  "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                  'silent 'inhibit-cookies))))
      (if (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (eval-print-last-sexp)
            (kill-buffer buf))
        (message "straight.el install skipped: cannot download installer now."))))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)
    (setq my/straight-ready t)))

(when my/straight-ready
  (setq straight-use-package-by-default t
        straight-check-for-modifications '(check-on-save find-when-checking)
        straight-vc-git-default-clone-depth 1)
  (straight-use-package 'use-package)
  (require 'use-package))

;; Small built-in packages as examples.
(if my/straight-ready
    (use-package emacs
      :ensure nil
      :config
      (savehist-mode 1)
      (recentf-mode 1)
      (global-auto-revert-mode 1))
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1))

;; UI modules from your existing config.
(require 'init-basicUI)

(when my/straight-ready
  (require 'init-vertico)
  (require 'init-consult-treesit-imenu)
  (require 'init-corfu)
  (require 'init-emacs-dev)
  (require 'init-org-core)
  (require 'init-org-ui)
  (require 'init-org-roam)
  (require 'init-org-paper)
  (require 'init-org-download)
  (require 'init-org-export)
  (require 'init-markdown)
  (require 'init-svg-tag)
  (require 'init-typst)
  (require 'init-dired)
  (require 'init-dirvish)
  (require 'init-tab-bar)
  (require 'init-treesitter)
  (require 'init-direnv)
  (require 'init-eglot)
  (require 'init-meow)
  (require 'init-advancedUI)
  (require 'init-magit)
  )

;; Restore startup-tuned values after init.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 64 1024 1024)
         gc-cons-percentage 0.1
         file-name-handler-alist
         (delete-dups (append file-name-handler-alist
                              my/default-file-name-handler-alist)))
   (message "Emacs ready in %.2fs, %d GCs"
            (float-time (time-subtract after-init-time before-init-time))
            gcs-done)))

;;; init.el ends here
