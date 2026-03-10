(defvar bootstrap-version)

;; Use straight.el as the only package manager.
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(require 'use-package)

;; `:ensure' belongs to package.el; keep it disabled when using straight.
(setq use-package-always-ensure nil
      use-package-enable-imenu-support t)

(provide 'init-straight)
;;; init-straight.el ends here
