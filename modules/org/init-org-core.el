;;; init-org-core.el --- Core org-mode config -*- lexical-binding: t; -*-

(use-package org
  :straight nil
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode))
  :custom
  (org-startup-indented nil)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  :config
  ;; Enable <s / <el / <sh template expansion.
  (require 'org-tempo)

  ;; Minimal Babel languages (same spirit as r-writing.el).
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (shell . t)
          (python . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages)

  (defun my/org-checkbox-state-at-point ()
    "Return checkbox state on current line."
    (save-excursion
      (goto-char (line-beginning-position))
      (cond
       ((re-search-forward " \\[X\\] " (line-end-position) t) 'checked)
       ((re-search-forward " \\[-\\] " (line-end-position) t) 'partial)
       ((re-search-forward " \\[ \\] " (line-end-position) t) 'unchecked)
       (t 'unknown))))

  (defun my/org-checkbox-done-timestamp-range ()
    "Return the bounds of timestamp after checkbox, if present."
    (let ((re " \\[[ X-]\\] \\(\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\] \\)"))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward re (line-end-position) t)
          (cons (match-beginning 1) (match-end 1))))))

  (defun my/org-checkbox-update-timestamp ()
    "Add/remove done timestamp when checkbox state changes."
    (save-excursion
      (let ((state (my/org-checkbox-state-at-point))
            (range (my/org-checkbox-done-timestamp-range)))
        (if (eq state 'checked)
            (unless range
              (goto-char (line-beginning-position))
              (re-search-forward " \\[X\\] " (line-end-position) t)
              (org-insert-timestamp (current-time) t t)
              (insert " "))
          (when range
            (delete-region (car range) (cdr range)))))))

  (add-hook 'org-checkbox-statistics-hook #'my/org-checkbox-update-timestamp))

(provide 'init-org-core)
;;; init-org-core.el ends here
