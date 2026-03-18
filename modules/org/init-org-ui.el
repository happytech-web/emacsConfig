;;; init-org-ui.el --- Org UI and font layering -*- lexical-binding: t; -*-

(defun my/org-ui-set-face-if-exists (face &rest args)
  "Set FACE with ARGS only when FACE exists."
  (when (facep face)
    (apply #'set-face-attribute face nil args)))

(defun my/org-ui-apply-font-faces ()
  "Apply font faces for org buffer sections.
Prose uses `variable-pitch' (LXGW from init-basicUI), code-like parts use
`fixed-pitch' (Fira Code from init-basicUI)."
  (with-eval-after-load 'org
    ;; Headings and title: variable pitch for better reading.
    (my/org-ui-set-face-if-exists 'org-document-title :inherit 'variable-pitch :weight 'bold :height 1.25)
    (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4
                    org-level-5 org-level-6 org-level-7 org-level-8))
      (my/org-ui-set-face-if-exists face :inherit 'variable-pitch :weight 'semi-bold))

    ;; Fixed-pitch regions in org.
    (dolist (face '(org-block
                    org-block-begin-line
                    org-block-end-line
                    org-code
                    org-verbatim
                    org-table
                    org-formula
                    org-meta-line
                    org-special-keyword
                    org-checkbox
                    org-document-info-keyword
                    org-drawer
                    org-property-value
                    org-tag
                    org-date
                    line-number
                    line-number-current-line))
      (my/org-ui-set-face-if-exists face :inherit 'fixed-pitch))
    ;; Keep visual indentation glyphs width-stable.
    (my/org-ui-set-face-if-exists 'org-indent :inherit '(org-hide fixed-pitch))

    (my/org-ui-set-face-if-exists 'org-ellipsis :inherit '(fixed-pitch default)))

  ;; Heading bullets from org-superstar use Fira Code while heading text stays LXGW.
  (with-eval-after-load 'org-superstar
    (my/org-ui-set-face-if-exists 'org-superstar-header-bullet :inherit 'fixed-pitch)
    (my/org-ui-set-face-if-exists 'org-superstar-item :inherit 'fixed-pitch)))

(defun my/org-ui-mode-setup ()
  "Enable org UI defaults for writing."
  (variable-pitch-mode 1)
  (setq-local line-spacing 0.2))

(defun my/org-center-document ()
  "Center org document with a readable text width."
  (setq-local visual-fill-column-width 110
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :straight nil
  :ensure nil
  :custom
  (org-hide-leading-stars nil)
  (org-startup-indented t)
  :hook ((org-mode . my/org-ui-mode-setup)
         (org-mode . org-indent-mode))
  :config
  (my/org-ui-apply-font-faces)
  ;; Themes may reset org faces; re-apply after theme changes.
  (advice-add 'load-theme :after (lambda (&rest _) (my/org-ui-apply-font-faces))))

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode))

(use-package org-appear
  :straight t
  :after org
  :custom
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t)
  (org-appear-autokeywords t)
  :hook (org-mode . org-appear-mode))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . my/org-center-document))

(provide 'init-org-ui)
;;; init-org-ui.el ends here
