;;; init-rice-coding.el --- Visual coding enhancements -*- lexical-binding: t; -*-

(use-package indent-bars
  :straight t
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.7))
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-lists 'skip)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-ts-color '(unspecified :blend 0.45))
  (indent-bars-treesit-scope
   '((python function_definition class_definition for_statement
      if_statement with_statement while_statement)
     (c function_definition struct_specifier class_specifier
      for_statement if_statement while_statement)
     (cpp function_definition struct_specifier class_specifier
      for_statement if_statement while_statement)
     (rust function_item impl_item struct_item enum_item
      for_expression if_expression while_expression match_expression)))
  :hook ((python-ts-mode c-ts-mode c++-ts-mode rust-ts-mode yaml-ts-mode)
         . indent-bars-mode))

(use-package treesitter-context
  :straight (treesitter-context :type git :host github :repo "zbelial/treesitter-context.el")
  :after posframe
  :custom
  (treesitter-context-idle-time 0.5)
  (treesitter-context-show-context-always t)
  (treesitter-context-show-line-number t)
  (treesitter-context-frame-autohide-timeout 10)
  (treesitter-context-fold-ellipsis-content " ⤵")
  :hook ((python-ts-mode c-ts-mode c++-ts-mode rust-ts-mode)
         . treesitter-context-mode)
   :config
  (with-eval-after-load 'treesitter-context-fold
    (set-face-foreground 'treesitter-context-fold-ellipsis-face
                         (face-attribute 'font-lock-comment-face :foreground)))
  (add-hook 'treesitter-context-mode-hook
            (lambda ()
              (when treesitter-context-mode
                (treesitter-context-fold-mode 1))))

  (defvar-local my/fold-cycle-state 0
    "Cycle state: 0=all open, 1=overview, 2=content.")

  (defun my/fold--fold-types ()
    "Return fold node types for current major mode."
    (cond
     ((derived-mode-p 'python-ts-mode) treesitter-context--python-fold-node-types)
     ((derived-mode-p 'c-ts-mode) treesitter-context--c-fold-node-types)
     ((derived-mode-p 'c++-ts-mode) treesitter-context--c++-fold-node-types)
     ((derived-mode-p 'rust-ts-mode) treesitter-context--rust-fold-node-types)))

  (defun my/fold--node-region (node)
    "Return (beg . end) for folding NODE, skipping its first line."
    (let ((beg (treesit-node-start node))
          (end (treesit-node-end node)))
      (save-excursion
        (goto-char beg)
        (setq beg (line-end-position)))
      (when (> end beg)
        (cons beg end))))

  (defun my/fold--fold-node (node)
    (when-let ((region (my/fold--node-region node)))
      (treesitter-context-fold--hide-region (car region) (cdr region))))

  (defun my/fold--unfold-node (node)
    "Remove the fold overlay exactly matching NODE's region."
    (when-let ((region (my/fold--node-region node)))
      (mapc #'delete-overlay
            (treesitter-context-fold--get-exact-overlays (car region) (cdr region)))))

  (defun my/fold--get-top-level-nodes ()
    "Return top-level foldable nodes (no foldable ancestor)."
    (let* ((root (treesit-buffer-root-node))
           (fold-types (my/fold--fold-types))
           result)
      (when (and root fold-types)
        (treesit-search-subtree
         root
         (lambda (node)
           (when (member (treesit-node-type node) fold-types)
             (let ((parent (treesit-node-parent node))
                   (is-top t))
               (while (and parent is-top)
                 (when (member (treesit-node-type parent) fold-types)
                   (setq is-top nil))
                 (setq parent (treesit-node-parent parent)))
               (when is-top
                 (push node result))))
           nil)))
      (nreverse result)))

  (defun my/fold--get-child-nodes (parent-nodes)
    "Return foldable nodes whose nearest foldable ancestor is in PARENT-NODES."
    (let ((fold-types (my/fold--fold-types))
          (parent-set (make-hash-table :test #'eq))
          result)
      (dolist (n parent-nodes)
        (puthash n t parent-set))
      (when fold-types
        (treesit-search-subtree
         (treesit-buffer-root-node)
         (lambda (node)
           (when (and (member (treesit-node-type node) fold-types)
                      (not (gethash node parent-set)))
             (let ((parent (treesit-node-parent node))
                   (nearest nil))
               (while (and parent (not nearest))
                 (when (gethash parent parent-set)
                   (setq nearest parent))
                 (when (member (treesit-node-type parent) fold-types)
                   (setq nearest 'other))
                 (setq parent (treesit-node-parent parent)))
               (when (eq nearest 'other)
                 (push node result))))
           nil)))
      (nreverse result)))

  (defun my/fold-cycle ()
    "Cycle: all open -> overview (top-level only) -> content (children visible) -> all open."
    (interactive)
    (cond
     ((or (not (bound-and-true-p treesitter-context-fold-mode))
          (not (treesit-available-p)))
      (user-error "Fold not available in this buffer"))
     ((= my/fold-cycle-state 0)
      (dolist (node (my/fold--get-top-level-nodes))
        (my/fold--fold-node node))
      (setq my/fold-cycle-state 1)
      (message "Overview"))
     ((= my/fold-cycle-state 1)
      (let ((top-nodes (my/fold--get-top-level-nodes)))
        (dolist (node top-nodes)
          (my/fold--unfold-node node))
        (dolist (node (my/fold--get-child-nodes top-nodes))
          (my/fold--fold-node node)))
      (setq my/fold-cycle-state 2)
      (message "Content"))
     (t
      (treesitter-context-fold--show-region (point-min) (point-max))
      (setq my/fold-cycle-state 0)
      (message "All open")))))

(provide 'init-rice-coding)
;;; init-rice-coding.el ends here
