;;; init-consult-treesit-imenu.el --- Treesit-enhanced consult-imenu -*- lexical-binding: t; -*-

(defconst my/treesit-consult-imenu-modes
  '(python-ts-mode c-ts-mode c++-ts-mode rust-ts-mode bash-ts-mode
    yaml-ts-mode markdown-ts-mode)
  "Tree-sitter modes where consult-imenu should use treesit index.")

(defvar my/treesit-imenu-prefer-custom-settings t
  "Prefer `my/treesit-imenu-settings-alist' over mode defaults.")

(defun my/python-ts-in-class-p (node)
  "Return non-nil when NODE is inside a Python class definition."
  (and (fboundp 'treesit-parent-until)
       (treesit-parent-until
        node
        (lambda (n) (string= (treesit-node-type n) "class_definition"))
        t)))

(defun my/python-ts-top-level-function-p (node)
  "Return non-nil for top-level Python function NODE."
  (not (my/python-ts-in-class-p node)))

(defun my/python-ts-method-p (node)
  "Return non-nil for Python method NODE."
  (my/python-ts-in-class-p node))

(defun my/treesit-node-field-text (node field)
  "Return text of NODE FIELD, or nil."
  (let ((child (and node (treesit-node-child-by-field-name node field))))
    (when child
      (treesit-node-text child t))))

(defun my/treesit-defun-name-safe (node)
  "Return a best-effort symbol name for NODE."
  (or (ignore-errors (treesit-defun-name node))
      (my/treesit-node-field-text node "name")
      "<?>"))

(defun my/treesit-method-name-with-owner (owner method)
  "Build METHOD display string with OWNER in a distinct face."
  (if (and owner (> (length owner) 0))
      (concat (propertize owner 'face 'font-lock-type-face)
              "::"
              method)
    method))

(defun my/python-ts-method-node-name (node)
  "Return display name for Python method NODE."
  (let* ((klass (treesit-parent-until
                 node
                 (lambda (n) (string= (treesit-node-type n) "class_definition"))
                 t))
         (owner (my/treesit-node-field-text klass "name"))
         (method (my/treesit-defun-name-safe node)))
    (my/treesit-method-name-with-owner owner method)))

(defun my/cpp-ts-in-class-p (node)
  "Return non-nil when NODE is inside C++ class/struct."
  (and (fboundp 'treesit-parent-until)
       (treesit-parent-until
        node
        (lambda (n)
          (member (treesit-node-type n) '("class_specifier" "struct_specifier")))
        t)))

(defun my/cpp-ts-top-level-function-p (node)
  "Return non-nil for top-level C++ function NODE."
  (not (my/cpp-ts-in-class-p node)))

(defun my/cpp-ts-method-p (node)
  "Return non-nil for C++ method NODE."
  (my/cpp-ts-in-class-p node))

(defun my/cpp-ts-method-node-name (node)
  "Return display name for C++ method NODE."
  (let* ((owner-node (treesit-parent-until
                      node
                      (lambda (n)
                        (member (treesit-node-type n)
                                '("class_specifier" "struct_specifier")))
                      t))
         (owner (my/treesit-node-field-text owner-node "name"))
         (method (my/treesit-defun-name-safe node)))
    (my/treesit-method-name-with-owner owner method)))

(defun my/rust-ts-in-impl-p (node)
  "Return non-nil when NODE is inside a Rust impl item."
  (and (fboundp 'treesit-parent-until)
       (treesit-parent-until
        node
        (lambda (n) (string= (treesit-node-type n) "impl_item"))
        t)))

(defun my/rust-ts-function-p (node)
  "Return non-nil for free Rust function NODE."
  (not (my/rust-ts-in-impl-p node)))

(defun my/rust-ts-method-p (node)
  "Return non-nil for Rust method NODE."
  (my/rust-ts-in-impl-p node))

(defun my/rust-ts-method-node-name (node)
  "Return display name for Rust method NODE."
  (let* ((impl-node (treesit-parent-until
                     node
                     (lambda (n) (string= (treesit-node-type n) "impl_item"))
                     t))
         (owner (or (my/treesit-node-field-text impl-node "type")
                    (my/treesit-node-field-text impl-node "trait")))
         (method (my/treesit-defun-name-safe node)))
    (my/treesit-method-name-with-owner owner method)))

(defun my/yaml-ts-key-node-name (node)
  "Return YAML key text for mapping NODE."
  (let ((key (treesit-node-child-by-field-name node "key")))
    (when key
      (replace-regexp-in-string
       "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" ""
       (treesit-node-text key t)))))

(defconst my/treesit-imenu-settings-alist
  '((python-ts-mode
     . (("Functions" "\\`function_definition\\'" my/python-ts-top-level-function-p nil)
        ("Methods" "\\`function_definition\\'" my/python-ts-method-p my/python-ts-method-node-name)
        ("Classes" "\\`class_definition\\'" nil nil)))
    (c-ts-mode
     . (("Functions" "\\`function_definition\\'" nil nil)
        ("Structs" "\\`struct_specifier\\'" nil nil)
        ("Unions" "\\`union_specifier\\'" nil nil)
        ("Enums" "\\`enum_specifier\\'" nil nil)
        ("Typedefs" "\\`type_definition\\'" nil nil)
        ("Macros" "\\`preproc_\\(def\\|function_def\\)\\'" nil nil)))
    (c++-ts-mode
     . (("Functions" "\\`function_definition\\'" my/cpp-ts-top-level-function-p nil)
        ("Methods" "\\`function_definition\\'" my/cpp-ts-method-p my/cpp-ts-method-node-name)
        ("Classes" "\\`class_specifier\\'" nil nil)
        ("Structs" "\\`struct_specifier\\'" nil nil)
        ("Enums" "\\`enum_specifier\\'" nil nil)
        ("Typedefs" "\\`type_definition\\'" nil nil)
        ("Namespaces" "\\`namespace_definition\\'" nil nil)
        ("Macros" "\\`preproc_\\(def\\|function_def\\)\\'" nil nil)))
    (rust-ts-mode
     . (("Functions" "\\`function_item\\'" my/rust-ts-function-p nil)
        ("Methods" "\\`function_item\\'" my/rust-ts-method-p my/rust-ts-method-node-name)
        ("Structs" "\\`struct_item\\'" nil nil)
        ("Enums" "\\`enum_item\\'" nil nil)
        ("Traits" "\\`trait_item\\'" nil nil)
        ("Impls" "\\`impl_item\\'" nil nil)
        ("Types" "\\`type_item\\'" nil nil)
        ("Constants" "\\`const_item\\'" nil nil)
        ("Statics" "\\`static_item\\'" nil nil)
        ("Macros" "\\`macro_definition\\'" nil nil)))
    (bash-ts-mode
     . (("Functions" "\\`function_definition\\'" nil nil)
        ("Variables" "\\`variable_assignment\\'" nil nil)))
    (yaml-ts-mode
     . (("Keys" "\\`block_mapping_pair\\'" nil my/yaml-ts-key-node-name)))
    (markdown-ts-mode
     . (("Headings" "\\`\\(atx\\|setext\\)_heading\\'" nil nil)
        ("Code Blocks" "\\`fenced_code_block\\'" nil nil))))
  "Tree-sitter imenu settings per major mode.")

(defun my/treesit-imenu-settings-for-current-mode ()
  "Return custom tree-sitter imenu settings for current major mode."
  (cdr (assq major-mode my/treesit-imenu-settings-alist)))

(defun my/consult-imenu-face-for-category (category)
  "Pick a face for imenu CATEGORY."
  (let ((name (downcase category)))
    (cond
     ((or (string-match-p "func" name) (string-match-p "method" name))
      'font-lock-function-name-face)
     ((or (string-match-p "type" name)
          (string-match-p "class" name)
          (string-match-p "struct" name)
          (string-match-p "enum" name)
          (string-match-p "interface" name)
          (string-match-p "trait" name))
      'font-lock-type-face)
     ((or (string-match-p "const" name) (string-match-p "macro" name))
      'font-lock-constant-face)
     ((or (string-match-p "var" name)
          (string-match-p "field" name)
          (string-match-p "property" name))
      'font-lock-variable-name-face)
     ((string-match-p "import" name) 'font-lock-keyword-face)
     (t 'font-lock-doc-face))))

(defun my/consult-imenu-key-for-category (category used)
  "Return a unique narrow key for CATEGORY, avoiding USED keys."
  (let* ((letters (string-to-list
                   (downcase
                    (replace-regexp-in-string "[^a-z]" "" category))))
         key)
    (while (and letters (not key))
      (let ((ch (pop letters)))
        (unless (memq ch used)
          (setq key ch))))
    (unless key
      (let ((ch ?a))
        (while (and (<= ch ?z) (not key))
          (unless (memq ch used)
            (setq key ch))
          (setq ch (1+ ch)))))
    (or key ?z)))

(defun my/consult-imenu-types-from-treesit-settings (settings)
  "Build `consult-imenu' :types config from tree-sitter SETTINGS."
  (let (types used)
    (dolist (spec settings (nreverse types))
      (let ((category (car-safe spec)))
        (when (and (stringp category) (> (length category) 0))
          (let ((key (my/consult-imenu-key-for-category category used)))
            (push key used)
            (push (list key category
                        (my/consult-imenu-face-for-category category))
                  types)))))))

(defun my/treesit-consult-imenu-setup ()
  "Configure `consult-imenu' to use tree-sitter index in current buffer."
  (when (and (fboundp 'treesit-simple-imenu)
             (boundp 'treesit-simple-imenu-settings)
             (apply #'derived-mode-p my/treesit-consult-imenu-modes))
    (let* ((custom-settings (my/treesit-imenu-settings-for-current-mode))
           (settings (if my/treesit-imenu-prefer-custom-settings
                         (or custom-settings treesit-simple-imenu-settings)
                       (or treesit-simple-imenu-settings custom-settings))))
      (when settings
        (setq-local treesit-simple-imenu-settings settings)
        (setq-local imenu-max-item-length 200)
        (setq-local imenu-create-index-function #'treesit-simple-imenu)
        (setq-local consult-imenu-config
                    (list
                     (list major-mode
                           :types (my/consult-imenu-types-from-treesit-settings
                                   settings))))))))

(dolist (hook '(python-ts-mode-hook
                c-ts-mode-hook
                c++-ts-mode-hook
                rust-ts-mode-hook
                bash-ts-mode-hook
                yaml-ts-mode-hook
                markdown-ts-mode-hook))
  (add-hook hook #'my/treesit-consult-imenu-setup))

(defun my/consult-imenu-treesit ()
  "Use tree-sitter enhanced `consult-imenu' when available."
  (interactive)
  (my/treesit-consult-imenu-setup)
  (consult-imenu))

(with-eval-after-load 'consult
  (keymap-global-set "C-c l s" #'my/consult-imenu-treesit))

(provide 'init-consult-treesit-imenu)
;;; init-consult-treesit-imenu.el ends here
