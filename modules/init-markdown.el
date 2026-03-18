;;; init-markdown.el --- Markdown support -*- lexical-binding: t; -*-

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"))

(provide 'init-markdown)
;;; init-markdown.el ends here
