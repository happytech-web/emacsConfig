;;; init-typst.el --- Typst + Tinymist support -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :straight t
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook (typst-ts-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (with-eval-after-load 'typst-ts-mode
    (defun my/typst-eglot-contact (_interactive)
      "Return the best available Typst language server command."
      (let ((candidates nil))
        (when (and (boundp 'typst-ts-lsp-download-path)
                   typst-ts-lsp-download-path
                   (file-exists-p typst-ts-lsp-download-path))
          (push typst-ts-lsp-download-path candidates))
        (when (executable-find "tinymist")
          (push "tinymist" candidates))
        (when (executable-find "typst-lsp")
          (push "typst-lsp" candidates))
        (setq candidates (nreverse candidates))
        (if candidates
            (eglot-alternatives candidates)
          (user-error "No Typst LSP found; run typst-ts-lsp-download-binary or install tinymist"))))

    (add-to-list 'eglot-server-programs
                 '((typst-ts-mode) . my/typst-eglot-contact))))

(provide 'init-typst)
;;; init-typst.el ends here
