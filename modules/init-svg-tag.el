;;; init-svg-tag.el --- Basic svg-tag-mode setup -*- lexical-binding: t; -*-

(use-package svg-tag-mode
  :straight t
  :if (display-graphic-p)
  :hook ((org-mode . svg-tag-mode)
         (markdown-mode . svg-tag-mode))
  :init
  ;; Keep it minimal first for pgtk verification.
  (setq svg-tag-tags
        '(("\\<TODO\\>" . ((lambda (_) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("\\<DONE\\>" . ((lambda (_) (svg-tag-make "DONE" :face 'org-done :inverse t :margin 0))))
          ("\\<FIXME\\>" . ((lambda (_) (svg-tag-make "FIXME" :face 'error :inverse t :margin 0))))
          ("\\<NOTE\\>" . ((lambda (_) (svg-tag-make "NOTE" :face 'warning :inverse t :margin 0))))
          ("\\[#[A-Z]\\]" . ((lambda (tag)
                               (svg-tag-make tag :face 'org-priority :inverse t
                                             :beg 2 :end -1 :margin 0))))))
  :config
  (when (not (image-type-available-p 'svg))
    (message "svg-tag-mode: SVG image type unavailable in this Emacs build")))

(provide 'init-svg-tag)
;;; init-svg-tag.el ends here
