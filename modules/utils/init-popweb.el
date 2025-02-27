;; (defvar path-to-popweb "~/.emacs.d/pluginTools/popweb")
;; (use-package org-transclusion
;;   :ensure t)
;; (use-package popweb
;;   :load-path path-to-popweb
;;   :config
;;   (setq popweb-url-web-window-width-scale 1.5)
;;   (setq popweb-url-web-window-height-scale 1.5)
;;   ;; Org-Roam ID link and footnote link previewer
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/org-roam"))
;;   (require 'popweb-org-roam-link)

;;   ;; LaTeX preview functionality
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/latex"))
;;   (require 'popweb-latex)
;;   (add-hook 'latex-mode-hook #'popweb-latex-mode)
;;   (add-hook 'org-mode-hook #'popweb-latex-mode)

;;   ;; Chinese-English translation popup
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/dict")) ;
;;   (require 'popweb-dict)

;;   ;; Anki note review popup
;;   (add-to-list 'load-path (concat path-to-popweb "/extension/anki-review"))
;;   (require 'popweb-anki-review)
;;   )
