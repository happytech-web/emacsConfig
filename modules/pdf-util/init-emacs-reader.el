(use-package reader
  :ensure nil
  :config
   (evil-define-key 'normal reader-mode-map
     "C-j" 'reader-next-page
     "C-k" 'reader-previous-page
     "j" 'reader-scroll-down
     "k" 'reader-scroll-up
     "h" 'reader-scroll-left
     "l" 'reader-scroll-right
     "H" 'reader-fit-to-height
     "W" 'reader-fit-to-width
     "=" 'reader-enlarge-size
     "-" 'reader-shrink-size
     )
  )

(with-eval-after-load 'reader
  (keymap-set reader-mode-map "C-j" #'reader-next-page)
  (keymap-set reader-mode-map "C-k" #'reader-previous-page)
  )

(provide 'init-emacs-reader)
