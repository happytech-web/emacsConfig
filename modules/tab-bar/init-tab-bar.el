;; tab bar background
(set-face-attribute 'tab-bar nil
                    :foreground "#1fab89")

;; active tab
(set-face-attribute 'tab-bar-tab nil
                    :foreground "#d7fbe8")

;; inactive tab
(set-face-attribute 'tab-bar-tab-inactive nil
                    :foreground "#62d2a2")

(use-package tab-bar
  :hook (window-setup . tab-bar-mode)
  :init
  (setq tab-bar-emoji "💻 ")

  ;; 修改 tab-bar-format，去掉 menu-bar
  (setq tab-bar-format
        '(tab-bar-format-emoji tab-bar-format-tabs))

;; 自定义函数来插入 emoji
  (defun tab-bar-format-emoji ()
    (propertize tab-bar-emoji 'face '(:height 1.1)))
  :config
  (setq tab-bar-separator ""
        tab-bar-show 1  ;; hide bar if less than 1 tabs open
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t)
  ;; menu bar

  ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))

  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; 我把 meow 的 indicator 也放在 tab-bar 上
  ;(setq tab-bar-format '(meow-indicator  tab-bar-format-tabs))
  (tab-bar--update-tab-bar-lines)

  ;; WORKAROUND: update tab-bar for daemon
  ;; (when (daemonp)
  ;;   (add-hook 'after-make-frame-functions
  ;;             #'(lambda (&rest _) (force-mode-line-update))))
  )

(rune/leader-keys
  "t" '(:ignore t :which-key "tab-bar")
  "th" '(tab-bar-switch-to-prev-tab :which-key "prev-tab") 
  "tl" '(tab-bar-switch-to-next-tab :which-key "next-tab") 
  "ti" '(tab-bar-new-tab :which-key "insert-new-tab") 
  "ts" '(tab-bar-select-tab-by-name :which-key "select-tab")
  "td" '(tab-bar-close-tab :which-key "close-tab")
  )

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
