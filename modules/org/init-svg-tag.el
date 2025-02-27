(use-package svg-tag-mode
  :ensure t
  :hook org-mode
  :init
  (setq svg-lib-style-default
        '(:background "#c3bef0" :foreground "#6639a6" :padding 1 :margin 0
                      :stroke 2 :radius 5 :alignment 0.5 :width 20 :height 0.9
                      :scale 0.5 :ascent center :crop-left nil :crop-right nil
                      :collection "material" :font-family "Fira Code Retina"
                      :font-size 14 :font-weight regular))
  (setq svg-tag-action-at-point 'edit)
  (setq svg-lib-icon-collections
      '(("bootstrap" .
         "https://icons.getbootstrap.com/assets/icons/%s.svg")
        ("simple" .
         "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
        ("material" .
         "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
        ("octicons" .
         "https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
        ("boxicons" .
         "https://boxicons.com/static/img/svg/regular/bx-%s.svg")))
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

    ;; [90%]
  (defun svg-progress-percent (value)
    (save-match-data
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                         nil :margin 0 :stroke 2 :radius 3 :padding 2 :height 0.6 :width 11 )
                  (svg-lib-tag (concat value "%")
                               nil :stroke 0 :margin 0 :height 0.6)) :ascent 'center)))

 ;; [30/40]
  (defun svg-progress-count (value)
    (save-match-data
      (let* ((seq (split-string value "/"))           
             (count (if (stringp (car seq))
                        (float (string-to-number (car seq)))
                      0))
             (total (if (stringp (cadr seq))
                        (float (string-to-number (cadr seq)))
                      1000)))
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ count total) nil
                                          :margin 0 :stroke 2 :radius 3 :padding 2 :height 0.6 :width 11)
                    (svg-lib-tag value nil
                                 :stroke 0 :height 0.6 :margin 0)) :ascent 'center))))

  (setq svg-tag-tags
        `(
          ;; Org tags
          ;("\\(:[A-Za-z0-9]+:\\)" . ((lambda (tag) (svg-tag-make tag))))
                                        ;(":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))


          ;; red [+ans+]
          ("\\[\\+[A-Za-z0-9]+\\+\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-red :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))

          ;; yellow [*check*]
          ("\\[\\*[A-Za-z0-9]+\\*\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-lyellow :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))

          ;; green [-ok-]
          ("\\[-[A-Za-z0-9]+-\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'all-the-icons-lgreen :inverse t 
                                              :beg 2 :end -2 :margin 0 :height 0.6))))
          ;;[#A]
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority :inverse t 
                                              :beg 2 :end -1 :margin 0 :height 0.6))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag)
                       (svg-tag-make "TODO" :face 'org-todo :inverse t
                                     :height 0.6 :margin 0))))

          ("DONE" . ((lambda (tag)
                       (svg-tag-make "DONE" :face 'org-done :inverse t
                                     :height 0.6 :margin 0))))

          ;; #+begin_src CC
          ;("\\(#\\+begin_src \\).+" . ((lambda (tag)
          ;             (svg-tag-make (concat "➫" (substring tag 8)) :face 'all-the-icons-lcyan :inverse t
          ;                           :height 0.5 :margin 0 :crop-right t))))

          ;("#\\+begin_src \\(.+\\)" . ((lambda (tag)
          ;             (svg-tag-make tag :face 'all-the-icons-lcyan :inverse nil 
          ;                           :height 0.5 :margin 0 :crop-left t))))
          ;("#\\+end_src" . ((lambda (tag)
          ;             (svg-tag-make (concat "➬" (substring tag 2)) :face 'all-the-icons-lcyan :inverse t
          ;                           :height 0.5 :margin 0))))

          ;; command
          ;(" =[^ ][^,]*[^ ]= " .((lambda (tag)
          ;                        (svg-tag-make tag :face 'info-node :inverse t
          ;                                      :height 0.5 :margin 0 :beg 1 :end -1))))
          ;; Citation of the form [cite:@Knuth:1984] 
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :height 0.6
                                                          :ascent 'center
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                     (svg-tag-make tag
                                                                   :end -1
                                                                   :height 0.6
                                                                   :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :inverse t :margin 0 :height 0.6))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :height 0.6))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :height 0.6))))

          ;; Inactive date  (with or without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :inverse t :margin 0 :face 'org-date :height 0.6))))
          (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :face 'org-date :height 0.6))))
          (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :face 'org-date :height 0.6))))

          ;; ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          ))

  (svg-tag-mode t))

(provide 'init-svg-tag)
;;; init-svg-tag.el ends here
