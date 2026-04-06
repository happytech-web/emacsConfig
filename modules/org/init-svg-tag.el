;;; init-svg-tag.el --- svg-tag-mode setup -*- lexical-binding: t; -*-

(use-package svg-tag-mode
  :straight t
  :if (display-graphic-p)
  :custom
  ;; When point enters a tag, show plain text for direct editing.
  ;; When point leaves, svg rendering is restored.
  (svg-tag-action-at-point 'edit)
  :hook ((org-mode . svg-tag-mode)
         (markdown-mode . svg-tag-mode))
  :config
  (let* ((date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
         (time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
         (day-re "[A-Za-z]\\{3\\}")
         (day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re)))
    (defun my/svg-progress-percent (value)
      (save-match-data
        (svg-image (svg-lib-concat
                    (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                          nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                    (svg-lib-tag (concat value "%") nil :stroke 0 :margin 0))
                   :ascent 'center)))

    (defun my/svg-progress-count (value)
      (save-match-data
        (let* ((seq (split-string value "/"))
               (count (if (stringp (car seq)) (float (string-to-number (car seq))) 0))
               (total (if (stringp (cadr seq)) (float (string-to-number (cadr seq))) 1000)))
          (svg-image (svg-lib-concat
                      (svg-lib-progress-bar (/ count total) nil
                                            :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                      (svg-lib-tag value nil :stroke 0 :margin 0))
                     :ascent 'center))))

    (setq svg-tag-tags
          `(("TODO" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("NEXT" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("ACTIVE" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :inverse t :margin 0))))
            ("DONE" . ((lambda (tag) (svg-tag-make tag :face 'org-done :inverse t :margin 0))))
            ("\\[#[A-Z]\\]" . ((lambda (tag)
                                 (svg-tag-make tag :face 'org-priority :beg 2 :end -1 :margin 0))))
            ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                              (svg-tag-make tag :inverse t :beg 7 :end -1 :crop-right t))))
            ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                       (svg-tag-make tag :end -1 :crop-left t))))
            (,(format "\\(<%s>\\)" date-re) .
             ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0))))
            (,(format "\\(<%s \\)%s>" date-re day-time-re) .
             ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
            (,(format "<%s \\(%s>\\)" date-re day-time-re) .
             ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
            (,(format "\\(\\[%s\\]\\)" date-re) .
             ((lambda (tag) (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
            (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
             ((lambda (tag) (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
            (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
             ((lambda (tag) (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))
            ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                                (my/svg-progress-percent (substring tag 1 -2)))))
            ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                              (my/svg-progress-count (substring tag 1 -1))))))))
  (unless (image-type-available-p 'svg)
    (message "svg-tag-mode: SVG image type unavailable in this Emacs build")))

(defun my/svg-tag-toggle-action-at-point ()
  "Toggle `svg-tag-action-at-point' between edit and echo."
  (interactive)
  (setq svg-tag-action-at-point
        (if (eq svg-tag-action-at-point 'edit) 'echo 'edit))
  (message "svg-tag-action-at-point: %s" svg-tag-action-at-point))

(provide 'init-svg-tag)
;;; init-svg-tag.el ends here
