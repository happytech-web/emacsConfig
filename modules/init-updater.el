(defun ll/straight-pull-all ()
  "Pull all straight.el packages."
  (interactive)
  (straight-pull-all)
  (message "straight pull finished"))

(defun ll/straight-pull-and-rebuild-all ()
  "Pull and rebuild all straight.el packages."
  (interactive)
  (straight-pull-all)
  (straight-rebuild-all)
  (message "straight pull + rebuild finished"))

(defun ll/straight-freeze-versions ()
  "Write current lockfile for straight.el packages."
  (interactive)
  (straight-freeze-versions)
  (message "straight versions frozen"))

(defun ll/straight-thaw-versions ()
  "Restore package revisions from straight.el lockfile."
  (interactive)
  (straight-thaw-versions)
  (message "straight versions thawed"))

(with-eval-after-load 'general
  (rune/leader-keys
    "pu" '(:ignore t :which-key "packages")
    "pup" '(ll/straight-pull-all :which-key "straight pull all")
    "puP" '(ll/straight-pull-and-rebuild-all :which-key "pull + rebuild")
    "puf" '(ll/straight-freeze-versions :which-key "freeze versions")
    "put" '(ll/straight-thaw-versions :which-key "thaw versions")))

(provide 'init-updater)
;;; init-updater.el ends here
