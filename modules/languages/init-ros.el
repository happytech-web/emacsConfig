;;; cannot figure out how this work
(use-package ros
  :ensure t
  :config
  (setq ros-workspaces
        (list
         (ros-dump-workspace :tramp-prefix nil :workspace "~/codes/rosWorkspace/tutorial_ws" :extends '("/opt/ros/humble/")))
        )
  )

(provide 'init-ros)
;;; init-ros.el ends here
