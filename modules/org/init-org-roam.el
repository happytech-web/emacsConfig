(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)

  (org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)

   ;; notes menu
   ("n" "notes")

   ("no" "notes overview" plain
    (file "~/RoamNotes/Templates/noteOverview.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)

   ("nc" "course notes" plain
    (file "~/RoamNotes/Templates/courseNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ("nh" "chapter notes" plain
    (file "~/RoamNotes/Templates/chapterNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ("nk" "little knowledge" plain
    (file "~/RoamNotes/Templates/knowledgeNote.org")
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed T)

   ;; work menu
   ;; create stuff in tmp.org, and then refile it into work.org
   ;; notice that in this way will not create id
   ("w" "Work Todo Entries")


   ("we" "No Time" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title} %?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("ws" "Scheduled" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("wd" "Deadline" entry 
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ("ww" "Scheduled & deadline" entry
    "** %^{Type|HW|READ|TODO|PROJ} ${title}\nSCHEDULED: %^t DEADLINE: %^t %?" :prepend t :empty-lines-before 0
    :target (file "~/RoamNotes/Archive/tmp.org")
    :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

   ))

  (org-roam-dailies-capture-templates
  '(("d" "default" entry "* %<%I:%M %p>: %?"
     :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("w" "Work Todo Entries")

   ("we" "No Time" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo} %?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("ws" "Scheduled" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nSCHEDULED: %^t%?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("wd" "Deadline" entry 
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nDEADLINE: %^t%?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

   ("ww" "Scheduled & deadline" entry
    "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nSCHEDULED: %^t DEADLINE: %^t %?"
    :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
    ))


  :bind (:map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; integrate with general key, <spc> is the prefix
(rune/leader-keys
  "o"  '(:ignore t :which-key "org roam")
  "ol" '(org-roam-buffer-toggle :which-key "backlinks-buffer")
  "of" '(org-roam-node-find :which-key "find-node")
  "oi" '(org-roam-node-insert :which-key "insert-node")
  "oc" '(org-id-get-create :which-key "create-id")
  "oa" '(org-roam-alias-add :which-key "add-alias")
  "od" '(:ignore t :which-key "dailies")
  "od." '(org-roam-dailies-find-directory :which-key "daily dir")
  "odh" '(org-roam-dailies-goto-previous-note :which-key "goto-previous")
  "odl" '(org-roam-dailies-goto-next-note :which-key "goto-next")
  "odD" '(org-roam-dailies-goto-today :which-key "goto-today")
  "odY" '(org-roam-dailies-goto-yesterday :which-key "goto-yesterday")
  "odT" '(org-roam-dailies-goto-tomorrow :which-key "goto-tomorrow")
  "odS" '(org-roam-dailies-goto-date :which-key "goto-date")
  "odd" '(org-roam-dailies-capture-today :which-key "capture-today")
  "ody" '(org-roam-dailies-capture-yesterday :which-key "capture-yesterday")
  "odt" '(org-roam-dailies-capture-tomorrow :which-key "capture-tomorrow")
  "ods" '(org-roam-dailies-capture-date :which-key "capture-date")
  )

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
