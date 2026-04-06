(use-package org-roam
  :straight t
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-dailies-directory "daily/")
  (org-roam-completion-everywhere t)

  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("n" "notes")

     ("no" "notes overview" plain
      (file "~/RoamNotes/Templates/noteOverview.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("nc" "course notes" plain
      (file "~/RoamNotes/Templates/courseNote.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("nh" "chapter notes" plain
      (file "~/RoamNotes/Templates/chapterNote.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("nk" "little knowledge" plain
      (file "~/RoamNotes/Templates/knowledgeNote.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("p" "paper workflow")

     ("pp" "paper note" plain
      "* Metadata\n:PROPERTIES:\n:TYPE: paper\n:SOURCE_URL: %^{URL}\n:ARXIV_ID: %^{arXiv ID}\n:DOI: %^{DOI}\n:PDF_PATH: %^{PDF Path}\n:BIB_KEY: %^{Bib Key}\n:TIME_RANGE: %^{Time Range}\n:END:\n\n* Summary\n%?\n\n* Why It Matters\n\n* Key Figures\n\n* Action Items\n** TODO Read full paper :paper:reading:\n** TODO Extract reusable ideas :paper:review:\n\n* Links\n- Related area:\n- Related project:\n- Transferable method:\n"
      :if-new (file+head "papers/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :paper:\n")
      :unnarrowed t)

     ("pr" "research area" plain
      "* Overview\n:PROPERTIES:\n:TYPE: research-area\n:END:\n\n- Keywords: %^{Keywords}\n- Core question: %^{Core Question}\n- Adjacent areas: %^{Adjacent Areas}\n\n* Canonical Papers\n\n* Recent Threads\n\n* Related Projects\n\n* Open Questions\n%?"
      :if-new (file+head "areas/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :area:\n")
      :unnarrowed t)

     ("pj" "project note" plain
      "* Project Overview\n:PROPERTIES:\n:TYPE: project\n:END:\n\n- Goal: %^{Goal}\n- Current bottleneck: %^{Bottleneck}\n- Time horizon: %^{Time Horizon}\n\n* Active Papers\n\n* Urgent Follow\n\n* Candidate Ideas\n\n* Notes\n%?"
      :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:\n")
      :unnarrowed t)

     ("pm" "transferable method" plain
      "* Method Overview\n:PROPERTIES:\n:TYPE: transferable-method\n:SOURCE_DOMAIN: %^{Source Domain}\n:TARGET_DOMAIN: %^{Target Domain}\n:END:\n\n- Core mechanism: %^{Core Mechanism}\n- Why transferable: %^{Why transferable}\n- Risks: %^{Risks}\n\n* Related Papers\n\n* Application Notes\n%?"
      :if-new (file+head "methods/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :method:\n")
      :unnarrowed t)

     ("w" "work todo entries")

     ("we" "no time" entry
      "** %^{Type|HW|READ|TODO|PROJ} ${title} %?" :prepend t :empty-lines-before 0
      :target (file "~/RoamNotes/Archive/tmp.org")
      :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

     ("ws" "scheduled" entry
      "** %^{Type|HW|READ|TODO|PROJ} ${title}\nSCHEDULED: %^t%?" :prepend t :empty-lines-before 0
      :target (file "~/RoamNotes/Archive/tmp.org")
      :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))

     ("wd" "deadline" entry
      "** %^{Type|HW|READ|TODO|PROJ} ${title}\nDEADLINE: %^t%?" :prepend t :empty-lines-before 0
      :target (file "~/RoamNotes/Archive/tmp.org")
      :refile-targets (("~/RoamNotes/Archive/work.org" :maxlevel . 2)))))

  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("p" "paper recommendation" entry
      "* TODO %^{Paper Title} :paper:reading:\nSCHEDULED: %^t\n:PROPERTIES:\n:CAPTURED: %U\n:TIME_RANGE: %^{Time Range}\n:SOURCE: %^{Source|skill|rss|manual}\n:END:\n- Why now: %?\n- Related paper node:\n- Related project:\n- Related area:\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("w" "work todo entries")

     ("we" "no time" entry
      "* %^{Type|HW|READ|TODO|PROJ} %^{todo} %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("ws" "scheduled" entry
      "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nSCHEDULED: %^t%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("wd" "deadline" entry
      "* %^{Type|HW|READ|TODO|PROJ} %^{todo}\nDEADLINE: %^t%?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))


  :bind (:map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (require 'org-roam-dailies)
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(defvar my/org-roam-prefix-map (make-sparse-keymap)
  "Prefix map for org-roam commands.")

(defvar my/org-roam-paper-prefix-map (make-sparse-keymap)
  "Prefix map for paper workflow commands.")

(defvar my/org-roam-dailies-prefix-map (make-sparse-keymap)
  "Prefix map for org-roam dailies commands.")

(define-key global-map (kbd "C-c o") my/org-roam-prefix-map)

(define-key my/org-roam-prefix-map (kbd "l") #'org-roam-buffer-toggle)
(define-key my/org-roam-prefix-map (kbd "f") #'org-roam-node-find)
(define-key my/org-roam-prefix-map (kbd "i") #'org-roam-node-insert)
(define-key my/org-roam-prefix-map (kbd "c") #'org-id-get-create)
(define-key my/org-roam-prefix-map (kbd "a") #'org-roam-alias-add)
(define-key my/org-roam-prefix-map (kbd "p") my/org-roam-paper-prefix-map)
(define-key my/org-roam-prefix-map (kbd "d") my/org-roam-dailies-prefix-map)

(define-key my/org-roam-paper-prefix-map (kbd "p") #'my/org-paper-capture-paper)
(define-key my/org-roam-paper-prefix-map (kbd "r") #'my/org-paper-capture-area)
(define-key my/org-roam-paper-prefix-map (kbd "j") #'my/org-paper-capture-project)
(define-key my/org-roam-paper-prefix-map (kbd "m") #'my/org-paper-capture-method)
(define-key my/org-roam-paper-prefix-map (kbd "d") #'my/org-paper-open-dashboard)
(define-key my/org-roam-paper-prefix-map (kbd "i") #'my/org-paper-open-index)
(define-key my/org-roam-paper-prefix-map (kbd "a") #'my/org-paper-open-agenda)
(define-key my/org-roam-paper-prefix-map (kbd "t") #'my/org-paper-capture-today-paper)

(define-key my/org-roam-dailies-prefix-map (kbd ".") #'org-roam-dailies-find-directory)
(define-key my/org-roam-dailies-prefix-map (kbd "h") #'org-roam-dailies-goto-previous-note)
(define-key my/org-roam-dailies-prefix-map (kbd "l") #'org-roam-dailies-goto-next-note)
(define-key my/org-roam-dailies-prefix-map (kbd "D") #'org-roam-dailies-goto-today)
(define-key my/org-roam-dailies-prefix-map (kbd "Y") #'org-roam-dailies-goto-yesterday)
(define-key my/org-roam-dailies-prefix-map (kbd "T") #'org-roam-dailies-goto-tomorrow)
(define-key my/org-roam-dailies-prefix-map (kbd "S") #'org-roam-dailies-goto-date)
(define-key my/org-roam-dailies-prefix-map (kbd "d") #'org-roam-dailies-capture-today)
(define-key my/org-roam-dailies-prefix-map (kbd "y") #'org-roam-dailies-capture-yesterday)
(define-key my/org-roam-dailies-prefix-map (kbd "t") #'org-roam-dailies-capture-tomorrow)
(define-key my/org-roam-dailies-prefix-map (kbd "s") #'org-roam-dailies-capture-date)

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
