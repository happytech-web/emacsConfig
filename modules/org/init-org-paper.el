;;; init-org-paper.el --- Paper reading workflow on top of org-roam -*- lexical-binding: t; -*-

(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'org-roam)
(require 'json)
(require 'seq)

(defconst my/org-paper-root (expand-file-name "~/RoamNotes")
  "Root directory for org-roam knowledge files.")

(defconst my/org-paper-archive-dir (expand-file-name "Archive/" my/org-paper-root))
(defconst my/org-paper-daily-dir (expand-file-name "daily/" my/org-paper-root))

(defun my/org-paper--managed-file-p (&optional file)
  "Return non-nil when FILE belongs to the roam workspace."
  (let ((target (or file buffer-file-name)))
    (and target
         (string-prefix-p (file-truename my/org-paper-root)
                          (file-truename target)))))

(defun my/org-paper--ensure-file (path title &optional extra)
  "Create PATH with TITLE and EXTRA content when it does not exist."
  (unless (file-exists-p path)
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert "#+title: " title "\n")
      (when extra
        (insert extra)))))

(defun my/org-paper-refresh-agenda-files ()
  "Refresh `org-agenda-files' from roam and archive directories."
  (interactive)
  (setq org-agenda-files
        (seq-filter
         (lambda (file)
           (and (string-match-p "\\.org\\'" file)
                (not (string-match-p "/Templates/" file))
                (not (string-match-p "/data/" file))
                (not (string-match-p "/\\.git/" file))))
         (directory-files-recursively my/org-paper-root "\\.org\\'"))))

(defun my/org-paper-refresh-agenda-files-maybe ()
  "Refresh agenda when saving a managed org file."
  (when (and (derived-mode-p 'org-mode)
             (my/org-paper--managed-file-p))
    (my/org-paper-refresh-agenda-files)))

(defun my/org-paper--open-file (path title extra)
  "Ensure PATH exists, then open it."
  (my/org-paper--ensure-file path title extra)
  (find-file path))

(defun my/org-paper--ensure-title (title)
  "Ensure current buffer has a #+title line matching TITLE."
  (goto-char (point-min))
  (if (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
      (replace-match title t t nil 1)
    (insert "#+title: " title "\n")))

(defun my/org-paper--upsert-roam-file (path title content)
  "Write CONTENT to PATH inside Emacs and refresh org-roam metadata."
  (make-directory (file-name-directory path) t)
  (let ((buffer (find-file-noselect path)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert content)
      (org-mode)
      (goto-char (point-min))
      (my/org-paper--ensure-title title)
      (goto-char (point-min))
      (org-id-get-create)
      (save-buffer))
    (when (fboundp 'org-roam-db-update-file)
      (org-roam-db-update-file path))
    (kill-buffer buffer)))

(defun my/org-paper-batch-apply-payload (payload-path)
  "Apply JSON payload from PAYLOAD-PATH and sync org-roam."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (payload (json-read-file payload-path))
         (files (plist-get payload :files)))
    (dolist (item files)
      (let ((path (plist-get item :path))
            (title (plist-get item :title))
            (content (plist-get item :content)))
        (my/org-paper--upsert-roam-file path title content)
        (princ (format "Wrote roam file: %s\n" path))))
    (my/org-paper-refresh-agenda-files)
    (when (fboundp 'org-roam-db-sync)
      (org-roam-db-sync))
    (princ "Org-roam sync complete\n")))

(defun my/org-paper-open-dashboard ()
  "Open the top-level research dashboard."
  (interactive)
  (my/org-paper--open-file
   (expand-file-name "research-dashboard.org" my/org-paper-root)
   "Research Dashboard"
   "\n* Areas\n\n* Active Projects\n\n* Urgent Follow\n\n* Review Queue\n"))

(defun my/org-paper-open-index ()
  "Open the paper index page for review and quick access."
  (interactive)
  (my/org-paper--open-file
   (expand-file-name "paper-index.org" my/org-paper-root)
   "Paper Index"
   "\n* Canonical Papers\n\n* Recent Papers\n\n* Transferable Methods\n\n* Reading Notes\n"))

(defun my/org-paper-open-agenda ()
  "Refresh agenda files and open paper-related agenda."
  (interactive)
  (my/org-paper-refresh-agenda-files)
  (org-agenda nil "P"))

(defun my/org-paper-capture-paper ()
  "Capture a new paper note into org-roam."
  (interactive)
  (org-roam-capture nil "pp"))

(defun my/org-paper-capture-area ()
  "Capture a new research area note into org-roam."
  (interactive)
  (org-roam-capture nil "pr"))

(defun my/org-paper-capture-project ()
  "Capture a new project note into org-roam."
  (interactive)
  (org-roam-capture nil "pj"))

(defun my/org-paper-capture-method ()
  "Capture a new transferable method note into org-roam."
  (interactive)
  (org-roam-capture nil "pm"))

(defun my/org-paper-capture-today-paper ()
  "Capture today's paper recommendation entry."
  (interactive)
  (org-roam-dailies-capture-today nil "p"))

(use-package org
  :straight nil
  :ensure nil
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "READ(r)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-tag-alist
   '(("paper" . ?p)
     ("reading" . ?r)
     ("review" . ?v)
     ("urgent" . ?u)
     ("classic" . ?c)
     ("project" . ?j)
     ("area" . ?a)
     ("method" . ?m)))
  :config
  (my/org-paper-refresh-agenda-files)
  (add-hook 'after-save-hook #'my/org-paper-refresh-agenda-files-maybe)
  (setq org-agenda-custom-commands
        (append
         org-agenda-custom-commands
         '(("P" "Paper Workflow"
            ((tags-todo "+paper+urgent"
                        ((org-agenda-overriding-header "Urgent Paper Follow-up")))
             (tags-todo "+paper+reading"
                        ((org-agenda-overriding-header "Reading Queue")))
             (tags "+paper"
                   ((org-agenda-overriding-header "All Paper Notes")))))))))

(provide 'init-org-paper)
;;; init-org-paper.el ends here
