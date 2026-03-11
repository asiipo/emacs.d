;;; org-capture-config.el --- Capture inbox and templates -*- lexical-binding: t; -*-
;;; Commentary:
;; PARA-method capture templates for tasks and content organization.

(require 'org)

;; Inbox Configuration
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(unless (file-exists-p org-default-notes-file)
  (with-temp-file org-default-notes-file
    (insert "#+TITLE: Inbox\n")))

;; Someday/Maybe Configuration
(defvar my/someday-file
  (expand-file-name "someday.org" org-directory)
  "File for someday/maybe tasks and ideas.")

(unless (file-exists-p my/someday-file)
  (with-temp-file my/someday-file
    (insert "#+TITLE: Someday/Maybe\n\n"
            "* Ideas\n"
            "Tasks and ideas without a specific timeline.\n"
            "Review these during weekly planning.\n\n"
            "* To Explore\n"
            "Things to learn or investigate when time permits.\n\n"
            "* Future Projects\n"
            "Project ideas for later.\n\n")))

;; Capture Templates
(setq org-capture-templates
      `(("t" "Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nCREATED: %U\n%i\n")
        
        ("n" "Note" entry 
         (file ,org-default-notes-file)
         "* %^{Note title} :%^{tags|work|home|research|admin|deep|quick|:}:\nCREATED: %U\n\n%?\n%i\n")

        ("s" "Scheduled Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nSCHEDULED: %^T\nCREATED: %U\n%i\n")

        ("d" "Task with Deadline" entry
         (file ,org-default-notes-file)
         "* TODO %?\nDEADLINE: %^T\nCREATED: %U\n%i\n")

        ("S" "Someday/Maybe" entry
         (file+headline my/someday-file "Ideas")
         "* %? :someday:\nCREATED: %U\n%i\n")))

(setq org-use-speed-commands t)

(provide 'org-capture-config)

