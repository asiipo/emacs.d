;;; org-capture-config.el --- Capture inbox and templates  -*- lexical-binding: t; -*-

(require 'org)

(defvar org-directory (expand-file-name "~/org"))
(defvar my/org-journal-file (expand-file-name "journal.org" org-directory)
  "Fallback journal file path if journal.el isn't loaded yet.")
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
(unless (file-exists-p org-default-notes-file)
  (with-temp-file org-default-notes-file
    (insert "#+TITLE: Inbox\n")))

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline (expand-file-name "tasks.org" org-directory) "Inbox")
         "* TODO %?\nCREATED: %U\n:PROPERTIES:\n:EFFORT: 0:15\n:END:\n%i\n%a")
        ("n" "Note" entry (file org-default-notes-file)
         "* %? :note:\nCREATED: %U\n%i\n%a")
        ("l" "Link" entry (file org-default-notes-file)
         "* %:description\nCREATED: %U\n%a\n\n%i")
        ("j" "Journal (today)" entry (file+datetree my/org-journal-file)
         "* %<%H:%M> %?\n")))

(global-set-key (kbd "C-c c") #'org-capture)

;; Speed commands and link storing (Org guide recommendations)
(setq org-use-speed-commands t)
(global-set-key (kbd "C-c l") #'org-store-link)

(provide 'org-capture-config)

