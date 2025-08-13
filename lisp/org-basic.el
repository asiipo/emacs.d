;;; ----------------------------------------------------------------------
;;; Org basics for TODOs, agenda, and quick capture
;;; ----------------------------------------------------------------------

;; Uses `org-directory` defined centrally in init.el

(let ((tasks (expand-file-name "tasks.org" org-directory)))
  (unless (file-exists-p tasks)
    (with-temp-file tasks
      (insert "#+TITLE: Tasks\n\n* Inbox\n"))))

;; The agenda scans these files for TODOs, schedules, deadlines, etc.
(let ((tasks (expand-file-name "tasks.org" org-directory)))
  (setq org-agenda-files (delete-dups (cons tasks org-agenda-files))))

;; Editing niceties inside Org buffers
(add-hook 'org-mode-hook #'visual-line-mode)  ; soft-wrap long lines at word boundary
(add-hook 'org-mode-hook #'org-indent-mode)   ; visual indentation mirroring outline

;; TODO workflow, priorities, and logging
;; - States: TODO -> NEXT -> DONE (or CANCELLED)
;; - Enforce dependencies; log into drawers to keep headlines clean
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)"))
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-done 'time
      org-log-into-drawer t
      org-priority-default ?C
      org-priority-highest ?A
      org-priority-lowest ?C)

;; Handy global keys
(global-set-key (kbd "C-c a") #'org-agenda)   ; open agenda
(global-set-key (kbd "C-c l") #'org-store-link) ; store link

;; Archiving
(setq org-archive-location "archive/%s::datetree/")

;; Appearance
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-startup-folded 'content
      org-image-actual-width 600)

;; Babel defaults
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

;; Stable links
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; Refiling: global defaults; journal specifics live in journal.el
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)
(add-to-list 'org-refile-targets '(org-agenda-files :maxlevel . 9))

(provide 'org-basic)
