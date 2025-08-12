;;; ----------------------------------------------------------------------
;;; Org basics for TODOs, agenda, and quick capture
;;; ----------------------------------------------------------------------

;; Where Org files live
(setq org-directory (expand-file-name "~/org"))


(let ((tasks (expand-file-name "tasks.org" org-directory)))
  (unless (file-exists-p tasks)
    (with-temp-file tasks
      (insert "#+TITLE: Tasks\n\n* Inbox\n"))))

;; The agenda scans these files for TODOs, schedules, deadlines, etc.
(setq org-agenda-files (list (expand-file-name "tasks.org" org-directory)))

;; Editing niceties inside Org buffers
(add-hook 'org-mode-hook #'visual-line-mode)  ; soft-wrap long lines at word boundary
(add-hook 'org-mode-hook #'org-indent-mode)   ; visual indentation mirroring outline

;; TODO workflow and logging:
;; - States: TODO -> NEXT -> DONE (or CANCELLED)
;; - When DONE, store a timestamp in a LOGBOOK drawer to avoid clutter.
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Handy global keys:
(global-set-key (kbd "C-c a") #'org-agenda)   ; open agenda
(global-set-key (kbd "C-c c") #'org-capture)  ; quick capture

;; Capture templates:
;; C-c c t  → prompt and file under Tasks ▸ Inbox as a TODO
(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "~/org/tasks.org" "Inbox")
         "* TODO %?\nCREATED: %U")))

(provide 'org-basic)
