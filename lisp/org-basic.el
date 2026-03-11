;;; org-basic.el --- Org mode basic configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Fundamental Org mode settings: PARA structure, TODO workflow, agenda, and archiving.

(require 'subr-x)

;; Org Directory

(defvar org-directory (expand-file-name "~/org")
  "Base directory for all Org files and PARA structure.")

(require 'org)

;; PARA Directory Structure
(dolist (d '("projects" "areas" "resources" "archive"))
  (make-directory (expand-file-name d org-directory) t))
(dolist (d '("archive/projects" "archive/areas" "archive/resources"))
  (make-directory (expand-file-name d org-directory) t))

;; Agenda Configuration
(defun my/org-agenda-files ()
  "Return list of org files for agenda scanning.
Restricted to inbox.org and gtd.org to keep buffers clean."
  (let* ((inbox (expand-file-name "inbox.org" org-directory))
         (gtd (expand-file-name "gtd.org" org-directory)))
    (delete-dups (delq nil (list (when (file-exists-p inbox) inbox)
                                 (when (file-exists-p gtd) gtd))))))

(setq org-agenda-files-function #'my/org-agenda-files
      org-agenda-files (my/org-agenda-files))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook 
            (lambda () 
              (when (get-buffer "*Org Agenda*")
                (with-current-buffer "*Org Agenda*"
                  (org-agenda-redo))))))

;; Org Buffer Behavior
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'org-indent-mode)

;; TODO Workflow and Logging
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)"))
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-done 'time
      org-log-redeadline 'time
      org-log-reschedule 'time
      org-log-into-drawer t
      org-log-state-notes-into-drawer t
      org-priority-default ?C
      org-priority-highest ?A
      org-priority-lowest ?C)

(add-to-list 'org-modules 'org-habit)
(require 'org-habit)

(setq org-stuck-projects '("project" ("TODO" "NEXT") nil ""))

;; Tags and Priorities
(setq org-tag-alist
      '((:startgroup . nil)
        (:endgroup . nil)
        ("work" . ?w) ("home" . ?h) ("research" . ?r) ("admin" . ?a)
        ("deep" . ?d) ("quick" . ?q)
        (:newline)
        (:startgroup . nil)
        ("project" . ?p) ("area" . ?A) ("resource" . ?R)
        (:endgroup . nil)
        (:newline)
        ("meeting" . ?m) ("phone" . ?P) ("email" . ?e)))

(setq org-fast-tag-selection-single-key t)

;; Inbox Functions

(defun my/goto-inbox ()
  "Open the inbox.org file for quick access."
  (interactive)
  (find-file (expand-file-name "inbox.org" org-directory)))

(defun my/goto-someday ()
  "Open the someday.org file for reviewing future ideas."
  (interactive)
  (find-file (expand-file-name "someday.org" org-directory)))

(defun my/refresh-agenda ()
  "Refresh the agenda view to include any new files."
  (interactive)
  (setq org-agenda-files (my/org-agenda-files))
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "Agenda refreshed!"))))

;; PARA-Aware File Archiving System

(defun my/org-archive-file ()
  "Move the current Org file into the appropriate archive subfolder, with confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode) 
    (user-error "Not in an Org buffer"))
  (let* ((src (or buffer-file-name (user-error "Buffer not visiting a file")))
         (file-name (file-name-nondirectory src))
         (subdir (cond
                  ((string-match-p "/projects/" src) "projects")
                  ((string-match-p "/areas/" src) "areas")  
                  ((string-match-p "/resources/" src) "resources")
                  (t "")))
         (archive-dir (if (string-empty-p subdir)
                         (expand-file-name "archive" org-directory)
                       (expand-file-name (concat "archive/" subdir) org-directory)))
         (dest (expand-file-name file-name archive-dir)))
    (when (file-exists-p dest)
      (setq dest (expand-file-name
                  (format "%s-%s%s"
                          (file-name-sans-extension file-name)
                          (format-time-string "%Y%m%d-%H%M%S")
                          (file-name-extension file-name t))
                  archive-dir)))
    (when (y-or-n-p (format "Archive this file to %s? " dest))
      (make-directory archive-dir t)
      (save-buffer)
      (kill-buffer (current-buffer))
      (rename-file src dest t)
      (find-file dest)
      (message "Archived to: %s" dest))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x a") #'my/org-archive-file))
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c C-x a") #'my/org-agenda-archive-file))

(defun my/org-agenda-archive-file ()
  "Archive the file associated with the agenda item at point."
  (interactive)
  (require 'org-agenda)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (my/org-archive-file))))
  (org-agenda-redo))

;; Org Appearance and Behavior
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-startup-folded 'content
      org-image-actual-width 600)

(require 'org-tempo)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Configure Python for Org Babel
(defun my/find-python-executable ()
  "Find the best Python executable across different platforms."
  (or 
   ;; Try standard Python 3 first
   (executable-find "python3")
   (executable-find "python")
   ;; macOS Anaconda
   (and (eq system-type 'darwin)
        (file-executable-p "/opt/anaconda3/bin/python3")
        "/opt/anaconda3/bin/python3")
   ;; System defaults
   (cond
    ((eq system-type 'windows-nt) "python.exe")
    (t "python3"))))


(unless (executable-find "python3")
  (let ((python-exec (my/find-python-executable)))
    (setq org-babel-python-command python-exec
          python-shell-interpreter python-exec)))

(setq org-babel-fortran-compiler "gfortran")

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (fortran . t)))

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive)

;; File Associations
(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . emacs)))

;; Time Tracking (Org-Clock)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-clock-in-resume t
      org-clock-persist-query-resume nil
      org-clock-out-remove-zero-time-clocks t
      org-clock-report-include-clocking-task t
      org-clock-history-length 10
      org-clock-mode-line-total 'current)

;; Refile Configuration
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3)
                          (nil :maxlevel . 9)))

;; Export Configuration
(require 'ox-beamer)

(provide 'org-basic)
