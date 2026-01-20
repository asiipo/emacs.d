;;; org-basic.el --- Org mode basic configuration -*- lexical-binding: t; -*-

;; Version: 1.0
;; Author: arttusii
;; Description: Fundamental Org mode settings for document structure and behavior
;;
;; Key Features:
;;   - Visual enhancements (org-indent, headlines styling)
;;   - Better list handling and structure editing
;;   - Improved source block editing experience
;;
;; Dependencies: Built-in Org mode packages only

(require 'subr-x)

;; ============================================================================
;; PARA DIRECTORY STRUCTURE
;; ============================================================================

;; Ensure main PARA directories exist
(dolist (d '("projects" "areas" "resources" "archive"))
  (make-directory (expand-file-name d org-directory) t))

;; Ensure archive subfolders for organized archiving
(dolist (d '("archive/projects" "archive/areas" "archive/resources"))
  (make-directory (expand-file-name d org-directory) t))

;; ============================================================================
;; AGENDA CONFIGURATION
;; ============================================================================

;; Dynamic agenda files function to always include new files
(defun my/org-agenda-files ()
  "Return list of org files for agenda scanning.
Restricted to inbox.org and gtd.org to keep buffers clean."
  (let* ((inbox (expand-file-name "inbox.org" org-directory))
         (gtd (expand-file-name "gtd.org" org-directory)))
    (delete-dups (delq nil (list (when (file-exists-p inbox) inbox)
                                 (when (file-exists-p gtd) gtd))))))

;; Set agenda files to use the dynamic function
(setq org-agenda-files-function #'my/org-agenda-files
      org-agenda-files (my/org-agenda-files))

;; Auto-refresh agenda after capture
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook 
            (lambda () 
              (when (get-buffer "*Org Agenda*")
                (with-current-buffer "*Org Agenda*"
                  (org-agenda-redo))))))

;; ============================================================================
;; ORG BUFFER BEHAVIOR
;; ============================================================================

;; Editing niceties inside Org buffers
(add-hook 'org-mode-hook #'visual-line-mode)  ;; Soft-wrap long lines at word boundary
(add-hook 'org-mode-hook #'org-indent-mode)   ;; Visual indentation mirroring outline

;; ============================================================================
;; TODO WORKFLOW AND LOGGING
;; ============================================================================

;; Multi-state workflow: TODO -> NEXT -> WAIT -> DONE/CANCELED
;; Based on Org Mode Guide section 5.2
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d)" "CANCELED(c)"))
      org-use-fast-todo-selection t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-log-done 'time
      org-log-redeadline 'time                  ;; Log when deadlines change
      org-log-reschedule 'time                  ;; Log when tasks are rescheduled
      org-log-into-drawer t
      org-log-state-notes-into-drawer t         ;; Store state change notes in drawer
      org-priority-default ?C
      org-priority-highest ?A
      org-priority-lowest ?C)

;; Enable habit tracking for recurring tasks
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)

;; Stuck project detection (Guide section 10.4)
(setq org-stuck-projects '("project" ("TODO" "NEXT") nil ""))

;; ============================================================================
;; TAGS AND PRIORITIES
;; ============================================================================

;; PARA-friendly tags for organizing work and life
;; Based on Org Mode Guide section 6
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

;; Fast tag selection
(setq org-fast-tag-selection-single-key t)

;; ============================================================================
;; INBOX FUNCTION
;; ============================================================================

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

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

;; ============================================================================
;; PARA-AWARE FILE ARCHIVING SYSTEM
;; ============================================================================

(defun my/org-archive-file ()
  "Move the current Org file into the appropriate archive subfolder, with confirmation."
  (interactive)
  (unless (derived-mode-p 'org-mode) 
    (user-error "Not in an Org buffer"))
  (let* ((src (or buffer-file-name (user-error "Buffer not visiting a file")))
         (file-name (file-name-nondirectory src))
         ;; Determine archive subdirectory based on source location
         (subdir (cond
                  ((string-match-p "/projects/" src) "projects")
                  ((string-match-p "/areas/" src) "areas")  
                  ((string-match-p "/resources/" src) "resources")
                  (t "")))
         (archive-dir (if (string-empty-p subdir)
                         (expand-file-name "archive" org-directory)
                       (expand-file-name (concat "archive/" subdir) org-directory)))
         (dest (expand-file-name file-name archive-dir)))
    ;; Add timestamp if file exists to avoid conflicts
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

;; Archive keybindings - simplified to one key
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


;; ============================================================================
;; ORG APPEARANCE AND BEHAVIOR
;; ============================================================================

;; Visual appearance settings
(setq org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-startup-folded 'content
      org-image-actual-width 600)

;; Enable easy templates (e.g., <s TAB for source blocks)
(require 'org-tempo)

;; Pretty bullets for org headings
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))
  ;; :config
  ;; (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

;; Configure Python interpreter for Org Babel (cross-platform)
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

;; Set Python interpreter if default detection fails
(unless (executable-find "python3")
  (let ((python-exec (my/find-python-executable)))
    (setq org-babel-python-command python-exec
          python-shell-interpreter python-exec)))

;; Configure Fortran compiler (using gfortran)
(setq org-babel-fortran-compiler "gfortran")

;; Source code evaluation (see Org Mode Guide section 14)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (fortran . t)))

;; Stable links for reliable references (see Org Mode Guide section 4)
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive)

;; ============================================================================
;; TIME TRACKING (ORG-CLOCK)
;; ============================================================================

;; Enable persistent clock - remember clocks across Emacs sessions
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Clock behavior
(setq org-clock-in-resume t                      ; Resume interrupted clocks
      org-clock-persist-query-resume nil         ; Don't ask about resuming
      org-clock-out-remove-zero-time-clocks t    ; Remove zero-time entries
      org-clock-report-include-clocking-task t   ; Show active clock in reports
      org-clock-history-length 10                ; Remember last 10 clocked tasks
      org-clock-mode-line-total 'current)        ; Display current task time in mode line

;; ============================================================================
;; REFILE CONFIGURATION
;; ============================================================================

;; Simple refile configuration using agenda files
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3)
                          (nil :maxlevel . 9)))

;; ============================================================================
;; EXPORT CONFIGURATION
;; ============================================================================

;; Load Beamer exporter (required for C-c C-e l P/B options)
(require 'ox-beamer)

;; To use Beamer export:
;; 1. Add to your org file: #+LATEX_CLASS: beamer
;; 2. Export with: C-c C-e l P (LaTeX → Beamer PDF)
;; 3. To use a custom .sty, add #+LATEX_HEADER: \usepackage{mybeamer} in your file

(provide 'org-basic)
