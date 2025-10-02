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
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/slugify (s)
  "Convert string S to a filesystem-friendly slug.
Removes special characters, converts spaces to hyphens, and ensures
the result is safe for use in filenames."
  (let* ((down (downcase (or s "")))
         (trim (string-trim down))
         (clean (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" trim))
         (spaces (replace-regexp-in-string "[[:space:]]+" "-" clean)))
    (replace-regexp-in-string "/" "-" spaces)))

;; ============================================================================
;; AGENDA CONFIGURATION
;; ============================================================================

;; Dynamic agenda files function to always include new files
(defun my/org-agenda-files ()
  "Return list of org files for agenda scanning.
Focus on actionable items: inbox + Projects + Areas (but exclude Resources for daily agenda)."
  (let* ((inbox (expand-file-name "inbox.org" org-directory))
         (projects (when (file-directory-p (expand-file-name "projects" org-directory))
                     (directory-files-recursively (expand-file-name "projects" org-directory) "\\.org$")))
         (areas (when (file-directory-p (expand-file-name "areas" org-directory))
                  (directory-files-recursively (expand-file-name "areas" org-directory) "\\.org$"))))
    ;; Exclude Resources from daily agenda - they're reference materials, not actionable
    (delete-dups (append (when (file-exists-p inbox) (list inbox)) 
                         projects areas))))

;; Set agenda files to use the dynamic function AND initialize the variable
(setq org-agenda-files-function #'my/org-agenda-files
      org-agenda-files (my/org-agenda-files))  ;; Initialize with current files

;; Auto-refresh agenda after capture (simplified - function approach auto-updates)
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook 
            (lambda () 
              ;; Function-based approach automatically includes new files
              ;; Just refresh agenda view if open
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

(defun my/refresh-agenda ()
  "Refresh the agenda view to include any new files.
Updates both the function and variable approaches for maximum compatibility."
  (interactive)
  ;; Update the variable to ensure compatibility with all agenda commands
  (setq org-agenda-files (my/org-agenda-files))
  ;; Refresh agenda view if open
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "Agenda refreshed!"))))

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

;; ============================================================================
;; SIMPLIFIED FILE ARCHIVING SYSTEM
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
      ;; Create archive directory if needed
      (make-directory archive-dir t)
      ;; Save and move file
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

;; Configure Python interpreter for Org Babel (cross-platform)
;; Note: Org Babel's "auto" detection usually works well, but we provide
;; explicit configuration for edge cases and better cross-platform support

(defun my/find-python-executable ()
  "Find the best Python executable across different platforms."
  (cond
   ;; macOS - Anaconda installation
   ((and (eq system-type 'darwin)
         (file-executable-p "/opt/anaconda3/bin/python3"))
    "/opt/anaconda3/bin/python3")
   
   ;; WSL/Linux - Check common locations  
   ((memq system-type '(gnu/linux berkeley-unix))
    (or (executable-find "python3")
        (executable-find "python")
        "/usr/bin/python3"))
   
   ;; Windows - Check Anaconda and system Python
   ((eq system-type 'windows-nt)
    (or (executable-find "python.exe")
        "python.exe"))
   
   ;; Fallback - use system PATH
   (t (or (executable-find "python3")
          (executable-find "python") 
          "python3"))))

;; Set Python interpreter with fallback (only if auto-detection might fail)
(unless (executable-find "python3")
  ;; Only override if python3 is not in PATH (e.g., some Windows/WSL setups)
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
(setq org-id-link-to-org-use-id t)

;; ============================================================================
;; REFILE CONFIGURATION
;; ============================================================================

;; Simple refile configuration using agenda files
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((org-agenda-files :maxlevel . 3)
                          (nil :maxlevel . 9)))

(provide 'org-basic)
