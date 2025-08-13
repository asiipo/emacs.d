;;; org-basic.el --- PARA structure, agenda, refile, archive -*- lexical-binding: t; -*-

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

;; Set agenda files to use the dynamic function
(setq org-agenda-files-function #'my/org-agenda-files)

;; Also set initial agenda files explicitly
(setq org-agenda-files (my/org-agenda-files))

;; Auto-refresh agenda after capture
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-after-finalize-hook 
            (lambda () 
              ;; Update agenda files list
              (setq org-agenda-files (my/org-agenda-files))
              ;; Refresh agenda view if open
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
  "Refresh the agenda view to include any new files."
  (interactive)
  ;; Update agenda files list
  (setq org-agenda-files (my/org-agenda-files))
  ;; Refresh agenda view if open
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "Agenda refreshed!"))))

(defun my/debug-agenda-files ()
  "Debug function to show which files are being scanned by agenda."
  (interactive)
  (let ((files (my/org-agenda-files)))
    (message "Agenda scanning %d files: %s" 
             (length files) 
             (mapconcat #'file-name-nondirectory files ", "))))

(defun my/force-refresh-agenda-files ()
  "Force refresh of agenda files and clear any caches."
  (interactive)
  (setq org-agenda-files (my/org-agenda-files))
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)))
  (message "Agenda files forcefully refreshed!"))

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================

;; Quick access to agenda and inbox
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c i") #'my/goto-inbox)
(global-set-key (kbd "C-c A") #'my/refresh-agenda)
(global-set-key (kbd "C-c C-w") #'org-refile)  ;; Refile items from inbox

;; ============================================================================
;; FILE ARCHIVING SYSTEM
;; ============================================================================

;; Archive whole files into appropriate archive subfolders
(defun my/org--archive-dest-for-file (file)
  "Return destination path for FILE under archive/ hierarchy.
Routes files to archive/projects/, archive/areas/, or archive/resources/
based on their source location. Adds timestamp suffix to avoid conflicts."
  (let* ((file (expand-file-name file))
         (base (file-name-as-directory (expand-file-name "archive" org-directory)))
         (projects (file-name-as-directory (expand-file-name "projects" org-directory)))
         (areas    (file-name-as-directory (expand-file-name "areas" org-directory)))
         (resources (file-name-as-directory (expand-file-name "resources" org-directory)))
         (subdir (cond
                  ((string-prefix-p projects file) "projects")
                  ((string-prefix-p areas file)    "areas")
                  ((string-prefix-p resources file) "resources")
                  (t "")))
         (dest-dir (if (string-empty-p subdir)
                       base
                     (file-name-as-directory (expand-file-name subdir base))))
         (dest (expand-file-name (file-name-nondirectory file) dest-dir)))
    (make-directory dest-dir t)
    ;; Avoid overwriting: add -YYYYMMDD-HHMMSS if file exists
    (while (file-exists-p dest)
      (setq dest (expand-file-name
                  (format "%s-%s%s"
                          (file-name-sans-extension (file-name-nondirectory file))
                          (format-time-string "%Y%m%d-%H%M%S")
                          (file-name-extension file t))
                  dest-dir)))
    dest))

(defun my/org-archive-file ()
  "Move the current Org file into the appropriate archive subfolder.
Based on the file's location in the PARA structure."
  (interactive)
  (unless (derived-mode-p 'org-mode) 
    (user-error "Not in an Org buffer"))
  (let* ((src (or buffer-file-name (user-error "Buffer not visiting a file")))
         (dest (my/org--archive-dest-for-file src)))
    (save-buffer)
    (let ((buf (current-buffer)))
      (kill-buffer buf))
    (rename-file src dest t)
    (find-file dest)
    (message "Archived file to: %s" dest)))

(defun my/org-agenda-archive-file ()
  "Move the source Org file of the current agenda item into archive.
Handles multiple buffers visiting the same file safely."
  (interactive)
  (unless (derived-mode-p 'org-agenda-mode) 
    (user-error "Not in an Org agenda"))
  (let* ((m (or (org-get-at-bol 'org-hd-marker) (org-get-at-bol 'org-marker)))
         (src-buf (and m (marker-buffer m)))
         (src-file (and src-buf (buffer-file-name src-buf))))
    (unless src-file 
      (user-error "No source file for current agenda item"))
    (let* ((dest (my/org--archive-dest-for-file src-file))
           (live-bufs (cl-remove-if-not (lambda (b)
                                          (let ((bf (buffer-local-value 'buffer-file-name b)))
                                            (and bf (file-equal-p bf src-file))))
                                        (buffer-list))))
      (dolist (b live-bufs) 
        (with-current-buffer b (save-buffer)) 
        (kill-buffer b))
      (rename-file src-file dest t)
      (message "Archived file to: %s" dest)
      (org-agenda-redo))))

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

;; Source code evaluation (see Org Mode Guide section 14)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

;; Stable links for reliable references (see Org Mode Guide section 4)
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;; ============================================================================
;; REFILE CONFIGURATION
;; ============================================================================

;; Configure refiling to move items from inbox to appropriate PARA locations
;; Based on Org Mode Guide section 9.2

(defun my/org--collect-org-files-recursively (dir)
  "Collect all .org files recursively from directory DIR."
  (when (file-directory-p dir)
    (directory-files-recursively dir "\\.org\\'")))

(defun my/org--refile-files ()
  "Return list of all .org files in projects, areas, and resources directories."
  (let ((projects (when (file-directory-p (expand-file-name "projects" org-directory))
                    (my/org--collect-org-files-recursively (expand-file-name "projects" org-directory))))
        (areas (when (file-directory-p (expand-file-name "areas" org-directory))
                 (my/org--collect-org-files-recursively (expand-file-name "areas" org-directory))))
        (resources (when (file-directory-p (expand-file-name "resources" org-directory))
                     (my/org--collect-org-files-recursively (expand-file-name "resources" org-directory)))))
    (append projects areas resources)))

;; Refile targets: current file + top of files in Projects/Areas/Resources
;; Also allow refiling directly to "Next actions" headings
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      `((nil :maxlevel . 3)  ;; Current buffer
        ,@(mapcar (lambda (f) (cons f '(:level . 1))) (my/org--refile-files))
        ,@(mapcar (lambda (f) (cons f '(:regexp . "^\\*+ Next actions\\b"))) (my/org--refile-files))))

(provide 'org-basic)
