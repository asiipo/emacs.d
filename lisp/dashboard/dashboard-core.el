;;; dashboard-core.el --- Core dashboard functionality -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)
(require 'org-table)

;; ============================================================================
;; STARTUP TIMING
;; ============================================================================

(defvar dashboard--startup-time nil
  "Time taken to load Emacs configuration in seconds.")

(defun dashboard--calculate-startup-time ()
  "Calculate and store the startup time from emacs-init-time."
  (cond
   ;; If emacs-init-time is available and set, use it
   ((and (boundp 'emacs-init-time) emacs-init-time (boundp 'before-init-time) before-init-time)
    (setq dashboard--startup-time
          (float-time (time-subtract emacs-init-time before-init-time))))
   ;; If we have before-init-time but no emacs-init-time yet, estimate from current time
   ((and (boundp 'before-init-time) before-init-time)
    (setq dashboard--startup-time
          (float-time (time-subtract (current-time) before-init-time))))
   ;; Fallback: show a reasonable default
   (t
    (setq dashboard--startup-time 0.5))))

(defun dashboard--format-startup-time ()
  "Format startup time for display."
  ;; Force recalculation if still nil
  (when (null dashboard--startup-time)
    (dashboard--calculate-startup-time))
  
  (cond
   ((null dashboard--startup-time)
    "⚡ Ready")
   ((< dashboard--startup-time 1.0)
    (format "⚡ Loaded in %.0f ms" (* dashboard--startup-time 1000)))
   (t
    (format "⚡ Loaded in %.2f seconds" dashboard--startup-time))))

;; Calculate startup time when this module loads (only once)
(dashboard--calculate-startup-time)

;; Don't recalculate startup time on refresh - keep the original time
;; (defun dashboard--update-startup-time ()
;;   "Update startup time calculation for dashboard refresh."
;;   (dashboard--calculate-startup-time))
;; 
;; (add-hook 'dashboard-refresh-hook 'dashboard--update-startup-time)

;; ============================================================================
;; DASHBOARD MODE
;; ============================================================================

;; Hook run when dashboard is refreshed
(defvar dashboard-refresh-hook nil
  "Hook run when the dashboard is refreshed.")

;; A major mode for the dashboard buffer
(define-derived-mode dashboard-mode special-mode "Personal Workspace"
  "Mode for the personal productivity workspace dashboard.
Shows reading progress, time tracking, key bindings, and quick access to important functions."
  (read-only-mode 1)
  (setq truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1)
  (orgtbl-mode 1)
  (define-key dashboard-mode-map (kbd "g") #'dashboard-refresh)
  (define-key dashboard-mode-map (kbd "q") #'quit-window))

;; ============================================================================
;; CHEATSHEET CONFIGURATION
;; ============================================================================

;; Curate your key cheatsheet here (add/remove as you like).
;; Format: either (:section "Title") or ("KEYS" "Description" COMMAND-or-nil)
(defvar dashboard-cheatsheet-keys
  '((:section "Org & Agenda")
    ("C-c a"   "Open agenda"                         org-agenda)
    ("C-c A"   "Refresh agenda"                      my/refresh-agenda)
    ("C-c i"   "Go to inbox"                         my/goto-inbox)
    ("C-c c"   "Capture menu"                        org-capture)
    ("C-c C-w" "Refile item"                         org-refile)
    ("C-c d"   "Go to /org"                    (lambda () (interactive) (dired org-directory)))
    ("C-c e"   "Go to .emacs.d"                      (lambda () (interactive) (dired user-emacs-directory)))
    ("C-c C-x a" "Archive file"                      my/org-archive-file)

    (:section "Reading tracker")
    ("C-c r o" "Open reading.org"                    my/org-reading-open)
    ("C-c r b" "Open books.org"                      my/org-reading-open-books)
    ("C-c r n" "Open book notes"                     my/org-reading-open-book-notes)
    ("C-c r a" "Add a book"                          my/org-reading-add-book)
    ("C-c r u" "Update current page"                 my/org-reading-set-current-page)
    ("C-c r c" "Complete a book"                     my/org-reading-complete-book)
    ("C-c r d" "Delete a book"                       my/org-reading-delete-book)

    (:section "Git Sync")
    ("C-c g g" "Git status (org)"                    my/magit-org-status)
    ("C-c g s" "Sync org to remote now"              my/org-sync-now)
    ("C-c g t" "Toggle auto-sync"                    my/org-toggle-auto-sync)
    ("C-x g"   "Magit status (any repo)"             magit-status)

    (:section "Spell Checking")
    ("C-c s c" "Correct word"                        jinx-correct)
    ("C-c s b" "Correct buffer"                      my/jinx-correct-buffer)
    ("C-c s l" "Switch language"                     jinx-languages)
    ("C-c s n" "Next misspelling"                    jinx-next)
    ("C-c s p" "Previous misspelling"                jinx-previous)
    ("M-x my/jinx-switch-to-english" "English only"  my/jinx-switch-to-english)
    ("M-x my/jinx-switch-to-finnish" "Finnish only"  my/jinx-switch-to-finnish)
    ("M-x my/jinx-switch-to-bilingual" "EN + FI"     my/jinx-switch-to-bilingual))
  "Rows for the startup cheatsheet buffer.")

;; ============================================================================
;; GIT STATUS HELPER
;; ============================================================================

(defun dashboard--git-status-clean-p ()
  "Return t if org directory git status is completely clean, nil otherwise.
Detects: unstaged changes, staged changes, untracked files, unmerged files, and ahead/behind remote."
  (let ((org-dir (or org-directory (expand-file-name "~/org"))))
    (when (and org-dir (file-directory-p org-dir))
      (let ((default-directory org-dir))
        (when (file-exists-p ".git")
          (condition-case nil
              (let* ((status-output (shell-command-to-string "git status --porcelain"))
                     (branch-status (shell-command-to-string "git status -b --porcelain | head -1"))
                     (has-local-changes (not (string-empty-p (string-trim status-output))))
                     (is-ahead (string-match "ahead" branch-status))
                     (is-behind (string-match "behind" branch-status)))
                ;; Repository is clean only if:
                ;; 1. No local file changes (unstaged, staged, untracked, unmerged)
                ;; 2. Not ahead of remote
                ;; 3. Not behind remote
                (not (or has-local-changes is-ahead is-behind)))
            (error nil)))))))

;; ============================================================================
;; CHEATSHEET RENDERING
;; ============================================================================

(defun dashboard--render-keybindings ()
  "Render the key bindings section in a 2x2 grid layout."
  ;; Split key bindings into sections
  (let ((org-section '())
        (reading-section '())
        (git-section '())
        (spell-section '())
        (current-section nil))
    
    ;; Separate the sections
    (dolist (row dashboard-cheatsheet-keys)
      (cond
       ((and (consp row) (keywordp (car row)) (eq (car row) :section))
        (setq current-section (cadr row)))
       ((string-equal current-section "Org & Agenda")
        (push row org-section))
       ((string-equal current-section "Reading tracker")
        (push row reading-section))
       ((string-equal current-section "Git Sync")
        (push row git-section))
       ((string-equal current-section "Spell Checking")
        (push row spell-section))))
    
    ;; Reverse to maintain original order
    (setq org-section (nreverse org-section))
    (setq reading-section (nreverse reading-section))
    (setq git-section (nreverse git-section))
    (setq spell-section (nreverse spell-section))
    
    ;; Insert 2x2 grid layout
    (insert "Key Bindings\n")
    (insert "════════════\n\n")
    (insert (format "%-40s %s\n" "ORG & AGENDA" "READING TRACKER"))
    (insert (format "%-40s %s\n" (make-string 12 ?-) (make-string 15 ?-)))
    
    ;; Print org and reading sections side by side (top row)
    (let ((max-rows (max (length org-section) (length reading-section))))
      (dotimes (i max-rows)
        (let ((org-row (nth i org-section))
              (reading-row (nth i reading-section)))
          (insert (format "%-40s %s\n"
                          (if org-row
                              (format "%-15s %s" (nth 0 org-row) (nth 1 org-row))
                            "")
                          (if reading-row
                              (format "%-15s %s" (nth 0 reading-row) (nth 1 reading-row))
                            ""))))))
    
    ;; Add Git Sync and Spell Checking sections below (bottom row)
    (insert "\n")
    (let* ((is-clean (dashboard--git-status-clean-p))
           (status-text (if is-clean 
                            "GIT SYNC ✅" 
                            "GIT SYNC ⚠️")))
      (insert (format "%-40s %s\n" 
                      status-text
                      "SPELL CHECKING")))
    (insert (format "%-40s %s\n" (make-string 8 ?-) (make-string 14 ?-)))
    
    (let ((max-rows (max (length git-section) (length spell-section))))
      (dotimes (i max-rows)
        (let ((git-row (nth i git-section))
              (spell-row (nth i spell-section)))
          (insert (format "%-40s %s\n"
                          (if git-row
                              (format "%-15s %s" (nth 0 git-row) (nth 1 git-row))
                            "")
                          (if spell-row
                              (format "%-15s %s" (nth 0 spell-row) (nth 1 spell-row))
                            ""))))))))

;; ============================================================================
;; MAIN DASHBOARD FUNCTIONS
;; ============================================================================

(defun dashboard-render ()
  "Render the complete dashboard content in the current buffer.
Creates the complete welcome page with reading dashboard, time tracking, and key bindings."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "📚 Personal Workspace — %s\n" 
                    (format-time-string "%Y-%m-%d %H:%M")))
    (insert "═════════════════════\n\n") 
    (insert (propertize "Productivity system based on PARA methodology + GTD principles\n" 
                        'face '(:foreground "dim gray")))
    (insert (propertize (format "Press 'g' to refresh • 'q' to close • %s\n" 
                                (dashboard--format-startup-time))
                        'face '(:foreground "dim gray")))
    
    ;; Insert dashboard components
    (when (fboundp 'reading-dashboard-insert)
      (reading-dashboard-insert))
    
    (when (fboundp 'time-dashboard-insert)
      (time-dashboard-insert))
    
    (insert "\n")
    (dashboard--render-keybindings)
    
    (goto-char (point-min))))

(defun dashboard-show ()
  "Display the dashboard in a new buffer.
Creates or switches to the dashboard buffer and renders the content."
  (interactive)
  (let ((buf (get-buffer-create "*Personal Workspace*")))
    (with-current-buffer buf
      (dashboard-mode)
      (dashboard-render))
    (switch-to-buffer buf)))

(defun dashboard-refresh ()
  "Re-render the dashboard buffer.
Updates the content with current reading progress, time tracking, and key bindings."
  (interactive)
  (when (derived-mode-p 'dashboard-mode)
    (run-hooks 'dashboard-refresh-hook)
    (dashboard-render)))

(provide 'dashboard-core)
