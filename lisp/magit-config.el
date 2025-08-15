;;; magit-config.el --- Magit setup -*- lexical-binding: t; -*-

(defun my/magit-org-status ()
  "Open magit-status for the org directory specifically."
  (interactive)
  (if (and org-directory (file-directory-p org-directory))
      (let ((default-directory org-directory))
        (if (file-exists-p ".git")
            (magit-status)
          (message "Org directory is not a git repository")))
    (message "Org directory not found")))

;; Always open magit for org directory, not current directory
(global-set-key (kbd "C-x g") #'my/magit-org-status)

;; Use same window behavior for popups if you prefer minimal window churn
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; ============================================================================
;; AUTO-SYNC ORG DIRECTORY
;; ============================================================================

(defvar my/org-auto-sync-enabled t
  "Enable automatic Git sync for org directory.")

(defvar my/org-last-sync-time 0
  "Timestamp of last sync to prevent too frequent commits.")

(defvar my/org-sync-delay 60  ;; Increase to 60 seconds for less noise
  "Minimum seconds between auto-sync operations.")

(defvar my/org-sync-in-progress nil
  "Flag to indicate if sync is currently in progress.")

(defun my/org-sync-set-indicator (active)
  "Set or clear the sync indicator in mode line."
  (setq my/org-sync-in-progress active)
  (force-mode-line-update t))

(defun my/org-sync-message (msg &optional warning)
  "Display sync message with optional warning styling."
  (if warning
      (message (propertize msg 'face '(:foreground "orange" :weight bold)))
    (message (propertize msg 'face '(:foreground "forest green" :weight bold)))))

(defun my/org-git-auto-sync ()
  "Automatically sync org directory: pull first, then commit and push changes.
Only runs if enough time has passed since last sync."
  (when (and my/org-auto-sync-enabled
             org-directory
             (file-directory-p org-directory)
             (> (float-time) (+ my/org-last-sync-time my/org-sync-delay)))
    (let ((default-directory org-directory))
      (when (file-exists-p ".git")
        (condition-case err
            (progn
              ;; Set sync indicator and warning
              (my/org-sync-set-indicator t)
              (my/org-sync-message "⚠️  SYNC IN PROGRESS - Don't quit Emacs!" t)
              
              ;; Step 1: Pull latest changes first
              (shell-command "git pull origin main > /dev/null 2>&1")
              
              ;; Step 2: Add all local changes
              (shell-command "git add -A")
              
              ;; Step 3: Check if there are changes to commit
              (let ((status (shell-command-to-string "git status --porcelain")))
                (if (not (string-empty-p (string-trim status)))
                    (progn
                      ;; Step 4: Commit with timestamp
                      (let ((commit-msg (format "Auto-sync: %s" 
                                                (format-time-string "%Y-%m-%d %H:%M:%S"))))
                        (shell-command (format "git commit -m \"%s\"" commit-msg))
                        ;; Step 5: Push to remote
                        (shell-command "git push origin main > /dev/null 2>&1")
                        (my/org-sync-message "✅ Org directory synced (pull + commit + push)")
                        (setq my/org-last-sync-time (float-time))))
                  ;; No local changes, but we still pulled
                  (progn
                    (my/org-sync-message "✅ Org directory synced (pull only)")
                    (setq my/org-last-sync-time (float-time)))))
              
              ;; Clear sync indicator
              (my/org-sync-set-indicator nil))
          (error
           ;; Clear indicator on error too
           (my/org-sync-set-indicator nil)
           (my/org-sync-message (format "❌ Org auto-sync failed: %s" (error-message-string err)) t)))))))

;; Manual sync function - now does full bidirectional sync
(defun my/org-sync-now ()
  "Manually sync org directory: pull latest, commit, and push changes."
  (interactive)
  (setq my/org-last-sync-time 0) ; Reset timer to force sync
  (my/org-git-auto-sync))

(defun my/org-setup-auto-sync ()
  "Set up automatic sync hooks for org files."
  (when (and org-directory (file-directory-p org-directory))
    ;; Add hook to org-mode files in org directory
    (add-hook 'after-save-hook 
              (lambda ()
                (when (and buffer-file-name
                           (string-prefix-p (expand-file-name org-directory) 
                                          (expand-file-name buffer-file-name)))
                  ;; Run sync in background after a short delay
                  (run-with-timer 2 nil #'my/org-git-auto-sync))))))

;; Toggle auto-sync
(defun my/org-toggle-auto-sync ()
  "Toggle automatic Git sync for org directory."
  (interactive)
  (setq my/org-auto-sync-enabled (not my/org-auto-sync-enabled))
  (message "Org auto-sync %s" 
           (if my/org-auto-sync-enabled "ENABLED" "DISABLED")))

;; Keybindings
(global-set-key (kbd "C-c g") #'my/magit-org-status)      ;; Git status for org directory
(global-set-key (kbd "C-c G s") #'my/org-sync-now)        ;; Manual sync (uppercase G)
(global-set-key (kbd "C-c G t") #'my/org-toggle-auto-sync) ;; Toggle auto-sync

;; Initialize auto-sync when org-directory is available
(with-eval-after-load 'org
  (my/org-setup-auto-sync))

;; Add sync indicator to mode line
(add-to-list 'mode-line-misc-info
             '(:eval (when my/org-sync-in-progress
                       (propertize " [SYNC]" 'face '(:foreground "orange" :weight bold)))))

;; Pull org files on startup (useful for new computers or syncing changes)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Small delay to ensure org-directory is available
            (run-with-timer 3 nil
              (lambda ()
                (when (and org-directory (file-directory-p org-directory))
                  (let ((default-directory org-directory))
                    (when (file-exists-p ".git")
                      (condition-case err
                          (progn
                            (my/org-sync-set-indicator t)
                            (my/org-sync-message "⚠️  STARTUP SYNC - Don't quit Emacs!" t)
                            (shell-command "git pull origin main > /dev/null 2>&1")
                            (my/org-sync-message "✅ Org files synced from remote")
                            (my/org-sync-set-indicator nil))
                        (error
                         (my/org-sync-set-indicator nil)
                         (my/org-sync-message (format "❌ Org startup sync failed: %s" (error-message-string err)) t))))))))))

(provide 'magit-config)
