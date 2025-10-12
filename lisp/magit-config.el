;;; magit-config.el --- Magit setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit configuration with org-directory integration and automatic syncing.

(require 'subr-x)

;; ============================================================================
;; MAGIT BASIC CONFIGURATION
;; ============================================================================

;; Use same window behavior for popups (minimal window churn)
(with-eval-after-load 'magit
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun my/magit-org-status ()
  "Open magit-status for the org directory specifically."
  (interactive)
  (if (and org-directory (file-directory-p org-directory))
      (let ((default-directory org-directory))
        (if (file-exists-p ".git")
            (magit-status)
          (message "Org directory is not a git repository")))
    (message "Org directory not found")))

;; ============================================================================
;; AUTO-SYNC ORG DIRECTORY
;; ============================================================================

(defvar my/org-auto-sync-enabled t
  "Enable automatic Git sync for org directory.")

(defvar my/org-last-sync-time 0
  "Timestamp of last sync to prevent too frequent commits.")

(defvar my/org-sync-delay 60
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

(defun my/org-git-command (command &optional silent)
  "Execute git command in org directory. Return t if successful."
  (let ((default-directory org-directory))
    (when (file-exists-p ".git")
      (let ((cmd (if silent
                     (format "%s > /dev/null 2>&1" command)
                   command)))
        (zerop (shell-command cmd))))))

(defun my/org-git-auto-pull ()
  "Automatically pull latest changes from remote (no commit/push).
Only runs if enough time has passed since last sync and no other sync is running."
  (when (and my/org-auto-sync-enabled
             (not my/org-sync-in-progress)
             org-directory
             (file-directory-p org-directory)
             (> (float-time) (+ my/org-last-sync-time my/org-sync-delay)))
    (let ((default-directory org-directory))
      (when (file-exists-p ".git")
        (condition-case err
            (progn
              (my/org-sync-set-indicator t)
              (my/org-sync-message "⬇️  Pulling latest changes..." t)
              
              ;; Only pull - no commit or push
              (if (my/org-git-command "git pull origin main" t)
                  (my/org-sync-message "✅ Pulled latest changes from remote")
                (my/org-sync-message "⚠️  Pull had issues, check manually" t))
              
              (setq my/org-last-sync-time (float-time))
              (my/org-sync-set-indicator nil))
          (error
           (my/org-sync-set-indicator nil)
           (my/org-sync-message (format "❌ Auto-pull failed: %s" (error-message-string err)) t)))))))

(defun my/org-git-full-sync ()
  "Manual full sync: pull, commit all changes, and push to remote."
  (when (and org-directory
             (file-directory-p org-directory))
    (let ((default-directory org-directory))
      (when (file-exists-p ".git")
        (condition-case err
            (progn
              (my/org-sync-set-indicator t)
              (my/org-sync-message "⚠️  FULL SYNC - Don't quit Emacs!" t)
              
              ;; Step 1: Pull latest changes
              (unless (my/org-git-command "git pull origin main" t)
                (my/org-sync-message "⚠️  Pull had conflicts, resolve manually" t)
                (my/org-sync-set-indicator nil)
                (error "Pull failed"))
              
              ;; Step 2: Add all changes
              (shell-command "git add -A")
              
              ;; Step 3: Check for changes and commit
              (let ((status (shell-command-to-string "git status --porcelain")))
                (if (not (string-empty-p (string-trim status)))
                    (progn
                      (let ((commit-msg (format "Sync: %s" 
                                                (format-time-string "%Y-%m-%d %H:%M:%S"))))
                        (shell-command (format "git commit -m %s" (shell-quote-argument commit-msg)))
                        
                        ;; Step 4: Push to remote
                        (if (my/org-git-command "git push origin main" t)
                            (my/org-sync-message "✅ Full sync complete (pull + commit + push)")
                          (my/org-sync-message "⚠️  Push failed, changes saved locally" t))))
                  (my/org-sync-message "✅ No local changes to commit")))
              
              (my/org-sync-set-indicator nil))
          (error
           (my/org-sync-set-indicator nil)
           (my/org-sync-message (format "❌ Full sync failed: %s" (error-message-string err)) t)))))))

;; Manual full sync function (C-c g s)
(defun my/org-sync-now ()
  "Manually sync org directory: pull latest, commit, and push changes."
  (interactive)
  (my/org-git-full-sync))

(defun my/org-setup-auto-sync ()
  "Set up automatic pull on startup (no auto-commit on save)."
  (message "Auto-sync configured: Pull on startup only, manual sync with C-c g s"))

(defun my/org-toggle-auto-sync ()
  "Toggle automatic Git pull on startup for org directory."
  (interactive)
  (setq my/org-auto-sync-enabled (not my/org-auto-sync-enabled))
  (message "Org auto-pull on startup %s" 
           (if my/org-auto-sync-enabled "ENABLED" "DISABLED")))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================
;; Keybindings are now centralized in keybindings.el

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
                            (if (my/org-git-command "git pull origin main" t)
                                (my/org-sync-message "✅ Org files synced from remote")
                              (my/org-sync-message "⚠️  Startup pull had issues" t))
                            (my/org-sync-set-indicator nil))
                        (error
                         (my/org-sync-set-indicator nil)
                         (my/org-sync-message (format "❌ Org startup sync failed: %s" (error-message-string err)) t))))))))))

(provide 'magit-config)
