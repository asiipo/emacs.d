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

(defvar my/org-sync-delay 30
  "Minimum seconds between auto-sync operations.")

(defun my/org-git-auto-sync ()
  "Automatically commit and push changes in org directory.
Only runs if enough time has passed since last sync."
  (when (and my/org-auto-sync-enabled
             org-directory
             (file-directory-p org-directory)
             (> (float-time) (+ my/org-last-sync-time my/org-sync-delay)))
    (let ((default-directory org-directory))
      (when (file-exists-p ".git")
        (condition-case err
            (progn
              ;; Add all changes
              (shell-command "git add -A")
              ;; Check if there are changes to commit
              (let ((status (shell-command-to-string "git status --porcelain")))
                (when (not (string-empty-p (string-trim status)))
                  ;; Commit with timestamp
                  (let ((commit-msg (format "Auto-sync: %s" 
                                          (format-time-string "%Y-%m-%d %H:%M:%S"))))
                    (shell-command (format "git commit -m \"%s\"" commit-msg))
                    ;; Push to remote (suppress output to avoid noise)
                    (shell-command "git push origin main > /dev/null 2>&1")
                    (message "Org directory synced to remote")
                    (setq my/org-last-sync-time (float-time))))))
          (error
           (message "Org auto-sync failed: %s" (error-message-string err))))))))

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

;; Manual sync function
(defun my/org-sync-now ()
  "Manually sync org directory to remote repository."
  (interactive)
  (setq my/org-last-sync-time 0) ; Reset timer to force sync
  (my/org-git-auto-sync))

;; Toggle auto-sync
(defun my/org-toggle-auto-sync ()
  "Toggle automatic Git sync for org directory."
  (interactive)
  (setq my/org-auto-sync-enabled (not my/org-auto-sync-enabled))
  (message "Org auto-sync %s" 
           (if my/org-auto-sync-enabled "ENABLED" "DISABLED")))

;; Keybindings
(global-set-key (kbd "C-c g s") #'my/org-sync-now)
(global-set-key (kbd "C-c g t") #'my/org-toggle-auto-sync)

;; Initialize auto-sync when org-directory is available
(with-eval-after-load 'org
  (my/org-setup-auto-sync))

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
                            (message "Syncing org files from remote...")
                            (shell-command "git pull origin main > /dev/null 2>&1")
                            (message "Org files synced from remote"))
                        (error
                         (message "Org startup sync failed: %s" (error-message-string err)))))))))))

(provide 'magit-config)
