;;; config-loader.el --- Smart configuration loading system -*- lexical-binding: t; -*-

;; Author: Personal Configuration
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: configuration, modules, error-handling

;; This module provides robust configuration loading with error handling,
;; performance monitoring, and diagnostic capabilities.
;;
;; Key Functions:
;;   my/load-all-config-modules  - Load all modules with error reporting
;;   my/diagnose-config          - Display system diagnostic information
;;   my/load-config-module       - Load individual module (internal)
;;
;; Usage:
;;   The config loader is automatically invoked by init.el
;;   For manual diagnosis: M-x my/diagnose-config

;; ============================================================================
;; CONFIGURATION LOADING SYSTEM
;; ============================================================================

(defvar my/config-modules
  '(;; === FOUNDATION LAYER ===
    path-utils          ;; Cross-platform path utilities (must be first)
    features            ;; Feature toggle system  
    core-init          ;; Backups, autosaves, editing defaults
    
    ;; === ORG MODE ECOSYSTEM ===
    org-basic            ;; PARA structure, agenda, refile, archive
    org-capture-config   ;; Capture templates and inbox workflow
    org-agenda-config    ;; Agenda display and behavior
    
    ;; === SPECIALIZED FEATURES ===  
    reading-tracker      ;; Reading progress tracking
    journal             ;; Daily journaling with datetree
    welcome             ;; Startup dashboard and reading dashboard
    
    ;; === EXTERNAL INTEGRATIONS ===
    magit-config        ;; Git integration
    spell-checking      ;; Modern spell checking with Jinx
    latex-config        ;; LaTeX editing and preview
    
    ;; === USER INTERFACE === (must be last)
    keybindings)        ;; Centralized global keybindings
  "List of configuration modules to load in dependency order.
Each module is loaded with error handling and progress reporting.")

(defvar my/config-failed-modules nil
  "List of modules that failed to load with their error messages.")

(defvar my/config-load-start-time nil
  "Time when configuration loading started.")

(defun my/load-config-module (module)
  "Load a configuration MODULE with error handling."
  (condition-case err
      (progn
        (require module)
        (message "✓ Loaded: %s" module)
        t) ;; Return success
    (error 
     (let ((error-msg (error-message-string err)))
       (add-to-list 'my/config-failed-modules (cons module error-msg))
       (message "✗ Failed to load %s: %s" module error-msg)
       nil)))) ;; Return failure

(defun my/load-all-config-modules ()
  "Load all configuration modules with comprehensive error reporting."
  (interactive)
  (setq my/config-failed-modules nil
        my/config-load-start-time (current-time))
  
  (message "🚀 Starting configuration loading...")
  
  (let ((success-count 0)
        (total-count (length my/config-modules)))
    
    (dolist (module my/config-modules)
      (when (my/load-config-module module)
        (setq success-count (1+ success-count))))
    
    ;; Final report
    (let* ((load-time (float-time (time-subtract (current-time) my/config-load-start-time)))
           (failed-count (length my/config-failed-modules)))
      
      (if (zerop failed-count)
          (message "🎉 Configuration loaded successfully! %d modules in %.3f seconds"
                   success-count load-time)
        (progn
          (message "⚠️  Configuration loaded with issues: %d/%d modules succeeded in %.3f seconds"
                   success-count total-count load-time)
          (message "Failed modules:")
          (dolist (failure my/config-failed-modules)
            (message "  - %s: %s" (car failure) (cdr failure)))
          (message "Run M-x my/diagnose-config for troubleshooting help")))
      
      ;; Return success status
      (zerop failed-count))))

(defun my/diagnose-config ()
  "Provide diagnostic information for configuration issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Config Diagnosis*")
    (erase-buffer)
    (insert "🔍 Emacs Configuration Diagnosis\n")
    (insert "================================\n\n")
    
    ;; Basic info
    (insert (format "Emacs version: %s\n" emacs-version))
    (insert (format "System: %s\n" system-type))
    (insert (format "Load path includes: %s\n" 
                    (mapcar (lambda (p) (file-name-nondirectory p)) load-path)))
    (insert "\n")
    
    ;; Failed modules
    (if my/config-failed-modules
        (progn
          (insert "❌ Failed Modules:\n")
          (dolist (failure my/config-failed-modules)
            (insert (format "  • %s\n    Error: %s\n\n" (car failure) (cdr failure)))))
      (insert "✅ All modules loaded successfully\n\n"))
    
    ;; Package status
    (insert "📦 Package Status:\n")
    (let ((required-packages '(magit doom-themes jinx)))
      (dolist (pkg required-packages)
        (insert (format "  • %s: %s\n" pkg 
                        (if (package-installed-p pkg) "✓ installed" "✗ missing")))))
    
    ;; Org directory
    (insert (format "\n📁 Org Directory: %s\n" org-directory))
    (insert (format "  Exists: %s\n" 
                    (if (file-directory-p org-directory) "✓ yes" "✗ no")))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'config-loader)
