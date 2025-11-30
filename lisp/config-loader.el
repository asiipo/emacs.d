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
    core-init          ;; Core Emacs optimization: performance, UI, editing, security
    
    ;; === ORG MODE ECOSYSTEM ===
    org-basic            ;; PARA structure, agenda, refile, archive
    org-capture-config   ;; Capture templates and inbox workflow
    org-agenda-config    ;; Agenda display and behavior
    org-roam-config      ;; Org-roam networked note-taking
    
    ;; === SPECIALIZED FEATURES ===  
    gtd                 ;; GTD daily tracking with routine management
    reading-tracker     ;; Simple reading progress tracking
    journal             ;; Daily journaling with datetree
    dashboard           ;; Consolidated startup dashboard with time tracking
    
    ;; === EXTERNAL INTEGRATIONS ===
    zotero-bibtex       ;; Zotero Better BibTeX integration
    magit-config        ;; Git integration
    spell-checking-config  ;; Modern spell checking with Jinx
    latex-config        ;; LaTeX editing and preview
    matlab-config       ;; MATLAB editing with platform-specific paths
    copilot-config      ;; GitHub Copilot AI code completion
    
    ;; === USER INTERFACE === (must be last)
    modeline-config     ;; Clean mode line by hiding noisy minor modes
    keybindings)        ;; Centralized global keybindings
  "List of configuration modules to load in dependency order.
Each module is loaded with error handling and progress reporting.")

(defvar my/config-module-cache (make-hash-table :test 'eq)
  "Cache for loaded modules to prevent duplicate loading.")

(defvar my/config-timing-data nil
  "Detailed timing data for each module load.")

(defvar my/config-total-load-time nil
  "Total configuration load time in seconds.")

(defvar my/config-failed-modules nil
  "List of modules that failed to load with their error messages.")

(defvar my/config-load-start-time nil
  "Time when configuration loading started.")

(defun my/load-config-module (module)
  "Load a configuration MODULE with error handling and timing."
  ;; Check cache first
  (if (gethash module my/config-module-cache)
      (progn
        (message "⚡ Cached: %s" module)
        t)
    ;; Load and time the module
    (let ((module-start-time (current-time)))
      (condition-case err
          (progn
            (require module)
            (let ((load-time (float-time (time-subtract (current-time) module-start-time))))
              (puthash module t my/config-module-cache)
              (push (cons module load-time) my/config-timing-data)
              (if (> load-time 0.1)
                  (message "✓ Loaded: %s (%.3fs)" module load-time)
                (message "✓ Loaded: %s" module))
              t)) ;; Return success
        (error 
         (let ((error-msg (error-message-string err)))
           (add-to-list 'my/config-failed-modules (cons module error-msg))
           (message "✗ Failed to load %s: %s" module error-msg)
           nil)))))) ;; Return failure

(defun my/load-all-config-modules ()
  "Load all configuration modules with comprehensive error reporting."
  (interactive)
  (setq my/config-failed-modules nil
        my/config-timing-data nil
        my/config-load-start-time (current-time))
  
  (message "🚀 Starting configuration loading...")
  
  (let ((success-count 0)
        (total-count (length my/config-modules)))
    
    (dolist (module my/config-modules)
      (when (my/load-config-module module)
        (setq success-count (1+ success-count))))
    
    ;; Final report with timing analysis
    (let* ((load-time (float-time (time-subtract (current-time) my/config-load-start-time)))
           (failed-count (length my/config-failed-modules))
           (slow-modules (seq-filter (lambda (timing) (> (cdr timing) 0.1)) 
                                   (reverse my/config-timing-data))))
      
      (if (zerop failed-count)
          (progn
            (message "🎉 Configuration loaded successfully! %d modules in %.3f seconds"
                     success-count load-time)
            ;; Report slow modules if any
            (when slow-modules
              (message "⏱️  Slow-loading modules:")
              (dolist (timing slow-modules)
                (message "  - %s: %.3fs" (car timing) (cdr timing)))))
        (progn
          (message "⚠️  Configuration loaded with issues: %d/%d modules succeeded in %.3f seconds"
                   success-count total-count load-time)
          (message "Failed modules:")
          (dolist (failure my/config-failed-modules)
            (message "  - %s: %s" (car failure) (cdr failure)))
          (message "Run M-x my/diagnose-config for troubleshooting help")))
      
      ;; Store total load time for external access
      (setq my/config-total-load-time load-time)
      
      ;; Return success status
      (zerop failed-count))))

(defun my/reload-config-module (module)
  "Reload a specific configuration MODULE."
  (interactive 
   (list (intern (completing-read "Reload module: " 
                                  my/config-modules nil t))))
  ;; Clear from cache
  (remhash module my/config-module-cache)
  ;; Force unload if possible
  (when (featurep module)
    (unload-feature module t))
  ;; Reload
  (if (my/load-config-module module)
      (message "✓ Reloaded: %s" module)
    (message "✗ Failed to reload: %s" module)))

(defun my/list-config-timing ()
  "Display timing information for loaded modules."
  (interactive)
  (if my/config-timing-data
      (let ((sorted-data (sort (copy-sequence my/config-timing-data) 
                              (lambda (a b) (> (cdr a) (cdr b))))))
        (message "Module timing: %s" 
                (mapconcat (lambda (timing) 
                           (format "%s(%.3fs)" (car timing) (cdr timing))) 
                          (seq-take sorted-data 5) ", ")))
    (message "No timing data available.")))

(defun my/diagnose-config ()
  "Provide essential diagnostic information for configuration issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Config Diagnosis*")
    (erase-buffer)
    (insert "🔍 Configuration Status\n")
    (insert "======================\n\n")
    
    ;; Module summary
    (let* ((loaded-count (hash-table-count my/config-module-cache))
           (total-count (length my/config-modules))
           (load-time (when my/config-timing-data 
                       (apply #'+ (mapcar #'cdr my/config-timing-data)))))
      (insert (format "Modules: %d/%d loaded" loaded-count total-count))
      (when load-time (insert (format " in %.3fs" load-time)))
      (insert "\n"))
    
    ;; Only show failures if any exist
    (when my/config-failed-modules
      (insert "\n❌ Failed modules:\n")
      (dolist (failure my/config-failed-modules)
        (insert (format "  %s: %s\n" (car failure) (cdr failure)))))
    
    ;; Only warn about slow modules if significantly slow
    (when my/config-timing-data
      (let ((slow-modules (seq-filter (lambda (timing) (> (cdr timing) 0.2)) 
                                     my/config-timing-data)))
        (when slow-modules
          (insert "\n⚠️  Slow modules (>200ms):\n")
          (dolist (timing slow-modules)
            (insert (format "  %s: %.3fs\n" (car timing) (cdr timing)))))))
    
    ;; Critical issues only
    (let ((issues '()))
      (unless (> gc-cons-threshold (* 15 1000 1000))
        (push "GC threshold not optimized" issues))
      (unless (file-directory-p (expand-file-name "var/backups/" user-emacs-directory))
        (push "Backup directory missing" issues))
      (when (boundp 'org-directory)
        (unless (file-directory-p org-directory)
          (push "Org directory missing" issues)))
      
      (if issues
          (progn
            (insert "\n⚠️  Issues:\n")
            (dolist (issue issues)
              (insert (format "  %s\n" issue))))
        (insert "\n✅ No critical issues detected\n")))
    
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; ============================================================================
;; PUBLIC API FOR STARTUP TIMING
;; ============================================================================

(defun my/get-startup-time ()
  "Get the total configuration load time in seconds.
Returns nil if configuration hasn't been loaded yet."
  my/config-total-load-time)

(defun my/format-startup-time ()
  "Format startup time for display.
Returns a formatted string like '⚡ Loaded in 150 ms' or '⚡ Ready'."
  (let ((load-time (my/get-startup-time)))
    (cond
     ((null load-time)
      "⚡ Ready")
     ((< load-time 1.0)
      (format "⚡ Loaded in %.0f ms" (* load-time 1000)))
     (t
      (format "⚡ Loaded in %.2f seconds" load-time)))))

(provide 'config-loader)
