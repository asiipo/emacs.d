;;; config-loader.el --- Configuration module loader -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads configuration modules with error handling and diagnostics.
;; Use M-x my/diagnose-config for troubleshooting.

;;; Code:

(defvar my/config-modules
  '(;; Foundation
    path-utils core-init theme-config
    ;; Org mode
    org-basic org-capture-config org-agenda-config org-roam-config
    ;; Features
    gtd reading-tracker journal dashboard
    ;; Integrations
    zotero-bibtex magit-config spell-checking-config
    latex-config matlab-config julia-config
    ;; UI (load last)
    modeline-config keybindings)
  "Configuration modules in load order.")

(defvar my/config-module-cache (make-hash-table :test 'eq))
(defvar my/config-timing-data nil)
(defvar my/config-total-load-time nil)
(defvar my/config-failed-modules nil)
(defvar my/config-load-start-time nil)

(defun my/load-config-module (module)
  "Load MODULE with error handling and timing."
  (if (gethash module my/config-module-cache)
      (progn
        (message "⚡ Cached: %s" module)
        t)
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
      
      (setq my/config-total-load-time load-time)
      (zerop failed-count))))

(defun my/reload-config-module (module)
  "Reload MODULE."
  (interactive 
   (list (intern (completing-read "Reload module: " 
                                  my/config-modules nil t))))
  (remhash module my/config-module-cache)
  (when (featurep module)
    (unload-feature module t))
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
  "Display configuration diagnostic information."
  (interactive)
  (with-current-buffer (get-buffer-create "*Config Diagnosis*")
    (erase-buffer)
    (insert "🔍 Configuration Status\n")
    (insert "======================\n\n")
    
    (let* ((loaded-count (hash-table-count my/config-module-cache))
           (total-count (length my/config-modules))
           (load-time (when my/config-timing-data 
                       (apply #'+ (mapcar #'cdr my/config-timing-data)))))
      (insert (format "Modules: %d/%d loaded" loaded-count total-count))
      (when load-time (insert (format " in %.3fs" load-time)))
      (insert "\n"))
    
    (when my/config-failed-modules
      (insert "\n❌ Failed modules:\n")
      (dolist (failure my/config-failed-modules)
        (insert (format "  %s: %s\n" (car failure) (cdr failure)))))
    
    (when my/config-timing-data
      (let ((slow-modules (seq-filter (lambda (timing) (> (cdr timing) 0.2)) 
                                     my/config-timing-data)))
        (when slow-modules
          (insert "\n⚠️  Slow modules (>200ms):\n")
          (dolist (timing slow-modules)
            (insert (format "  %s: %.3fs\n" (car timing) (cdr timing)))))))
    
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

(defun my/get-startup-time ()
  \"Get configuration load time in seconds.\"
  my/config-total-load-time)

(defun my/format-startup-time ()
  \"Format startup time for display.\"
  (let ((load-time (my/get-startup-time)))
    (cond
     ((null load-time)
      "⚡ Ready")
     ((< load-time 1.0)
      (format "⚡ Loaded in %.0f ms" (* load-time 1000)))
     (t
      (format "⚡ Loaded in %.2f seconds" load-time)))))

(provide 'config-loader)
