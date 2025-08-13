;;; core-init.el --- Backups, autosaves, editing defaults -*- lexical-binding: t; -*-

;; ============================================================================
;; BACKUP AND AUTO-SAVE CONFIGURATION
;; ============================================================================

;; Configure backup and auto-save directories under ~/.emacs.d/var/
(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (backup-dir (expand-file-name "backups/" var-dir))
       (autosave-dir (expand-file-name "auto-saves/" var-dir)))
  ;; Create directories if they don't exist
  (dolist (d (list backup-dir autosave-dir)) 
    (make-directory d t))
  
  ;; Set backup and auto-save behavior
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        create-lockfiles nil
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        version-control t))

;; ============================================================================
;; EDITING DEFAULTS
;; ============================================================================

;; Sensible editing defaults for better workflow
(setq sentence-end-double-space nil      ;; Don't require double space after sentences
      require-final-newline t            ;; Always end files with newline
      confirm-kill-emacs nil             ;; Don't ask when quitting
      confirm-kill-processes nil)        ;; Don't ask when killing processes

(provide 'core-init)

