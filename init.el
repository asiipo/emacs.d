;;; -*- lexical-binding: t; -*-
;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; CUSTOMIZATION AND PACKAGES
;; ============================================================================

;; Add our lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Dashboard is now consolidated in lisp/dashboard.el

;; Initialize package system and install required packages FIRST
(require 'packages)

;; THEN load custom.el after packages are available
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))  ;; load quietly if present

;; ============================================================================
;; ORG MODE SETUP
;; ============================================================================

;; Central Org location for all modules (see org-basic.el for usage)
(defvar org-directory (expand-file-name "~/org")
  "Base directory for all Org files and PARA structure.")

;; Version control: don't create backup files for Git-managed files
(setq vc-make-backup-files nil)

;; Load Org early so all modules can rely on it
(require 'org)

;; ============================================================================
;; MODULE LOADING
;; ============================================================================

;; Load the centralized config loader first
(require 'config-loader)

;; Use the centralized config loader for better error handling  
(unless (my/load-all-config-modules)
  (message "⚠️  Some modules failed to load. Run M-x my/diagnose-config for details."))

(when (eq system-type 'darwin)
  ;; On macOS, use left Option as Meta, disable right Option as Meta
  (setq mac-option-modifier 'meta
        mac-right-option-modifier nil))

;; ============================================================================
;; END OF INIT.EL
;; ============================================================================
