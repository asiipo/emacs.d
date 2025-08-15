;;; -*- lexical-binding: t; -*-
;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; CUSTOMIZATION AND PACKAGES
;; ============================================================================

;; Add our lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; Core system modules
(require 'core-init)           ;; backups, autosaves, editing defaults
(require 'org-basic)           ;; PARA structure, agenda, refile, archive
(require 'org-capture-config)  ;; capture templates and inbox workflow
(require 'org-agenda-config)   ;; agenda display and behavior

;; Specialized modules
(require 'reading-tracker)     ;; reading progress tracking
(require 'welcome)             ;; startup cheatsheet and reading dashboard
(require 'journal)             ;; daily journaling with datetree

;; External integrations
(require 'magit-config)        ;; Git integration
(require 'spell-checking)      ;; Modern spell checking with Jinx

;; ============================================================================
;; END OF INIT.EL
;; ============================================================================
