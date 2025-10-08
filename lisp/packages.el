;;; packages.el --- Minimal package setup -*- lexical-binding: t; -*-

;;; Commentary:
;; This module initializes the package system and ensures essential packages
;; are installed for the configuration to work properly.

;; ============================================================================
;; PACKAGE SYSTEM INITIALIZATION
;; ============================================================================

(require 'package)

;; Add MELPA repository (GNU ELPA is included by default)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package.el if not already done
(unless package--initialized
  (package-initialize))

;; Refresh package archives on first run if needed
(unless package-archive-contents
  (package-refresh-contents))

;; ============================================================================
;; ESSENTIAL PACKAGES
;; ============================================================================

;; Helper function to install packages with error handling
(defun my/ensure-package-installed (package &optional note)
  "Ensure PACKAGE is installed, with optional installation NOTE."
  (unless (package-installed-p package)
    (condition-case err
        (progn
          (message "Installing %s..." package)
          (package-install package)
          (message "%s installed successfully" package)
          (when note (message "Note: %s" note)))
      (error
       (message "Warning: Failed to install %s: %s" package (error-message-string err))
       (when note (message "Note: %s" note))))))

;; Essential packages for the configuration
(defvar my/essential-packages
  '((use-package . "Required for module configuration macros")
    (magit . "Git integration and version control")
    (doom-themes . "Theme collection including doom-dracula")
    (jinx . "Modern spell checking (requires: brew install enchant)")
    (org-roam-bibtex . "Bibliography management for org-roam"))
  "List of essential packages with descriptions.")

;; Install essential packages
(dolist (pkg-info my/essential-packages)
  (let ((package (car pkg-info))
        (note (cdr pkg-info)))
    (my/ensure-package-installed package note)))

;; Special setup for use-package
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'packages) 