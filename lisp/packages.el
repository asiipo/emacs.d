;;; packages.el --- Minimal package setup -*- lexical-binding: t; -*-

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

;; Ensure Magit is installed for Git integration
;; Respects Customize settings if already configured
(unless (package-installed-p 'magit)
  (ignore-errors
    (package-install 'magit)))

(provide 'packages) 