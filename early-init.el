;;; early-init.el --- Early startup optimization -*- lexical-binding: t; -*-

;;; Commentary:
;; Performance optimizations and UI configuration loaded before package system.

;;; Code:

;; ============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; ============================================================================

(setq gc-cons-threshold (* 64 1024 1024)      ; 64 MB during startup
      read-process-output-max (* 4 1024 1024) ; 4 MB for LSP performance
      load-prefer-newer t)

;; Native compilation (Emacs 28+)
(when (fboundp 'native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; ============================================================================
;; PACKAGE SYSTEM
;; ============================================================================

(setq package-enable-at-startup nil)

;; ============================================================================
;; UI CONFIGURATION
;; ============================================================================

(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil)

;; Minimal interface
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

(provide 'early-init)
;;; early-init.el ends here

