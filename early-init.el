;;; early-init.el --- Early startup optimization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before the package system and GUI is initialized.
;; Use it for performance optimizations and early UI configuration.

;;; Code:

;; ============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; ============================================================================

;; Increase garbage collection threshold during startup (64 MB)
;; This speeds up initialization by reducing GC pauses
(setq gc-cons-threshold (* 64 1024 1024)
      read-process-output-max (* 4 1024 1024)  ; 4 MB for better LSP performance
      load-prefer-newer t)                      ; Load newest .el or .elc file

;; Native compilation settings (Emacs 28+)
(when (fboundp 'native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))

;; ============================================================================
;; PACKAGE SYSTEM
;; ============================================================================

;; Disable package.el automatic loading - we handle it in init.el
(setq package-enable-at-startup nil)

;; ============================================================================
;; UI CONFIGURATION
;; ============================================================================

;; Prevent frame resize flicker during startup
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t          ; Skip default startup screen
      initial-scratch-message nil)      ; Empty *scratch* buffer

;; Remove GUI chrome for a clean, minimal interface
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Start with a comfortable window size (160x60 characters)
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 60))

;; Default font (adjust size as needed)
(add-to-list 'default-frame-alist '(font . "Monospace-11"))

;; ============================================================================
;; END OF EARLY-INIT.EL
;; ============================================================================

(provide 'early-init)
;;; early-init.el ends here

