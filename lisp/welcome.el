;;; welcome.el --- Simple entry point for the dashboard system -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'dashboard-core)      ;; Core dashboard functionality
(require 'reading-dashboard)   ;; Reading progress component
(require 'time-dashboard)      ;; Time tracking component

;; ============================================================================
;; STARTUP CONFIGURATION
;; ============================================================================

;; Don't show the GNU splash or a scratch message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; ============================================================================
;; PUBLIC INTERFACE
;; ============================================================================

;; Provide simple aliases for the main dashboard functions
(defalias 'my/cheatsheet-show 'dashboard-show
  "Display the personal workspace dashboard.")

(defalias 'my/cheatsheet-refresh 'dashboard-refresh
  "Refresh the personal workspace dashboard.")

(defalias 'my/cheatsheet-mode 'dashboard-mode
  "Dashboard mode for the personal workspace.")

;; ============================================================================
;; STARTUP INTEGRATION
;; ============================================================================

;; Show welcome page on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Ensure startup time is calculated before showing dashboard
            (when (fboundp 'dashboard--calculate-startup-time)
              (dashboard--calculate-startup-time))
            (dashboard-show)
            (delete-other-windows)))  ;; Ensure a single window

;; Keybindings are now centralized in keybindings.el

(provide 'welcome)
