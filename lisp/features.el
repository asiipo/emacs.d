;;; features.el --- Feature toggle system -*- lexical-binding: t; -*-

;; Author: Personal Configuration  
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: configuration, features, toggles

;; This module provides a feature toggle system allowing users to enable/disable
;; configuration components without editing code.

;; ============================================================================
;; FEATURE TOGGLES
;; ============================================================================

(defcustom my/features-enabled
  '(org-mode              ;; Org-mode and PARA methodology
    reading-tracker       ;; Book reading progress tracking
    time-tracking         ;; Clock table and time logging
    git-integration       ;; Magit and auto-sync
    spell-checking        ;; Modern spell checking with Jinx
    journal              ;; Daily journaling
    dashboard            ;; Welcome screen and productivity dashboard
    startup-timing)       ;; Performance monitoring
  "List of features to enable in the configuration.
Each symbol corresponds to a feature that can be toggled on/off."
  :type '(repeat (choice
                  (const :tag "Org Mode & PARA" org-mode)
                  (const :tag "Reading Tracker" reading-tracker)  
                  (const :tag "Time Tracking" time-tracking)
                  (const :tag "Git Integration" git-integration)
                  (const :tag "Spell Checking" spell-checking)
                  (const :tag "Journaling" journal)
                  (const :tag "Dashboard" dashboard)
                  (const :tag "Startup Timing" startup-timing)))
  :group 'my-config)

(defun my/feature-enabled-p (feature)
  "Check if FEATURE is enabled."
  (memq feature my/features-enabled))

(defmacro my/when-feature (feature &rest body)
  "Execute BODY when FEATURE is enabled."
  `(when (my/feature-enabled-p ',feature)
     ,@body))

(defun my/toggle-feature (feature)
  "Toggle FEATURE on or off."
  (interactive 
   (list (intern (completing-read "Toggle feature: "
                                  '(org-mode reading-tracker time-tracking 
                                    git-integration spell-checking journal 
                                    dashboard startup-timing)))))
  (if (my/feature-enabled-p feature)
      (progn
        (setq my/features-enabled (delq feature my/features-enabled))
        (message "Disabled feature: %s" feature))
    (progn
      (add-to-list 'my/features-enabled feature)
      (message "Enabled feature: %s" feature)))
  
  (message "Restart Emacs for changes to take effect."))

(defun my/list-features ()
  "Show current feature status."
  (interactive)
  (message "Enabled features: %s" my/features-enabled))

(provide 'features)
