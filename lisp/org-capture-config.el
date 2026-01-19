;;; org-capture-config.el --- Capture inbox and templates -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures org-capture with PARA-method templates for efficient
;; task and content capture into proper organizational structure.

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)

;; ============================================================================
;; INBOX CONFIGURATION
;; ============================================================================

;; Set inbox as the default capture target
;; Based on Org Mode Guide section 9.1
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Create inbox file if it doesn't exist
(unless (file-exists-p org-default-notes-file)
  (with-temp-file org-default-notes-file
    (insert "#+TITLE: Inbox\n")))

;; ============================================================================
;; SOMEDAY/MAYBE CONFIGURATION
;; ============================================================================

;; Set up someday file for future ideas and non-urgent tasks
(defvar my/someday-file
  (expand-file-name "someday.org" org-directory)
  "File for someday/maybe tasks and ideas.")

;; Create someday file if it doesn't exist
(unless (file-exists-p my/someday-file)
  (with-temp-file my/someday-file
    (insert "#+TITLE: Someday/Maybe\n\n"
            "* Ideas\n"
            "Tasks and ideas without a specific timeline.\n"
            "Review these during weekly planning.\n\n"
            "* To Explore\n"
            "Things to learn or investigate when time permits.\n\n"
            "* Future Projects\n"
            "Project ideas for later.\n\n")))

;; ============================================================================
;; CAPTURE TEMPLATES
;; ============================================================================

;; PARA-method capture templates for comprehensive content organization
(setq org-capture-templates
      `(;; ========== INBOX TEMPLATES ==========
        
        ;; Task: Quick TODO item to inbox
        ("t" "Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nCREATED: %U\n%i\n")
        
        ;; Note: Tagged note to inbox
        ("n" "Note" entry 
         (file ,org-default-notes-file)
         "* %^{Note title} :%^{tags|work|home|research|admin|deep|quick|:}:\nCREATED: %U\n\n%?\n%i\n")

        ;; Scheduled task: Task with specific date/time
        ("s" "Scheduled Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nSCHEDULED: %^T\nCREATED: %U\n%i\n")

        ;; Task with deadline
        ("d" "Task with Deadline" entry
         (file ,org-default-notes-file)
         "* TODO %?\nDEADLINE: %^T\nCREATED: %U\n%i\n")

        ;; Someday/Maybe: Ideas and future tasks
        ("S" "Someday/Maybe" entry
         (file+headline my/someday-file "Ideas")
         "* %? :someday:\nCREATED: %U\n%i\n")))

;; ============================================================================
;; KEYBINDINGS AND SETTINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

;; Enable speed commands for faster Org navigation
;; Based on Org Mode Guide section 2.1
(setq org-use-speed-commands t)


;; ---------------------------------------------------------------------------
;; GTD capture: Removed from org-capture-templates
;; Use C-c G keybinding instead (defined in keybindings.el)
;; ---------------------------------------------------------------------------

(provide 'org-capture-config)

