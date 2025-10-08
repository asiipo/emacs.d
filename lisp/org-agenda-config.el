;;; org-agenda-config.el --- Agenda display and behavior -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures Org Agenda with custom commands, display settings,
;; and keybindings for an efficient task management workflow.

;; ============================================================================
;; AGENDA INITIALIZATION
;; ============================================================================

(with-eval-after-load 'org-agenda
  ;; Enable habits for tracking recurring tasks
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  
  ;; ============================================================================
  ;; AGENDA DISPLAY SETTINGS
  ;; ============================================================================
  
  ;; Basic agenda configuration
  (setq org-agenda-span 'week                    ;; Show week view by default
        org-agenda-start-on-weekday nil          ;; Start on current day
        org-deadline-warning-days 7              ;; Warn 7 days before deadline
        org-agenda-compact-blocks t              ;; Compact agenda display
        org-agenda-window-setup 'current-window  ;; Use current window
        org-agenda-format-date-aligned t         ;; Align dates nicely
        org-agenda-show-future-repeats 'next     ;; Show next repeat only
        org-agenda-dim-blocked-tasks t           ;; Dim blocked tasks
        org-agenda-use-time-grid t               ;; Show time grid
        org-agenda-time-grid '((daily today require-timed)
                              (800 1000 1200 1400 1600 1800 2000)
                              "......" "----------------")
        org-agenda-todo-ignore-with-date nil     ;; Show todos with dates
        org-agenda-tags-todo-honor-ignore-options t)

  ;; ============================================================================
  ;; CUSTOM AGENDA COMMANDS
  ;; ============================================================================
  
  ;; Custom agenda views for different workflows
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and next actions"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-show-all-dates nil)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-max-entries 10)))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting For")
                          (org-agenda-max-entries 5)))))
          
          ("w" "Weekly review"
           ((agenda "" ((org-agenda-span 7)))
            (todo "NEXT" ((org-agenda-overriding-header "All Next Actions")))
            (tags-todo "CATEGORY=\"Project\"" ((org-agenda-overriding-header "All Projects")))))
          
          ("p" "Projects overview" tags-todo "CATEGORY=\"Project\""
           ((org-agenda-overriding-header "All Projects")))
          
          ("n" "Next actions" todo "NEXT"
           ((org-agenda-overriding-header "Next Actions")))))
  
  ;; ============================================================================
  ;; AGENDA KEYBINDINGS
  ;; ============================================================================
  
  ;; Enhanced agenda keybindings for workflow integration
  (define-key org-agenda-mode-map (kbd "i") #'my/goto-inbox)
  (define-key org-agenda-mode-map (kbd "r") #'org-agenda-redo-all))

(provide 'org-agenda-config)

