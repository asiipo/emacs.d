;;; theme-config.el --- Custom face configurations for doom-dracula -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides custom face configurations to enhance the visual
;; appearance of org-mode and org-agenda with the doom-dracula theme.
;; Inspired by modern, minimal org-agenda aesthetics with proper spacing,
;; typography, and visual hierarchy.

;; ============================================================================
;; DOOM-DRACULA THEME CUSTOMIZATIONS
;; ============================================================================

(with-eval-after-load 'doom-themes
  (custom-theme-set-faces
   'doom-dracula
   
   ;; -------------------------------------------------------------------------
   ;; Org-Agenda Date Headers - Boxed with padding for visual separation
   ;; -------------------------------------------------------------------------
   '(org-agenda-date ((t (:weight semibold
                          :slant normal
                          :box (:line-width 7 :color "#282a36" :style nil)))))
   
   '(org-agenda-date-weekend ((t (:weight semibold
                                   :slant normal
                                   :box (:line-width 7 :color "#282a36" :style nil)))))
   
   ;; Today's date - bold and highlighted
   '(org-agenda-date-today ((t (:weight bold
                                 :slant normal
                                 :box (:line-width 7 :color "#44475a" :style nil)))))
   
   ;; -------------------------------------------------------------------------
   ;; Org-Agenda Items - Clean, minimal appearance
   ;; -------------------------------------------------------------------------
   ;; Lightweight calendar events and scheduled items
   '(org-agenda-calendar-event ((t (:weight light))))
   '(org-scheduled ((t (:weight light))))
   '(org-scheduled-today ((t (:weight light))))
   '(org-agenda-calendar-sexp ((t (:weight light))))
   '(org-scheduled-previously ((t (:weight light))))
   
   ;; Completed items - subdued with strike-through
   '(org-agenda-done ((t (:foreground "#6272a4"
                          :weight light
                          :strike-through "#44475a"))))
   
   ;; Section headers - larger, bold, boxed
   '(org-agenda-structure ((t (:height 1.4
                                :weight bold
                                :box (:line-width 2 :color "#282a36" :style nil)))))
   
   ;; -------------------------------------------------------------------------
   ;; Org-Mode Headline Levels - Clear visual hierarchy
   ;; -------------------------------------------------------------------------
   '(org-level-1 ((t (:foreground "#ff79c6"
                      :weight semibold
                      :height 1.3))))
   
   '(org-level-2 ((t (:foreground "#bd93f9"
                      :weight normal
                      :height 1.15))))
   
   '(org-level-3 ((t (:foreground "#8be9fd"
                      :weight normal
                      :height 1.05))))
   
   '(org-level-4 ((t (:foreground "#50fa7b"
                      :weight normal))))
   
   '(org-level-5 ((t (:foreground "#50fa7b"
                      :weight normal))))
   
   '(org-level-6 ((t (:foreground "#50fa7b"
                      :weight normal))))
   
   ;; -------------------------------------------------------------------------
   ;; Org-Mode Document Elements
   ;; -------------------------------------------------------------------------
   '(org-document-title ((t (:foreground "#ff79c6"
                             :height 1.6
                             :weight bold))))
   
   '(org-link ((t (:foreground "#8be9fd"
                   :weight normal
                   :underline t))))
   
   ;; Subdued metadata
   '(org-drawer ((t (:foreground "#6272a4"))))
   '(org-meta-line ((t (:foreground "#6272a4"))))
   '(org-headline-done ((t (:foreground "#6272a4"))))
   
   ;; Tags - blend with background
   '(org-tag ((t (:foreground "#44475a"))))
   
   ;; -------------------------------------------------------------------------
   ;; Org-Code Blocks - Subtle background differentiation
   ;; -------------------------------------------------------------------------
   '(org-block-begin-line ((t (:foreground "#6272a4"
                                :background "#1e2029"
                                :extend t))))
   
   '(org-block-end-line ((t (:foreground "#6272a4"
                              :background "#1e2029"
                              :extend t))))
   
   '(org-ellipsis ((t (:foreground "#6272a4"
                       :height 1.0))))))

;; ============================================================================
;; ORG-HABIT CONFIGURATION
;; ============================================================================

(with-eval-after-load 'org-habit
  (setq org-habit-graph-column 50
        org-habit-following-days 1
        org-habit-preceding-days 10
        org-habit-show-habits-only-for-today t))

(provide 'theme-config)
;;; theme-config.el ends here
