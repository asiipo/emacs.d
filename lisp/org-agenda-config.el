;;; org-agenda-config.el --- Agenda display and behavior -*- lexical-binding: t; -*-
;;; Commentary:
;; Org Agenda custom commands, display settings, and keybindings.

(with-eval-after-load 'org-agenda
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  
  ;; Agenda Display Settings
  (setq org-agenda-span 'week
        org-agenda-start-on-weekday nil
        org-deadline-warning-days 7
        org-agenda-compact-blocks t
        org-agenda-window-setup 'current-window
        org-agenda-format-date-aligned t
        org-agenda-show-future-repeats 'next
        org-agenda-dim-blocked-tasks t
        org-agenda-use-time-grid t
        org-agenda-time-grid '((daily today require-timed remove-match)
                              (600 900 1200 1500 1800 2100)
                              "      "
                              "┈┈┈┈┈┈┈┈┈┈┈┈┈")
        org-agenda-todo-ignore-with-date nil
        org-agenda-tags-todo-honor-ignore-options t
        org-agenda-breadcrumbs-separator " ❱ "
        org-agenda-block-separator (string-to-char " ")
        org-agenda-hidden-separator "‌‌ "
        org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
        org-agenda-scheduled-leaders '("" "")
        org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))

  ;; Custom Agenda Commands
  (setq org-agenda-custom-commands
        '(("d" "Daily agenda"
           ((todo "TODO" ((org-agenda-overriding-header "\n⚡ Do Today")
                          (org-agenda-sorting-strategy '(priority-down))
                          (org-agenda-remove-tags t)
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))
                          (org-agenda-prefix-format "   %-2i ")
                          (org-agenda-todo-keyword-format "")))
            (agenda "" ((org-agenda-span 5)
                        (org-agenda-show-all-dates t)
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-skip-timestamp-if-done t)
                        (org-agenda-time-leading-zero t)
                        (org-agenda-timegrid-use-ampm nil)
                        (org-agenda-overriding-header "\n⚡ Schedule")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        (org-agenda-todo-keyword-format "")))))
          
          ("w" "Weekly overview"
           ((todo "TODO" ((org-agenda-overriding-header "\n⚡ Do Today")
                          (org-agenda-sorting-strategy '(priority-down))
                          (org-agenda-remove-tags t)
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))
                          (org-agenda-prefix-format "   %-2i ")
                          (org-agenda-todo-keyword-format "")))
            (agenda "" ((org-agenda-span 7)
                        (org-agenda-skip-scheduled-if-done nil)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-overriding-header "\n⚡ Week Schedule")
                        (org-agenda-repeating-timestamp-show-all nil)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "   %i %?-2 t%s")
                        (org-agenda-todo-keyword-format "")))))
          
          ("t" "All TODO items" todo "TODO"
           ((org-agenda-overriding-header "\n⚡ All To Do Items")
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":STYLE:.*habit"))
            (org-agenda-remove-tags t)
            (org-agenda-prefix-format "   %-2i ")
            (org-agenda-todo-keyword-format "")))
          
          ("n" "Next actions" todo "NEXT"
           ((org-agenda-overriding-header "\n⚡ Next Actions")
            (org-agenda-remove-tags t)
            (org-agenda-prefix-format "   %-2i ")
            (org-agenda-todo-keyword-format ""))))) ; Close setq
  
  ;; Visual Enhancement Hooks
  
  (defun my/agenda-color-emoji ()
    "Enhance ⚡ emoji in agenda headers with larger size and gold color."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "⚡" nil t)
        (put-text-property (match-beginning 0) (match-end 0)
                           'face '(:height 1.0 :foreground "gold2" :weight bold)))))
  
  (defun my/set-agenda-window-clean ()
    "Apply minimal aesthetic to agenda window."
    (setq line-spacing 2)                ;; Breathing room between lines
    (my/agenda-color-emoji)              ;; Style emoji headers
    (setq mode-line-format nil)          ;; Hide mode line
    (setq header-line-format " ")        ;; Minimal header
    (set-face-attribute 'header-line nil :background "#282a36")  ;; Match dracula bg
    (set-window-margins (selected-window) 4))
  
  (add-hook 'org-agenda-finalize-hook #'my/set-agenda-window-clean)
  
  ;; Agenda Keybindings
  (define-key org-agenda-mode-map (kbd "i") #'my/goto-inbox)
  (define-key org-agenda-mode-map (kbd "r") #'org-agenda-redo-all)) ; Close with-eval-after-load

(provide 'org-agenda-config)

