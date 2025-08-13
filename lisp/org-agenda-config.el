;;; org-agenda-config.el --- Agenda polish and custom commands -*- lexical-binding: t; -*-

(with-eval-after-load 'org-agenda
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-agenda-span 'week
        org-agenda-start-on-weekday nil
        org-deadline-warning-days 7
        org-agenda-compact-blocks t
        org-agenda-window-setup 'current-window
        org-agenda-format-date-aligned t
        org-agenda-show-future-repeats 'next
        org-agenda-dim-blocked-tasks t
        org-agenda-prefix-format
        '((agenda  . "%-12:c %-5e %?-12t %s ")
          (todo    . "%-12:c %-5e ")
          (tags    . "%-12:c ")
          (search  . "%-12:c "))
        org-agenda-sorting-strategy
        '((agenda habit-down time-up deadline-up priority-down)
          (todo priority-down effort-up)
          (tags priority-down)
          (search category-keep))
        org-stuck-projects '("+project/-DONE-CANCELLED" ("NEXT") nil ""))

(setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
            (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High priority")))))))
)
(provide 'org-agenda-config)

