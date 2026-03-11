;;; keybindings.el --- Centralized keybinding configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Global keybindings for all modules.

;;; Code:

;; Org Mode Keybindings

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c i") #'my/goto-inbox)
(global-set-key (kbd "C-c s") #'my/goto-someday)
(global-set-key (kbd "C-c A") #'my/refresh-agenda)
(global-set-key (kbd "C-c C-w") #'org-refile)
(global-set-key (kbd "C-c c") #'org-capture)

;; GTD Menu - Interactive menu for GTD workflow
;; C-c G → menu: [t]oday, [T]omorrow, [w]eek, [l]ast week, [r]eports
(global-set-key (kbd "C-c G") #'my/gtd-open)

;; GTD KEYBINDINGS

;; Main GTD menu (recommended entry point)
;; C-c G                → Interactive menu with all options
;;   t                  → Create/jump to today's headline
;;   T                  → Create/jump to tomorrow's headline  
;;   w                  → Weekly review (show current week)
;;   l                  → Last week review (show last week)
;;   r                  → Time reports submenu:
;;     w                → Week time report with tag aggregation
;;     m                → Month time report with tag aggregation

;; Direct access (if needed, but menu is recommended)
;; (global-set-key (kbd "C-c G t") #'my/gtd-insert-today)
;; (global-set-key (kbd "C-c G w") #'my/gtd-weekly-review)

;; Note: For archiving, use Org mode's built-in commands:
;;   C-c C-x C-s        → Archive subtree (org-archive-subtree)
;;   C-c C-x a          → Archive dispatcher menu

;; ORG-ROAM KEYBINDINGS

(global-set-key (kbd "C-c n f") #'org-roam-node-find)
(global-set-key (kbd "C-c n c") #'org-roam-capture)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n o") #'my/org-roam-open-directory)
(global-set-key (kbd "C-c n b") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n t") #'my/org-roam-find-by-tag)
(global-set-key (kbd "C-c n u") #'org-roam-ui-mode)

;; READING TRACKER KEYBINDINGS

(global-set-key (kbd "C-c r o") #'my/reading-open)          ;; Open reading file
(global-set-key (kbd "C-c r a") #'my/reading-add-book)      ;; Add book
(global-set-key (kbd "C-c r u") #'my/reading-update-progress) ;; Update progress
(global-set-key (kbd "C-c r c") #'my/reading-complete-book) ;; Complete book
(global-set-key (kbd "C-c r d") #'my/reading-set-deadline)  ;; Set deadline

;; ZOTERO/BIBTEX KEYBINDINGS

(global-set-key (kbd "C-c z c") #'my/zotero-check-bib)         ;; Check bibliography status
(global-set-key (kbd "C-c z o") #'my/zotero-open-bib-file)     ;; Open bibliography file
(global-set-key (kbd "C-c z i") #'orb-insert-link)             ;; Insert bibliography link

;; GIT/MAGIT KEYBINDINGS

(global-set-key (kbd "C-c g g") #'my/magit-org-status)
(global-set-key (kbd "C-c g s") #'my/org-sync-now)
(global-set-key (kbd "C-c g t") #'my/org-toggle-auto-sync)
(global-set-key (kbd "C-x g") #'magit-status)

;; WORKSPACE NAVIGATION KEYBINDINGS

(defun my/open-org-directory ()
  "Open org directory in dired."
  (interactive)
  (dired org-directory))

(defun my/open-emacs-directory ()
  "Open Emacs configuration directory in dired."
  (interactive)
  (dired user-emacs-directory))

(global-set-key (kbd "C-c d") #'my/open-org-directory)
(global-set-key (kbd "C-c e") #'my/open-emacs-directory)
(global-set-key (kbd "C-c h") #'dashboard-show)

;; SPELL CHECKING KEYBINDINGS

(global-set-key (kbd "C-;") #'jinx-correct)     ;; Correct word at point
(global-set-key (kbd "C-M-;") #'jinx-languages) ;; Switch languages

;; JOURNAL KEYBINDINGS

(global-set-key (kbd "C-c J") #'my/journal-capture-today)

;; JULIA DEVELOPMENT KEYBINDINGS

(global-set-key (kbd "C-c j r") #'julia-repl)                    ;; Start Julia REPL
(global-set-key (kbd "C-c j b") #'julia-repl-send-buffer)        ;; Send buffer to REPL
(global-set-key (kbd "C-c j l") #'julia-repl-send-line)          ;; Send line to REPL
(global-set-key (kbd "C-c j d") #'eldoc-doc-buffer)              ;; Show documentation
(global-set-key (kbd "C-c j g") #'xref-find-definitions)         ;; Jump to definition
(global-set-key (kbd "C-c j n") #'eglot-rename)                  ;; Rename symbol

;; LLM ASSISTANT KEYBINDINGS (disabled)

;; (global-set-key (kbd "C-c l b") #'my/daily-briefing)         ;; Generate daily briefing
;; (global-set-key (kbd "C-c l w") #'my/llm-quick-win)           ;; Quick win (30-min window)
;; (global-set-key (kbd "C-c l c") #'gptel)                     ;; Open LLM chat buffer
;; (global-set-key (kbd "C-c l p") #'my/llm-open-prompts)        ;; Open prompts directory

;; ADDITIONAL UTILITY KEYBINDINGS

;; Quick access to configuration diagnosis
(global-set-key (kbd "C-c D") #'my/diagnose-config)      ;; Capital D for Diagnose
(global-set-key (kbd "C-c T") #'my/list-config-timing)   ;; Capital T for Timing

(provide 'keybindings)
