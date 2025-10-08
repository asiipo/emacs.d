;;; keybindings.el --- Centralized keybinding configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================

;; This file centralizes all global keybindings for better management and
;; to avoid conflicts. Load this after all other modules are loaded.

;; ============================================================================
;; ORG MODE KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c i") #'my/goto-inbox)
(global-set-key (kbd "C-c A") #'my/refresh-agenda)
(global-set-key (kbd "C-c C-w") #'org-refile)
(global-set-key (kbd "C-c c") #'org-capture)

;; ============================================================================
;; READING TRACKER KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c r o") #'my/reading-open)          ;; Open reading file
(global-set-key (kbd "C-c r a") #'my/reading-add-book)      ;; Add book
(global-set-key (kbd "C-c r u") #'my/reading-update-progress) ;; Update progress
(global-set-key (kbd "C-c r c") #'my/reading-complete-book) ;; Complete book
(global-set-key (kbd "C-c r d") #'my/reading-set-deadline)  ;; Set deadline

;; ============================================================================
;; ZOTERO/BIBTEX KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c z c") #'my/zotero-check-bib)         ;; Check bibliography status
(global-set-key (kbd "C-c z o") #'my/zotero-open-bib-file)     ;; Open bibliography file
(global-set-key (kbd "C-c z i") #'orb-insert-link)             ;; Insert bibliography link

;; ============================================================================
;; GIT/MAGIT KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c g g") #'my/magit-org-status)
(global-set-key (kbd "C-c g s") #'my/org-sync-now)
(global-set-key (kbd "C-c g t") #'my/org-toggle-auto-sync)
(global-set-key (kbd "C-x g") #'magit-status)

;; ============================================================================
;; WORKSPACE NAVIGATION KEYBINDINGS
;; ============================================================================

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

;; ============================================================================
;; SPELL CHECKING KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "M-$") #'jinx-correct)     ;; Correct word at point
(global-set-key (kbd "C-M-$") #'jinx-languages) ;; Switch languages
;; Note: C-c s * keybindings are defined in spell-checking.el

;; ============================================================================
;; JOURNAL KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c j") #'my/journal-capture-today)

;; ============================================================================
;; ORG LINK KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-l") #'org-insert-link)

;; ============================================================================
;; ADDITIONAL UTILITY KEYBINDINGS
;; ============================================================================

;; Quick access to configuration diagnosis
(global-set-key (kbd "C-c ? d") #'my/diagnose-config)
(global-set-key (kbd "C-c ? t") #'my/list-config-timing)

(provide 'keybindings)
