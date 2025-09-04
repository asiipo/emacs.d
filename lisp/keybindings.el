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

(global-set-key (kbd "C-c r o") #'my/org-reading-open)
(global-set-key (kbd "C-c r b") #'my/org-reading-open-books)
(global-set-key (kbd "C-c r n") #'my/org-reading-open-book-notes)
(global-set-key (kbd "C-c r a") #'my/org-reading-add-book)
(global-set-key (kbd "C-c r u") #'my/org-reading-set-current-page)
(global-set-key (kbd "C-c r c") #'my/org-reading-complete-book)
(global-set-key (kbd "C-c r d") #'my/org-reading-delete-book)
(global-set-key (kbd "C-c r D") #'my/org-reading-set-deadline)

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

(global-set-key (kbd "C-c d") (lambda () (interactive) (dired org-directory)))
(global-set-key (kbd "C-c e") (lambda () (interactive) (dired user-emacs-directory)))
(global-set-key (kbd "C-c h") #'my/cheatsheet-show)

;; ============================================================================
;; SPELL CHECKING KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "M-$") #'jinx-correct)              ;; Correct word at point
(global-set-key (kbd "C-M-$") #'jinx-languages)          ;; Switch languages
(global-set-key (kbd "C-c s c") #'jinx-correct)          ;; Easier alternative
(global-set-key (kbd "C-c s l") #'jinx-languages)        ;; Language switching
(global-set-key (kbd "C-c s b") #'my/jinx-correct-buffer) ;; Correct entire buffer

;; ============================================================================
;; JOURNAL KEYBINDINGS
;; ============================================================================

(global-set-key (kbd "C-c j") #'my/journal-capture-today)

(provide 'keybindings)
