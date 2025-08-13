;;; -*- lexical-binding: t; -*-


;; Keep Customize out of init.el

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))  ;; load quietly if present



(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Backups: don’t make ~ files for VC-managed files (Git, etc.)
(setq vc-make-backup-files nil)  ;; default, kept explicit for clarity

;; Load Org early so modules can rely on it
(require 'org)

(require 'org-basic)        ;; org dir, agenda, capture, editing niceties
(require 'reading-tracker)  ;; reading file + dashboard helpers
(require 'welcome)          ;; startup page (reuses reading-tracker)

;; Make sure the reading file is included in the agenda (idempotent)
(when (boundp 'my/org-reading-file)
  (setq org-agenda-files
        (delete-dups (cons my/org-reading-file org-agenda-files))))


;;; End of init.el
