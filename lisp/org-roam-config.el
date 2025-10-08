;;; org-roam-config.el --- Org-roam v2 configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures Org-roam v2 with bibliography integration (org-roam-bibtex)
;; and interactive visualization (org-roam-ui) for building a knowledge management system.

(eval-when-compile
  (declare-function orb--new-note "org-roam-bibtex"))

(use-package org-roam
  :ensure t
  :custom
  ;; Directory configuration - keep database WITH roam files
  (org-roam-directory (file-truename "~/org/resources/roam"))
  (org-roam-db-location (file-truename "~/org/resources/roam/org-roam.db"))
  (org-roam-completion-everywhere t)
  
  ;; Simple display template - just show the title
  (org-roam-node-display-template "${title}")
  
  ;; Default capture template for general notes
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: \n")
      :unnarrowed t)))
  
  :config
  ;; Enable autosync mode to keep database up-to-date
  (org-roam-db-autosync-mode)

  ;; Ensure capture subdirectories exist
  (dolist (subdir '("main" "article"))
    (make-directory (expand-file-name subdir org-roam-directory) t))
  
  ;; Helper function to open roam directory
  (defun my/org-roam-open-directory ()
    "Open org-roam directory in dired."
    (interactive)
    (dired org-roam-directory))
  
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . my/org-roam-open-directory)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n u" . org-roam-ui-mode)))

;; ============================================================================
;; ORG-ROAM-BIBTEX INTEGRATION
;; ============================================================================

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :custom
  ;; Bibliography configuration
  (orb-roam-ref-format 'org-cite)
  (orb-preformat-keywords '("citekey" "title" "author" "year" "url" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))
  (bibtex-completion-bibliography 
   (list (expand-file-name "zotero-library.bib" org-roam-directory)))
  (orb-insert-link-description "${author-abbrev} (${year})")
  :config
  (org-roam-bibtex-mode)

  ;; Bibliography-specific capture template
  (defvar my/orb-roam-capture-template
    '(("r" "reference" plain "%?"
       :if-new (file+head "article/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+author: ${author}\n#+filetags: :resource:article: \n")
       :unnarrowed t))
    "Capture template for bibliography-driven notes.")

  ;; Ensure ORB uses the bibliography template
  (defun my/orb--use-reference-template (orig-fn &rest args)
    "Use bibliography-specific template for ORB captures."
    (let ((org-roam-capture-templates my/orb-roam-capture-template))
      (apply orig-fn args)))

  (unless (advice-member-p #'my/orb--use-reference-template 'orb--new-note)
    (advice-add 'orb--new-note :around #'my/orb--use-reference-template)))

;; ============================================================================
;; ORG-ROAM-UI VISUALIZATION
;; ============================================================================

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; ============================================================================
;; DISPLAY CONFIGURATION
;; ============================================================================

;; Configure backlinks sidebar display
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(provide 'org-roam-config)
;;; org-roam-config.el ends here