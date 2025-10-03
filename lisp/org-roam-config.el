;;; org-roam-config.el --- Org-roam v2 configuration -*- lexical-binding: t; -*-

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/resources/roam"))
  ;; Database location - keep it WITH your roam files, not in .emacs.d
  (org-roam-db-location (file-truename "~/org/resources/roam/org-roam.db"))
  ;; Dailies directory (relative to org-roam-directory)
  (org-roam-dailies-directory "daily/")
  ;; Enable completion everywhere for easy linking
  (org-roam-completion-everywhere t)
  ;; Capture templates for regular nodes
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     
     ("r" "reference" plain "%?"
      :if-new (file+head "ref/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :reference:\n")
      :unnarrowed t)
     
     ("b" "book" plain "* Notes\n\n* Review\n\n"
      :if-new (file+head "books/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+author: %^{Author}\n#+year: %^{Year}\n#+rating: %^{Rating (1-5)|3}\n#+filetags: :book:reference:\n")
      :unnarrowed t)))
  
  ;; Capture templates for dailies
  (org-roam-dailies-capture-templates
   '(("d" "default" plain 
      "* %?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)))
  
  :config
  ;; Enable autosync mode to keep database up-to-date
  (org-roam-db-autosync-mode)
  
  ;; Helper function to open roam directory
  (defun my/org-roam-open-directory ()
    "Open org-roam directory in dired."
    (interactive)
    (dired org-roam-directory))
  
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n c" . org-roam-capture)  ; 'c' for capture new node
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . my/org-roam-open-directory)  ; 'o' for open roam folder
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph)  ; 'g' for graph
         ("C-c n u" . org-roam-ui-mode) ; 'u' for UI (interactive web visualization)
         ;; Dailies
         ("C-c n d d" . org-roam-dailies-capture-today)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n d m" . org-roam-dailies-goto-tomorrow)
         ("C-c n d c" . org-roam-dailies-goto-date)))

;; ============================================================================
;; Org-roam-bibtex for bibliography management
;; ============================================================================
(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :custom
  ;; Use org-cite format for references (@citekey)
  (orb-roam-ref-format 'org-cite)
  ;; Keywords to preformat in templates
  (orb-preformat-keywords '("citekey" "title" "author" "year" "url" "file"))
  ;; Process file keyword for PDF attachments
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))
  ;; Set bibliography file (will be populated by Zotero)
  (bibtex-completion-bibliography 
   (list (expand-file-name "zotero-library.bib" org-roam-directory)))
  :config
  (org-roam-bibtex-mode))

;; ============================================================================
;; Org-roam-ui for interactive graph visualization
;; ============================================================================
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)          ; Sync with Emacs theme
  (org-roam-ui-follow t)              ; Follow node in UI
  (org-roam-ui-update-on-save t)      ; Update graph on save
  (org-roam-ui-open-on-start t))      ; Open browser on start


;; ============================================================================
;; Display configuration for backlinks sidebar
;; ============================================================================
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(provide 'org-roam-config)
;;; org-roam-config.el ends here