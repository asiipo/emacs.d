;;; org-roam-config.el --- Org-roam v2 configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; Org-roam with bibliography integration and visualization.

(eval-when-compile
  (declare-function orb--new-note "org-roam-bibtex"))

(require 'seq)
(require 'subr-x)

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (file-truename "~/org/resources/roam"))
  (org-roam-db-location (file-truename "~/org/resources/roam/org-roam.db"))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template "${title}")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: \n")
      :unnarrowed t)))
  
  :config
  (org-roam-db-autosync-mode)

  (dolist (subdir '("main" "article"))
    (make-directory (expand-file-name subdir org-roam-directory) t))
  
  (defun my/org-roam-open-directory ()
    "Open org-roam directory in dired."
    (interactive)
    (dired org-roam-directory))
  
  ;; Tag-aware helpers
  (defun my/org-roam--all-tags ()
    "Return a list of all tags in the Org-roam database."
    (seq-uniq
     (seq-filter (lambda (tag) (and tag (not (string-empty-p tag))))
                 (seq-mapcat #'org-roam-node-tags (org-roam-node-list)))))

  (defun my/org-roam-find-by-tag (tag)
    "Find an Org-roam node filtered by TAG."
    (interactive (list (completing-read "Roam tag: " (my/org-roam--all-tags) nil t)))
    (org-roam-node-find nil nil
                        (lambda (node)
                          (member tag (org-roam-node-tags node))))))

;; Org-Roam-Bibtex Integration

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :custom
  (orb-roam-ref-format 'org-cite)
  (orb-preformat-keywords '("citekey" "title" "author" "year" "url" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf"))
  (bibtex-completion-bibliography 
   (list (expand-file-name "zotero-library.bib" org-roam-directory)))
  (orb-insert-link-description "${author-abbrev} (${year})")
  :config
  (org-roam-bibtex-mode)

  (defvar my/orb-roam-capture-template
    '(("r" "reference" plain "%?"
       :if-new (file+head "article/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+author: ${author}\n#+filetags: :resource:article: \n")
       :unnarrowed t))
    "Capture template for bibliography-driven notes.")

  (defun my/orb--use-reference-template (orig-fn &rest args)
    "Use bibliography-specific template for ORB captures."
    (let ((org-roam-capture-templates my/orb-roam-capture-template))
      (apply orig-fn args)))

  (unless (advice-member-p #'my/orb--use-reference-template 'orb--new-note)
    (advice-add 'orb--new-note :around #'my/orb--use-reference-template)))

;; Org-Roam-UI Visualization

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; Display Configuration
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(provide 'org-roam-config)
;;; org-roam-config.el ends here