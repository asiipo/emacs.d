;;; zotero-bibtex.el --- Minimal Zotero Better BibTeX integration -*- lexical-binding: t; -*-

;; Version: 6.0 - Streamlined
;; Author: arttusii
;; Description: Essential Zotero bibliography utilities for org-roam

;;; Commentary:
;;
;; Minimal Zotero integration for org-roam using Better BibTeX auto-export.
;; Provides bibliography file path and basic utilities.

;;; Code:

;; Bibliography file configuration
(defvar my/zotero-bib-file
  (expand-file-name "resources/roam/zotero-library.bib" org-directory)
  "Path to Zotero's auto-exported bibliography file.")

(with-eval-after-load 'org-roam
  (setq my/zotero-bib-file 
        (expand-file-name "zotero-library.bib" org-roam-directory)))

;; Check bibliography status
(defun my/zotero-check-bib ()
  "Check if the Zotero bibliography file exists and show its status."
  (interactive)
  (if (not my/zotero-bib-file)
      (message "❌ Bibliography path not set. Load org-roam or customize `my/zotero-bib-file'.")
    (let ((bib-file (expand-file-name my/zotero-bib-file)))
      (if (file-exists-p bib-file)
        (let* ((attrs (file-attributes bib-file))
               (size (file-attribute-size attrs))
               (mod-time (format-time-string "%Y-%m-%d %H:%M" 
                                           (file-attribute-modification-time attrs))))
          (message "✅ Bibliography ready: %d bytes, modified %s" size mod-time)
          (when (fboundp 'bibtex-completion-clear-cache)
            (bibtex-completion-clear-cache)))
        (message "❌ Bibliography file not found: %s" bib-file)
        (message "Set up Zotero auto-export to this location")))))

;; Open bibliography file
(defun my/zotero-open-bib-file ()
  "Open the Zotero bibliography file."
  (interactive)
  (if (not my/zotero-bib-file)
      (message "❌ Bibliography path not set. Load org-roam or customize `my/zotero-bib-file'.")
    (let ((bib-file (expand-file-name my/zotero-bib-file)))
      (if (file-exists-p bib-file)
          (find-file bib-file)
        (message "❌ Bibliography file not found. Run M-x my/zotero-check-bib")))))

(provide 'zotero-bibtex)
;;; zotero-bibtex.el ends here
