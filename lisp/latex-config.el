;;; latex-config.el --- LaTeX editing and preview configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'path-utils)  ;; Cross-platform path management

;; ============================================================================
;; AUCTEX CONFIGURATION (Ubuntu/WSL)
;; ============================================================================

;; Use distro AUCTeX
(add-to-list 'load-path "/usr/share/emacs/site-lisp/auctex")
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Basic AUCTeX settings optimized for natbib workflow
(setq TeX-auto-save t
      TeX-parse-self t
      TeX-PDF-mode t
      TeX-source-correlate-mode t
      TeX-save-query nil)

;; LaTeX mode hook with natbib-optimized settings
(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; Set LaTeX (pdflatex) as default for compilation
            (setq TeX-command-default "LaTeX")
            ;; Enable source correlation for forward/inverse search
            (TeX-source-correlate-mode 1)
            ;; Enable RefTeX for citations and cross-references
            (turn-on-reftex)
            (setq reftex-plug-into-AUCTeX t)
            ;; Auto-fill for better text editing
            (auto-fill-mode 1)))


;; ============================================================================
;; LATEX PREVIEW CONFIGURATION
;; ============================================================================

;; Make previews bigger for better readability
(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 2.0))

;; Use crisp SVG images instead of blurry PNGs for better quality
(setq org-preview-latex-default-process 'dvisvgm)

;; PERFORMANCE: LaTeX preview on-demand only (use C-c C-x C-l to toggle)
;; Auto-preview disabled to prevent startup slowdown when loading many org files
(setq org-startup-with-latex-preview nil)

;; Note: LaTeX paths are now handled by path-utils.el

(provide 'latex-config)
