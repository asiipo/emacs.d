;;; latex-config.el --- LaTeX editing and preview configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'path-utils)  ;; Cross-platform path management

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
