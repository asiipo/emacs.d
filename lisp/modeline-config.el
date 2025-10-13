;;; modeline-config.el --- Clean mode line configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures the mode line to hide noisy minor mode indicators
;; while keeping useful information visible. Uses diminish to selectively
;; hide or shorten minor mode names.

;;; Code:

(require 'diminish)

;; ============================================================================
;; HIDE NOISY MINOR MODES
;; ============================================================================

;; These modes are always active and clutter the mode line without adding value

;; Org-roam and related modes
(with-eval-after-load 'org-roam
  (diminish 'org-roam-mode))

(with-eval-after-load 'org-roam-ui
  (diminish 'org-roam-ui-mode)
  (diminish 'org-roam-ui-follow-mode))

(with-eval-after-load 'org-roam-bibtex
  (diminish 'org-roam-bibtex-mode))

;; Built-in Emacs modes
(diminish 'visual-line-mode)           ; Word wrapping
(diminish 'auto-fill-function)         ; Auto-fill
(diminish 'eldoc-mode)                 ; Documentation display
(diminish 'abbrev-mode)                ; Abbreviations

;; Org-mode built-ins
(with-eval-after-load 'org
  (diminish 'org-indent-mode))         ; Org indentation

;; Version control
(with-eval-after-load 'vc
  (diminish 'vc-mode))                 ; Version control indicator

;; ============================================================================
;; SHORTEN USEFUL MODES (keep visible but shorter)
;; ============================================================================

;; These modes are useful to see, but we'll use shorter names

;; Spell checking - show which language is active
(with-eval-after-load 'jinx
  (diminish 'jinx-mode " ✓"))         ; Checkmark indicates spell-check active

;; If you add more modes later, you can shorten them like this:
;; (diminish 'some-mode " Short")     ; Shows " Short" instead of full name

;; ============================================================================
;; MODE LINE FORMAT TWEAKS
;; ============================================================================

;; Optional: Simplify the mode line format slightly
;; Keep buffer name, major mode, position, but clean up the rest

;; Show line and column numbers in a cleaner format
(setq mode-line-position
      '((line-number-mode
         ("%l" (column-number-mode ":%c")))
        (-3 "%p")))  ; Show percentage through buffer

;; ============================================================================
;; DOCUMENTATION
;; ============================================================================

;; To hide additional modes later, add lines like:
;;   (with-eval-after-load 'package-name
;;     (diminish 'mode-name))
;;
;; To shorten instead of hide:
;;   (diminish 'mode-name " Short")
;;
;; To see all active minor modes:
;;   M-x describe-mode

(provide 'modeline-config)
;;; modeline-config.el ends here
