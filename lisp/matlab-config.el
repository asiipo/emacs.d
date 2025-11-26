;;; matlab-config.el --- MATLAB mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for MATLAB editing and integration.
;; Sets up matlab-mode with platform-specific paths.

;;; Code:

;; ============================================================================
;; MATLAB MODE SETUP
;; ============================================================================

;; Ensure matlab-mode is available (install via package.el if needed)
(unless (package-installed-p 'matlab-mode)
  (package-install 'matlab-mode))

(require 'matlab-mode)

;; ============================================================================
;; PLATFORM-SPECIFIC PATHS
;; ============================================================================

;; WSL: Custom MATLAB path
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME"))
  (setq matlab-shell-command "/home/arttu/.matlab/R2023a/bin/matlab"))

;; macOS: Uses system default (already works correctly)
;; No configuration needed

;; ============================================================================
;; MATLAB MODE SETTINGS
;; ============================================================================

;; File associations
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;; Basic editing settings
(setq matlab-indent-function t          ; Use MATLAB's indent function
      matlab-shell-command-switches '("-nodesktop" "-nosplash"))  ; CLI mode

;; Optional: Enable fill (word wrap) in comments
(add-hook 'matlab-mode-hook
          (lambda ()
            (setq fill-column 80)
            (auto-fill-mode 1)))

(provide 'matlab-config)
;;; matlab-config.el ends here
