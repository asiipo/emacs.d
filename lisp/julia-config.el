;;; julia-config.el --- Julia development environment -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Julia editing with LSP and REPL integration.

;;; Code:

;; ============================================================================
;; PACKAGE INSTALLATION
;; ============================================================================

(unless (package-installed-p 'julia-mode)
  (package-install 'julia-mode))

(unless (package-installed-p 'eglot-jl)
  (package-install 'eglot-jl))

(unless (package-installed-p 'julia-repl)
  (package-install 'julia-repl))

(unless (package-installed-p 'company)
  (package-install 'company))

;; ============================================================================
;; JULIA MODE
;; ============================================================================

(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'")

;; ============================================================================
;; LSP SUPPORT
;; ============================================================================

(use-package eglot-jl
  :ensure t
  :config
  (eglot-jl-init)
  (setq eglot-connect-timeout 180))  ; First run downloads LanguageServer.jl

(add-hook 'julia-mode-hook #'eglot-ensure)

;; ============================================================================
;; REPL INTEGRATION
;; ============================================================================

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

;; ============================================================================
;; CODE COMPLETION
;; ============================================================================

(use-package company
  :ensure t
  :hook (julia-mode . company-mode))

;; Julia startup options
(setq julia-repl-switches "-t auto")

(provide 'julia-config)
;;; julia-config.el ends here
