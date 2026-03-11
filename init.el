;;; init.el --- Main Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modular configuration. See lisp/config-loader.el for loaded modules.

;;; Code:

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Initialize packages
(require 'packages)

;; Load customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Load all configuration modules
(require 'config-loader)
(my/load-all-config-modules)

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))

(provide 'init)
;;; init.el ends here
