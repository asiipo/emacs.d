;;; packages.el --- Package management -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize package system and install essential packages.

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Essential packages
(defvar my/essential-packages
  '(use-package diminish magit doom-themes jinx org-roam-bibtex 
    svg-lib julia-mode eglot-jl julia-repl company))

;; Install with error handling
(dolist (pkg my/essential-packages)
  (unless (package-installed-p pkg)
    (condition-case err
        (package-install pkg)
      (error
       (message "Warning: Failed to install %s: %s" 
                pkg (error-message-string err))))))

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'packages)
;;; packages.el ends here 