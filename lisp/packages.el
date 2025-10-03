;;; packages.el --- Minimal package setup -*- lexical-binding: t; -*-

;; ============================================================================
;; PACKAGE SYSTEM INITIALIZATION
;; ============================================================================

(require 'package)

;; Add MELPA repository (GNU ELPA is included by default)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package.el if not already done
(unless package--initialized
  (package-initialize))

;; Refresh package archives on first run if needed
(unless package-archive-contents
  (package-refresh-contents))

;; ============================================================================
;; ESSENTIAL PACKAGES
;; ============================================================================

;; Ensure Magit is installed for Git integration
(unless (package-installed-p 'magit)
  (condition-case err
      (progn
        (message "Installing magit...")
        (package-install 'magit)
        (message "Magit installed successfully"))
    (error 
     (message "Warning: Failed to install magit: %s" (error-message-string err)))))

;; Ensure doom-themes is installed (since you're using doom-dracula)
(unless (package-installed-p 'doom-themes)
  (condition-case err
      (progn
        (message "Installing doom-themes...")
        (package-install 'doom-themes)
        (message "Doom-themes installed successfully"))
    (error 
     (message "Warning: Failed to install doom-themes: %s" (error-message-string err)))))

;; Ensure jinx is installed for modern spell checking
(unless (package-installed-p 'jinx)
  (condition-case err
      (progn
        (message "Installing jinx...")
        (package-install 'jinx)
        (message "Jinx installed successfully"))
    (error 
     (message "Warning: Failed to install jinx: %s" (error-message-string err))
     (message "Please install enchant library first: brew install enchant"))))

;; Ensure org-roam-bibtex is installed for bibliography management
(unless (package-installed-p 'org-roam-bibtex)
  (condition-case err
      (progn
        (message "Installing org-roam-bibtex...")
        (package-install 'org-roam-bibtex)
        (message "Org-roam-bibtex installed successfully"))
    (error 
     (message "Warning: Failed to install org-roam-bibtex: %s" (error-message-string err)))))

(provide 'packages) 