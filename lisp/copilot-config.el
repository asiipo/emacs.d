;;; copilot-config.el --- GitHub Copilot integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for GitHub Copilot AI code completion.
;; Requires Node.js 22+ and a GitHub Copilot subscription.

;;; Code:

;; ============================================================================
;; COPILOT SETUP
;; ============================================================================

(use-package copilot
  :hook (prog-mode . copilot-mode)
  
  :custom
  ;; Completion delay (0 = trigger immediately after typing stops)
  (copilot-idle-delay 0.5)
  
  :init
  ;; Don't show completions in certain situations
  (setq copilot-disable-predicates
        '((lambda () buffer-read-only)     ; Don't show in read-only buffers
          (lambda () (minibufferp))))      ; Don't show in minibuffer
  
  :config
  ;; Commands that won't clear the overlay
  (setq copilot-clear-overlay-ignore-commands
        '(mwheel-scroll scroll-up-command scroll-down-command))
  
  ;; Keybindings
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "C-c <tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "M-<right>") #'copilot-accept-completion-by-word)
  (define-key copilot-mode-map (kbd "M-<down>") #'copilot-accept-completion-by-line)
  (define-key copilot-mode-map (kbd "M-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "C-c C-d") #'copilot-diagnose))

;; ============================================================================
;; SETUP INSTRUCTIONS
;; ============================================================================

;; After installing the copilot package:
;;
;; 1. Make sure Node.js 22+ is installed: node --version
;;
;; 2. Install copilot server: M-x copilot-install-server
;;
;; 3. Login to GitHub: M-x copilot-login
;;
;; 4. Check status: M-x copilot-diagnose
;;
;; Usage:
;; - Copilot automatically shows suggestions as you type in programming modes
;; - C-<tab>: Accept full completion
;; - M-<right>: Accept next word
;; - M-<down>: Accept next line
;; - M-n / M-p: Cycle through suggestions

(provide 'copilot-config)
;;; copilot-config.el ends here
