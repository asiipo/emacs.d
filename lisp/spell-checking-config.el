;;; spell-checking-config.el --- Modern spell checking with Jinx -*- lexical-binding: t; -*-

;; Version: 2.0 - Streamlined
;; Author: arttusii
;; Description: Essential Jinx spell checking for org-roam

;;; Commentary:
;;
;; Minimal Jinx spell checking setup with English/Finnish support.
;; Requires: brew install enchant (macOS)

;;; Code:

;; macOS setup for Jinx compilation
(when (eq system-type 'darwin)
  (when (file-directory-p "/opt/homebrew")
    (setenv "PKG_CONFIG_PATH" 
            (concat (or (getenv "PKG_CONFIG_PATH") "") 
                    ":/opt/homebrew/lib/pkgconfig"))))

;; Jinx configuration
(when (package-installed-p 'jinx)
  ;; Set up bilingual spell checking
  (setq jinx-languages "en_US fi"
        jinx-delay 0.2)
  
  ;; Enable globally for text modes
  (add-hook 'emacs-startup-hook 
            (lambda () 
              (when (fboundp 'global-jinx-mode)
                (global-jinx-mode 1))))
  
  ;; Mode-specific keybindings
  (with-eval-after-load 'jinx
    (define-key jinx-mode-map (kbd "C-c s n") #'jinx-next)
    (define-key jinx-mode-map (kbd "C-c s p") #'jinx-previous)
    (define-key jinx-mode-map (kbd "C-c s c") #'jinx-correct)))

;; Setup validation
(defun my/check-jinx-setup ()
  "Check if Jinx spell checking is ready."
  (interactive)
  (cond
   ((not (package-installed-p 'jinx))
    (message "❌ Install: M-x package-install jinx"))
   ((not (executable-find "enchant-lsmod-2"))
    (message "❌ Install: brew install enchant"))
   (t
    (message "✅ Jinx ready with languages: %s" jinx-languages))))

(provide 'spell-checking-config)
;;; spell-checking-config.el ends here
