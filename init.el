;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'org-basic)
(require 'reading-tracker)
(require 'welcome)


;; Never make backups for version-controlled files (Git, etc.)
(setq vc-make-backup-files nil)   ;; this is the default


;; Load Org, then register the reading file in the agenda safely
(require 'org)
(add-to-list 'org-agenda-files my/org-reading-file)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi-tinted))
 '(package-selected-packages '(magit))
 '(scroll-bar-mode nil)) ; Hide GUI scrollbars

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 240 :width normal)))))










;;; End of init.el
