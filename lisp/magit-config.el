;;; magit-config.el --- Magit setup -*- lexical-binding: t; -*-

;; Basic keybinding
(global-set-key (kbd "C-x g") #'magit-status)

;; Use same window behavior for popups if you prefer minimal window churn
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(provide 'magit-config) 