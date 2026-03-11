;;; core-init.el --- Core settings -*- lexical-binding: t; -*-

;;; Code:

;; Backup and auto-save
(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (backup-dir (expand-file-name "backups/" var-dir))
       (autosave-dir (expand-file-name "auto-saves/" var-dir)))
  ;; Create directories if they don't exist
  (dolist (d (list backup-dir autosave-dir)) 
    (make-directory d t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        create-lockfiles nil
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        version-control t
        vc-make-backup-files nil))

;; Editing defaults
(setq sentence-end-double-space nil
      require-final-newline t
      confirm-kill-emacs nil
      confirm-kill-processes nil
      indent-tabs-mode nil
      tab-width 4
      fill-column 80
      truncate-lines nil
      word-wrap t)

;; macOS-specific
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier nil))

;; UI
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil)

;; Scrolling
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 10000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      hscroll-step 1
      hscroll-margin 1)

;; Fonts
(when (display-graphic-p)
  (when (find-font (font-spec :name "Iosevka"))
    (set-face-attribute 'default nil :font "Iosevka" :height 160))
  (when (find-font (font-spec :name "Iosevka Etoile"))
    (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile" :height 160))
  (when (find-font (font-spec :name "Noto Color Emoji"))
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))
  (unless (find-font (font-spec :name "Iosevka"))
    (cond
     ((find-font (font-spec :name "Fira Code"))
      (set-face-attribute 'default nil :font "Fira Code" :height 140))
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)))))

;; Transparency
(when (display-graphic-p)
  (set-frame-parameter nil 'alpha-background 94)
  (add-to-list 'default-frame-alist '(alpha-background . 94)))

(defun my/toggle-transparency ()
  "Toggle frame transparency."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (if (and alpha (< alpha 100))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background 94))))

;; File handling
(setq large-file-warning-threshold (* 100 1000 1000)
      vc-follow-symlinks t
      backup-by-copying t
      recentf-max-saved-items 200
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)

;; Minibuffer
(setq enable-recursive-minibuffers t
      read-extended-command-predicate #'command-completion-default-include-p
      history-length 1000
      history-delete-duplicates t
      savehist-additional-variables
      '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))

(repeat-mode 1)

;; Security
(setq network-security-level 'high
      gnutls-verify-error t)

;; Built-in modes
(when (fboundp 'global-auto-revert-mode)
  (global-auto-revert-mode 1))
(when (fboundp 'delete-selection-mode)
  (delete-selection-mode 1))
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1))
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode 1))
(when (fboundp 'savehist-mode)
  (savehist-mode 1))
(when (fboundp 'recentf-mode)
  (recentf-mode 1))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;; PDF Tools
(when (or (package-installed-p 'pdf-tools)
          (locate-library "pdf-tools"))
  (unless (fboundp 'pdf-view-mode)
    (require 'pdf-tools nil t)
    (when (fboundp 'pdf-tools-install)
      (pdf-tools-install t nil t)))
  (when (fboundp 'pdf-view-mode)
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    (setq-default pdf-view-display-size 'fit-width)
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                (local-set-key (kbd "C-s") #'isearch-forward)
                (local-set-key (kbd "C-r") #'isearch-backward)
                (local-set-key (kbd "=") #'pdf-view-enlarge)
                (local-set-key (kbd "-") #'pdf-view-shrink)
                (local-set-key (kbd "0") #'pdf-view-scale-reset)
                (local-set-key (kbd "W") #'pdf-view-fit-width-to-window)
                (local-set-key (kbd "H") #'pdf-view-fit-height-to-window)
                (local-set-key (kbd "P") #'pdf-view-fit-page-to-window)))))

(provide 'core-init)
;;; core-init.el ends here

