;;; core-init.el --- Backups, autosaves, editing defaults -*- lexical-binding: t; -*-

;; ============================================================================
;; BACKUP AND AUTO-SAVE CONFIGURATION
;; ============================================================================

;; Configure backup and auto-save directories under ~/.emacs.d/var/
(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (backup-dir (expand-file-name "backups/" var-dir))
       (autosave-dir (expand-file-name "auto-saves/" var-dir)))
  ;; Create directories if they don't exist
  (dolist (d (list backup-dir autosave-dir)) 
    (make-directory d t))
  
  ;; Set backup and auto-save behavior
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        create-lockfiles nil
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        version-control t))

;; ============================================================================
;; EDITING DEFAULTS
;; ============================================================================

;; Text editing behavior
(setq sentence-end-double-space nil      ;; Don't require double space after sentences
      require-final-newline t            ;; Always end files with newline
      confirm-kill-emacs nil             ;; Don't ask when quitting
      confirm-kill-processes nil         ;; Don't ask when killing processes
      indent-tabs-mode nil               ;; Use spaces instead of tabs
      tab-width 4                        ;; Standard tab width
      fill-column 80                     ;; Standard line length limit
      truncate-lines nil                 ;; Wrap long lines by default
      word-wrap t)                       ;; Wrap at word boundaries

;; ============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; ============================================================================

;; Increase garbage collection threshold during startup
(setq gc-cons-threshold (* 50 1000 1000))  ;; 50MB
(setq gc-cons-percentage 0.6)

;; Reset GC settings after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 20 1000 1000)  ;; 20MB
                  gc-cons-percentage 0.1)))

;; Reduce file handler checking during startup
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

;; ============================================================================
;; UI IMPROVEMENTS
;; ============================================================================

;; Clean up the interface
(setq inhibit-startup-message t           ;; No startup message
      inhibit-splash-screen t             ;; No splash screen
      initial-scratch-message nil         ;; Clean scratch buffer
      ring-bell-function 'ignore          ;; No bell sounds
      use-dialog-box nil                  ;; No GUI dialogs
      use-file-dialog nil)                ;; No file dialogs

;; Scrolling behavior
(setq scroll-step 1                       ;; Smooth scrolling
      scroll-margin 3                     ;; Show context while scrolling
      scroll-conservatively 10000          ;; Never recenter when scrolling
      auto-window-vscroll nil             ;; Disable automatic vertical scrolling
      fast-but-imprecise-scrolling t      ;; Better performance for large files
      hscroll-step 1                      ;; Smooth horizontal scrolling
      hscroll-margin 1)

;; ============================================================================
;; TYPOGRAPHY AND FONTS
;; ============================================================================

;; Set default fonts (Iosevka family for beautiful, modern look)
(when (display-graphic-p)
  ;; Main monospace font for code and most text
  (when (find-font (font-spec :name "Iosevka"))
    (set-face-attribute 'default nil
                        :font "Iosevka"
                        :height 160))  ;; 16pt (height is in 1/10pt)
  
  ;; Variable-pitch font for prose (org-mode text)
  (when (find-font (font-spec :name "Iosevka Etoile"))
    (set-face-attribute 'variable-pitch nil
                        :font "Iosevka Etoile"
                        :height 160))
  
  ;; Emoji support - ensures colorful emoji display
  (when (find-font (font-spec :name "Noto Color Emoji"))
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))
  
  ;; Fallback to common fonts if Iosevka not available
  (unless (find-font (font-spec :name "Iosevka"))
    (cond
     ((find-font (font-spec :name "Fira Code"))
      (set-face-attribute 'default nil :font "Fira Code" :height 140))
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)))))

;; Frame transparency (94% opaque for subtle effect)
(when (display-graphic-p)
  (set-frame-parameter nil 'alpha-background 94)
  (add-to-list 'default-frame-alist '(alpha-background . 94)))

(defun my/toggle-transparency ()
  "Toggle between transparent and opaque frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (if (and alpha (< alpha 100))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background 94))))

;; ============================================================================
;; FILE HANDLING
;; ============================================================================

;; Better file handling
(setq large-file-warning-threshold (* 100 1000 1000)  ;; 100MB warning threshold
      vc-follow-symlinks t               ;; Follow symlinks automatically
      backup-by-copying t)               ;; Copy files for backup (safer)

;; Recent files
(setq recentf-max-saved-items 200        ;; Remember more recent files
      recentf-max-menu-items 15          ;; Show reasonable number in menu
      recentf-auto-cleanup 'never)       ;; Don't auto-cleanup (can be slow)

;; ============================================================================
;; MINIBUFFER AND COMPLETION
;; ============================================================================

;; Better minibuffer behavior
(setq enable-recursive-minibuffers t      ;; Allow nested minibuffer commands
      read-extended-command-predicate     ;; Hide disabled commands
      #'command-completion-default-include-p)

;; History settings
(setq history-length 1000                ;; Remember more history
      history-delete-duplicates t        ;; Remove duplicate entries
      savehist-additional-variables       ;; Save additional variables
      '(mark-ring global-mark-ring        ;; Mark rings
        search-ring regexp-search-ring    ;; Search history
  extended-command-history))        ;; M-x history

;; ;; Make *Completions* buffer list candidates one per line (TAB help)
;; (setq completions-format 'one-column)

;; Modern minibuffer completion (vertical layout similar to Ivy/Selectrum)
;; (when (fboundp 'fido-vertical-mode)
;;   (fido-vertical-mode 1))

;; ============================================================================
;; REPEAT MODE - Enable command repetition
;; ============================================================================

;; Enable repeat-mode for better UX (e.g., C-x o o o to cycle windows)
(repeat-mode 1)

;; ============================================================================
;; SECURITY AND PRIVACY
;; ============================================================================

;; Security improvements (reasonable defaults)
(setq network-security-level 'high       ;; Require secure connections
      gnutls-verify-error t)              ;; Verify TLS certificates

;; ============================================================================
;; STARTUP OPTIMIZATIONS
;; ============================================================================

;; Enable useful built-in modes
(when (fboundp 'global-auto-revert-mode)
  (global-auto-revert-mode 1))           ;; Auto-revert changed files

(when (fboundp 'delete-selection-mode)
  (delete-selection-mode 1))             ;; Replace selection when typing

(when (fboundp 'show-paren-mode)
  (show-paren-mode 1))                   ;; Highlight matching parentheses

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode 1))                ;; Auto-insert matching brackets

(when (fboundp 'savehist-mode)
  (savehist-mode 1))                     ;; Save minibuffer history

(when (fboundp 'recentf-mode)
  (recentf-mode 1))                      ;; Track recent files

;; Set up better default modes for programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)  ;; Line numbers in code
(add-hook 'text-mode-hook #'visual-line-mode)          ;; Visual line mode for text

;; ============================================================================
;; PDF TOOLS CONFIGURATION (REPLACE DOCVIEW)
;; ============================================================================

;; Configure PDF Tools as the default PDF viewer (replacing DocView)
(when (or (package-installed-p 'pdf-tools)
          (locate-library "pdf-tools"))
  
  ;; Install and initialize PDF Tools (but don't ask for compilation)
  (unless (fboundp 'pdf-view-mode)
    (require 'pdf-tools nil t)
    (when (fboundp 'pdf-tools-install)
      (pdf-tools-install t nil t)))  ;; Silent install
  
  ;; Replace DocView with PDF Tools for PDF files
  (when (fboundp 'pdf-view-mode)
    ;; Set PDF Tools as the default for PDF files
    (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
    
    ;; Better PDF Tools settings
    (setq-default pdf-view-display-size 'fit-width)  ;; Default to fit width
    
    ;; PDF Tools keybindings enhancement
    (add-hook 'pdf-view-mode-hook
              (lambda ()
                ;; Better navigation
                (local-set-key (kbd "C-s") #'isearch-forward)
                (local-set-key (kbd "C-r") #'isearch-backward)
                ;; Better zoom controls
                (local-set-key (kbd "=") #'pdf-view-enlarge)
                (local-set-key (kbd "-") #'pdf-view-shrink)
                (local-set-key (kbd "0") #'pdf-view-scale-reset)
                ;; Fit controls
                (local-set-key (kbd "W") #'pdf-view-fit-width-to-window)
                (local-set-key (kbd "H") #'pdf-view-fit-height-to-window)
                (local-set-key (kbd "P") #'pdf-view-fit-page-to-window)))))

(provide 'core-init)

