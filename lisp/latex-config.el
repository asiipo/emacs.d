;;; latex-config.el --- LaTeX editing and preview configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'path-utils)  ;; Cross-platform path management

;; ============================================================================
;; AUCTEX CONFIGURATION
;; ============================================================================

;; Only configure LaTeX if AucTeX is available
(when (or (package-installed-p 'auctex)
          (file-exists-p "/usr/share/emacs/site-lisp/auctex/auctex.el")
          (file-directory-p "/opt/homebrew/share/emacs/site-lisp/auctex"))
  
  ;; Try to load AucTeX from various possible locations
  (cond
   ;; Check if AucTeX is installed via package manager
   ((package-installed-p 'auctex)
    (require 'auctex))
   
   ;; Ubuntu/Debian system installation
   ((file-exists-p "/usr/share/emacs/site-lisp/auctex/auctex.el")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/auctex")
    (load "auctex.el" nil t t)
    (load "preview-latex.el" nil t t))
   
   ;; macOS Homebrew installation (if using emacs-plus or similar)
   ((file-directory-p "/opt/homebrew/share/emacs/site-lisp/auctex")
    (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/auctex")
    (load "auctex.el" nil t t)))

  ;; Basic AUCTeX settings optimized for natbib workflow
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-save-query nil
        ;; Better completion and interaction
        TeX-electric-escape nil          ;; Disable automatic escaping
        TeX-electric-math '("$" . "$")   ;; Auto-close math delimiters
        LaTeX-electric-left-right-brace t ;; Auto-close braces
        ;; Enable SyncTeX for forward/inverse search
        TeX-source-correlate-start-server t)

  ;; LaTeX mode hook with enhanced settings
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; Set LaTeX (pdflatex) as default for compilation
              (setq TeX-command-default "LaTeX")
              ;; Enable source correlation for forward/inverse search
              (TeX-source-correlate-mode 1)
              ;; Enable RefTeX for citations and cross-references
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t
                    ;; Better RefTeX settings for natbib workflow
                    reftex-cite-format "%l"  ;; Simple citation format
                    reftex-default-bibliography '("~/bibliography.bib")
                    reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
              ;; Text editing enhancements
              (auto-fill-mode 1)
              (setq fill-column 80)
              ;; Enable folding for better document navigation
              (TeX-fold-mode 1)
              ;; Enable company completion if available
              (when (fboundp 'company-mode)
                (company-mode 1))
              ;; Enable CDLaTeX for faster math input if available
              (when (fboundp 'turn-on-cdlatex)
                (turn-on-cdlatex))))

  ;; ============================================================================
  ;; ADDITIONAL LATEX PACKAGES CONFIGURATION
  ;; ============================================================================

  ;; Configure company-auctex for better autocompletion (if available)
  (when (package-installed-p 'company-auctex)
    (require 'company-auctex)
    (company-auctex-init))

  ;; Configure CDLaTeX for faster math input (if available)
  (when (package-installed-p 'cdlatex)
    (require 'cdlatex)
    ;; Customize CDLaTeX settings
    (setq cdlatex-paired-parens "$[{(")
    (setq cdlatex-command-alist
          '(("frac" "Insert \\frac{}{}" "\\frac{?}{}" cdlatex-position-cursor nil nil t)
            ("sum" "Insert \\sum_{}{}" "\\sum_{?}^{}" cdlatex-position-cursor nil nil t))))

  ;; Configure PDF Tools for better PDF viewing (if available)
  (when (package-installed-p 'pdf-tools)
    (require 'pdf-tools)
    ;; Initialize pdf-tools
    (pdf-tools-install-noverify)
    ;; Configure AUCTeX to use PDF Tools
    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
          TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))))
  
  ;; Fallback PDF viewer configuration (if PDF Tools not available)
  (unless (package-installed-p 'pdf-tools)
    (setq TeX-view-program-selection '((output-pdf "PDF Viewer"))
          TeX-view-program-list '(("PDF Viewer" "open %o"))))

  ;; AUCTeX preview configuration
  (when (fboundp 'preview-latex)
    ;; Better preview settings
    (setq preview-scale-function 1.5)
    ;; Preview only math and graphics by default
    (setq preview-default-option-list '("displaymath" "floats" "graphics" "textmath")))

  ;; Note: LaTeX executable paths are handled by path-utils.el
  ;; Optional packages (company-auctex, cdlatex, pdf-tools) are configured if available
  )

;; ============================================================================
;; ORG-MODE LATEX PREVIEW CONFIGURATION (outside AUCTeX block)
;; ============================================================================

;; Apply to all org files, including org-roam
(with-eval-after-load 'org
  ;; Make previews bigger for better readability
  (plist-put org-format-latex-options :scale 2.0)

  ;; Use crisp SVG images instead of blurry PNGs for better quality
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; PERFORMANCE: LaTeX preview on-demand only (use C-c C-x C-l to toggle)
  ;; Auto-preview disabled to prevent startup slowdown when loading many org files
  (setq org-startup-with-latex-preview nil))

;; ============================================================================
  ;; UTILITY FUNCTIONS
  ;; ============================================================================

  (defun my/latex-word-count ()
    "Count words in current LaTeX document using texcount."
    (interactive)
    (if (executable-find "texcount")
        (let ((file (buffer-file-name)))
          (if file
              (shell-command (format "texcount -1 -sum %s" (shell-quote-argument file)))
            (message "Buffer must be saved to a file first")))
      (message "texcount not found. Install texlive-extra-utils or similar package.")))

  (defun my/latex-clean-aux-files ()
    "Clean auxiliary LaTeX files in current directory."
    (interactive)
    (let ((aux-extensions '("aux" "bbl" "blg" "fls" "fdb_latexmk" "log" "out" "toc" "nav" "snm" "vrb")))
      (dolist (ext aux-extensions)
        (dolist (file (file-expand-wildcards (format "*.%s" ext)))
          (delete-file file)))
      (message "Cleaned LaTeX auxiliary files")))

  ;; Keybindings for LaTeX utilities (only in LaTeX mode)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l w") #'my/latex-word-count)
              (local-set-key (kbd "C-c l c") #'my/latex-clean-aux-files)))

  ;; Note: LaTeX executable paths are handled by path-utils.el
  ;; Optional packages (company-auctex, cdlatex, pdf-tools) are configured if available
  )

;; ============================================================================
;; INFORMATION & USAGE
;; ============================================================================

;; This configuration provides:
;; - Cross-platform AUCTeX setup with graceful fallback
;; - Enhanced completion with company-auctex (if installed)
;; - Fast math input with CDLaTeX (if installed)
;; - Better PDF viewing with pdf-tools (if installed)
;; - Utility functions for word counting and cleanup
;; - Optimized preview settings for org-mode and AUCTeX
;;
;; Key bindings in LaTeX mode:
;; - C-c l w : Word count (requires texcount)
;; - C-c l c : Clean auxiliary files
;;
;; Optional packages to install for full functionality:
;; - company-auctex: Enhanced autocompletion
;; - cdlatex: Fast math input and templates
;; - pdf-tools: Better PDF viewing and annotation

;; If AucTeX not available, LaTeX support is simply not configured
;; (no error messages needed - consistent with other optional features)

(provide 'latex-config)
