;;; spell-checking.el --- Modern spell checking with Jinx -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

;; Jinx requires enchant library to be installed
;; On macOS: brew install enchant
;; On Ubuntu/Debian: sudo apt install libenchant-2-dev pkgconf
;; On Arch: sudo pacman -S enchant pkgconf

;; ============================================================================
;; PACKAGE INSTALLATION
;; ============================================================================

;; Jinx is now installed by packages.el, so we don't need to install it here
;; Just check if enchant library is available and provide helpful messages

;; ============================================================================
;; JINX CONFIGURATION
;; ============================================================================

;; Ensure Homebrew paths are available (important for GUI Emacs on macOS)
(when (eq system-type 'darwin)
  (let ((homebrew-paths '("/opt/homebrew/bin" "/usr/local/bin")))
    (dolist (path homebrew-paths)
      (when (file-directory-p path)
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat (getenv "PATH") ":" path)))))
  
  ;; Set up compiler environment for Jinx native module compilation
  ;; Set up compiler environment for Jinx native module compilation
  (when (file-directory-p "/opt/homebrew")
    (setenv "PKG_CONFIG_PATH" 
            (concat (or (getenv "PKG_CONFIG_PATH") "") 
                    ":/opt/homebrew/lib/pkgconfig"))
    
    ;; Try to get flags from pkg-config, fallback to hardcoded paths
    (let ((pkg-config-output 
           (condition-case nil
               (shell-command-to-string "pkg-config --cflags --libs enchant-2 2>/dev/null")
             (error nil))))
      (if (and pkg-config-output (not (string-empty-p (string-trim pkg-config-output))))
          ;; Use pkg-config output
          (let ((flags (split-string (string-trim pkg-config-output))))
            (setenv "CPPFLAGS" 
                    (concat (or (getenv "CPPFLAGS") "") " "
                            (mapconcat 'identity 
                                     (seq-filter (lambda (s) (string-prefix-p "-I" s)) flags) 
                                     " ")))
            (setenv "LDFLAGS" 
                    (concat (or (getenv "LDFLAGS") "") " "
                            (mapconcat 'identity 
                                     (seq-filter (lambda (s) (or (string-prefix-p "-L" s) 
                                                                 (string-prefix-p "-l" s))) flags) 
                                     " "))))
        ;; Fallback to hardcoded paths
        (setenv "CPPFLAGS" 
                (concat (or (getenv "CPPFLAGS") "") 
                        " -I/opt/homebrew/Cellar/enchant/2.8.12/include/enchant-2"
                        " -I/opt/homebrew/Cellar/glib/2.84.4/include"
                        " -I/opt/homebrew/Cellar/glib/2.84.4/include/glib-2.0"
                        " -I/opt/homebrew/Cellar/glib/2.84.4/lib/glib-2.0/include"
                        " -I/opt/homebrew/opt/gettext/include"
                        " -I/opt/homebrew/Cellar/pcre2/10.45/include"))
        (setenv "LDFLAGS" 
                (concat (or (getenv "LDFLAGS") "") 
                        " -L/opt/homebrew/Cellar/enchant/2.8.12/lib"))))))

;; Load jinx if available
(when (package-installed-p 'jinx)
  ;; Configure languages for English and Finnish
  (setq jinx-languages "en_US fi_FI")
  
  ;; Enable jinx globally for all text modes
  (add-hook 'emacs-startup-hook #'global-jinx-mode)
  
  ;; Keybindings for spell checking
  (keymap-global-set "M-$" #'jinx-correct)      ;; Correct word at point (Alt+Shift+4)
  (keymap-global-set "C-M-$" #'jinx-languages)  ;; Switch languages
  
  ;; Additional easier keybindings
  (keymap-global-set "C-c s c" #'jinx-correct)     ;; Easier alternative: Ctrl+c s c
  (keymap-global-set "C-c s l" #'jinx-languages)   ;; Language switching: Ctrl+c s l
  (keymap-global-set "C-c s b" #'my/jinx-correct-buffer)  ;; Correct entire buffer
  
  ;; Additional useful keybindings
  (with-eval-after-load 'jinx
    ;; These keys work when point is on a misspelled word
    (define-key jinx-mode-map (kbd "C-c s n") #'jinx-next)      ;; Next misspelling
    (define-key jinx-mode-map (kbd "C-c s p") #'jinx-previous)  ;; Previous misspelling
    (define-key jinx-mode-map (kbd "C-c s c") #'jinx-correct))  ;; Correct current word
  
  ;; Performance and behavior settings
  (setq jinx-delay 0.2)  ;; Check spelling after 0.2 seconds of idle time
  
  ;; Enable repeat mode for easy navigation (if available)
  (when (fboundp 'repeat-mode)
    (repeat-mode 1)))

;; ============================================================================
;; LANGUAGE SWITCHING HELPERS
;; ============================================================================

(defun my/jinx-switch-to-english ()
  "Switch jinx to English only."
  (interactive)
  (when (package-installed-p 'jinx)
    (setq jinx-languages "en_US")
    (when (bound-and-true-p jinx-mode)
      (jinx-mode -1)
      (jinx-mode 1))
    (message "Spell checking: English")))

(defun my/jinx-switch-to-finnish ()
  "Switch jinx to Finnish only."
  (interactive)
  (when (package-installed-p 'jinx)
    (setq jinx-languages "fi_FI")
    (when (bound-and-true-p jinx-mode)
      (jinx-mode -1)
      (jinx-mode 1))
    (message "Spell checking: Finnish")))

(defun my/jinx-switch-to-bilingual ()
  "Switch jinx to both English and Finnish."
  (interactive)
  (when (package-installed-p 'jinx)
    (setq jinx-languages "en_US fi_FI")
    (when (bound-and-true-p jinx-mode)
      (jinx-mode -1)
      (jinx-mode 1))
    (message "Spell checking: English + Finnish")))

(defun my/jinx-correct-buffer ()
  "Correct all misspellings in the entire buffer.
Equivalent to 'C-u M-$' but with an easier keybinding."
  (interactive)
  (when (package-installed-p 'jinx)
    (save-excursion
      (let ((universal-argument '(4)))  ;; Simulate C-u prefix
        (jinx-correct universal-argument)))
    (message "Buffer spell check completed")))

;; ============================================================================
;; MODE-SPECIFIC CONFIGURATION
;; ============================================================================

;; Enable jinx for specific modes if global mode is not used
;; (Uncomment if you prefer per-mode activation instead of global)
;; (dolist (hook '(text-mode-hook org-mode-hook markdown-mode-hook))
;;   (add-hook hook #'jinx-mode))

;; ============================================================================
;; INSTALLATION CHECKER
;; ============================================================================

(defun my/check-jinx-setup ()
  "Check if jinx is properly set up and provide installation guidance."
  (interactive)
  (cond
   ((not (package-installed-p 'jinx))
    (message "❌ Jinx not installed. Run M-x package-install jinx"))
   ((not (fboundp 'jinx-mode))
    (message "❌ Jinx not loaded. Try restarting Emacs"))
   ((not (or (executable-find "enchant-lsmod-2") 
             (executable-find "enchant-lsmod")
             (file-exists-p "/opt/homebrew/bin/enchant-lsmod-2")))
    (message "❌ Enchant not found. Install with: brew install enchant"))
   (t
    (message "✅ Jinx spell checking is ready!")
    (message "Languages: %s" (if (boundp 'jinx-languages) jinx-languages "default"))
    (message "Commands: M-$ (correct), C-M-$ (switch language)")
    (message "Mode helpers: C-c s n/p (next/prev), C-c s c (correct)"))))

;; ============================================================================
;; KEYBINDINGS SUMMARY
;; ============================================================================

;; Global keybindings:
;; M-$         jinx-correct (correct word at point)
;; C-M-$       jinx-languages (switch languages)
;; C-c s c     jinx-correct (easier alternative)
;; C-c s b     my/jinx-correct-buffer (correct entire buffer)
;; C-c s l     jinx-languages (language switching)
;; C-c s n     jinx-next (next misspelling)
;; C-c s p     jinx-previous (previous misspelling)

;; Language switching:
;; M-x my/jinx-switch-to-english
;; M-x my/jinx-switch-to-finnish
;; M-x my/jinx-switch-to-bilingual

;; Setup check:
;; M-x my/check-jinx-setup

(provide 'spell-checking)

;;; spell-checking.el ends here
