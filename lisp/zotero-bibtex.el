;;; zotero-bibtex.el --- Zotero Better BibTeX cache management -*- lexical-binding: t; -*-

;; Version: 4.0
;; Author: arttusii
;; Description: Simple Zotero library fetcher for org-roam-bibtex
;; Requires: Better BibTeX plugin for Zotero (https://retorque.re/zotero-better-bibtex/)

;;; Commentary:
;;
;; Minimal Zotero integration: just fetch and cache your bibliography.
;; org-roam-bibtex configuration is in org-roam-config.el
;;
;; Commands:
;; - C-c z r: Refresh Zotero library cache (auto-fallback to manual source)
;; - C-c z m: Use manual bibliography source (for WSL/offline)
;; - C-c z t: Test Zotero connection
;; - C-c z i: Insert link to bibliography note (in org-roam-config.el)
;;
;; WSL/Offline Setup:
;; 1. Export bibliography from Zotero to a file (e.g., ~/Dropbox/zotero.bib)
;; 2. Set: (setq my/zotero-manual-bib-source "~/Dropbox/zotero.bib")
;; 3. Use C-c z m to manually sync, or C-c z r will auto-fallback if connection fails

;;; Code:

(require 'url)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

(defvar my/zotero-bbt-port 23119
  "Port for Better BibTeX HTTP server.")

(defvar my/zotero-library-id "1"
  "Zotero library ID ('1' = My Library).")

(defvar my/zotero-bbt-format "biblatex"
  "Export format: 'bibtex' or 'biblatex'.")

(defvar my/zotero-timeout 10
  "Timeout in seconds for HTTP requests to Zotero.")

(defvar my/zotero-auto-fetch-on-startup t
  "Automatically fetch Zotero library on first Emacs startup (after 5s idle).")

(defvar my/zotero-auto-fetched nil
  "Internal flag to track if auto-fetch has been performed.")

(defvar my/zotero-manual-bib-source nil
  "Path to manually downloaded BibTeX file (for WSL or offline use).
If set, this file will be copied when Zotero connection fails.
Example: \"~/Dropbox/zotero-export.bib\" or \"/mnt/c/Users/YourName/zotero.bib\"")

;; Set this to your manually downloaded bibliography file location
(setq my/zotero-manual-bib-source "~/org/resources/roam/My Library.biblatex")

(defun my/zotero-bib-file ()
  "Return path to Zotero bibliography cache file."
  (let ((roam-dir (or (and (boundp 'org-roam-directory) org-roam-directory)
                      (file-truename "~/org/resources/roam"))))
    (expand-file-name "zotero-library.bib" roam-dir)))

;; ============================================================================
;; CORE FUNCTIONALITY
;; ============================================================================

;;;###autoload
(defun my/zotero-refresh ()
  "Fetch BibTeX data from Zotero and cache it for org-roam-bibtex.
If connection fails and my/zotero-manual-bib-source is set, copy from that file instead."
  (interactive)
  (let ((url (format "http://127.0.0.1:%d/better-bibtex/export?/library;id:%s/My%%20Library.%s"
                     my/zotero-bbt-port
                     my/zotero-library-id
                     my/zotero-bbt-format))
        (success nil))
    (message "Fetching Zotero library...")
    (condition-case err
        (let ((buf (url-retrieve-synchronously url t nil my/zotero-timeout)))
          (if (not (buffer-live-p buf))
              (progn
                (message "✗ Failed to fetch (timeout after %ds)" my/zotero-timeout)
                (setq success nil))
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "^$" nil 'move)
              (forward-line)
              (let ((data (buffer-substring (point) (point-max)))
                    (target (my/zotero-bib-file)))
                (kill-buffer)
                ;; Ensure directory exists
                (let ((dir (file-name-directory target)))
                  (unless (file-directory-p dir)
                    (make-directory dir t)))
                ;; Write to cache file with UTF-8 encoding
                (let ((coding-system-for-write 'utf-8))
                  (with-temp-file target
                    (insert data)))
                (message "✓ Zotero library cached to %s" target)
                ;; Refresh bibtex-completion cache
                (when (fboundp 'bibtex-completion-clear-cache)
                  (bibtex-completion-clear-cache))
                (setq success t)))))
      (error
       (message "✗ Error fetching from Zotero: %s" (error-message-string err))
       (setq success nil)))
    
    ;; Fallback: try manual source if connection failed
    (unless success
      (my/zotero-use-manual-source))
    
    success))

;;;###autoload
(defun my/zotero-use-manual-source ()
  "Copy bibliography from manual source file (fallback for WSL/offline).
Uses my/zotero-manual-bib-source if set."
  (interactive)
  (if (and my/zotero-manual-bib-source
           (file-exists-p (expand-file-name my/zotero-manual-bib-source)))
      (let ((source (expand-file-name my/zotero-manual-bib-source))
            (target (my/zotero-bib-file)))
        (message "Using manual bibliography source: %s" source)
        ;; Ensure directory exists
        (let ((dir (file-name-directory target)))
          (unless (file-directory-p dir)
            (make-directory dir t)))
        ;; Copy file
        (copy-file source target t)
        (message "✓ Bibliography copied from %s to %s" source target)
        (message "  You can now use orb-insert-link!")
        ;; Refresh bibtex-completion cache
        (when (fboundp 'bibtex-completion-clear-cache)
          (bibtex-completion-clear-cache))
        t)
    (progn
      (message "✗ No manual bibliography source configured")
      (message "  Set my/zotero-manual-bib-source to a file path, e.g.:")
      (message "  (setq my/zotero-manual-bib-source \"~/org/roam/My Library.biblatex\")")
      nil)))

;; ============================================================================
;; UTILITY COMMANDS
;; ============================================================================

;;;###autoload
(defun my/zotero-test-connection ()
  "Test connection to Zotero Better BibTeX."
  (interactive)
  (condition-case err
      (let* ((test-url (format "http://127.0.0.1:%d/better-bibtex/cayw?probe=true" 
                              my/zotero-bbt-port))
             (buf (url-retrieve-synchronously test-url t nil my/zotero-timeout)))
        (if (not (buffer-live-p buf))
            (message "✗ Cannot connect to Zotero on port %d (timeout)" my/zotero-bbt-port)
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "^$" nil 'move)
            (forward-line)
            (let ((response (buffer-substring (point) (point-max))))
              (kill-buffer)
              (if (string-match-p "ready" response)
                  (message "✓ Zotero Better BibTeX is accessible on port %d" my/zotero-bbt-port)
                (message "✗ Zotero responded but Better BibTeX may not be ready"))))))
    (error
     (message "✗ Cannot connect to Zotero on port %d: %s" 
              my/zotero-bbt-port (error-message-string err))
     (message "  Make sure Zotero is running and Better BibTeX is installed"))))

;; ============================================================================
;; AUTO-FETCH ON STARTUP
;; ============================================================================

;; Auto-refresh Zotero library on first idle (if enabled)
(when my/zotero-auto-fetch-on-startup
  (run-with-idle-timer 5 nil
    (lambda ()
      (when (and (not my/zotero-auto-fetched)
                 (file-directory-p (expand-file-name "~/org/resources/roam")))
        (setq my/zotero-auto-fetched t)
        (message "Auto-fetching Zotero library in background...")
        (my/zotero-refresh)))))

(provide 'zotero-bibtex)
;;; zotero-bibtex.el ends here
