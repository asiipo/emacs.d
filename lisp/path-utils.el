;;; path-utils.el --- Cross-platform path utilities -*- lexical-binding: t; -*-

;; Author: Personal Configuration
;; Version: 1.0  
;; Package-Requires: ((emacs "28.1"))
;; Keywords: utilities, paths, cross-platform

;; This module provides cross-platform path management utilities for
;; consistently setting up system paths across Windows, macOS, and Linux.

;; ============================================================================
;; SYSTEM PATH UTILITIES
;; ============================================================================

(defun my/add-to-exec-path (paths)
  "Add PATHS to exec-path and PATH environment variable.
PATHS should be a list of directory paths to add."
  (let ((separator (if (eq system-type 'windows-nt) ";" ":")))
    (dolist (path paths)
      (when (and path (file-directory-p path))
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat (getenv "PATH") separator path))))))

(defun my/setup-system-paths ()
  "Set up system-specific executable paths."
  (cond
   ;; Windows paths
   ((eq system-type 'windows-nt)
    (my/add-to-exec-path 
     '("C:/Program Files/MiKTeX/miktex/bin/x64"
       "C:/texlive/2023/bin/win32"
       "C:/Program Files/Git/bin")))
   
   ;; macOS paths
   ((eq system-type 'darwin)
    (my/add-to-exec-path 
     '("/opt/homebrew/bin" 
       "/usr/local/bin"
       "/usr/local/texlive/2023/bin/universal-darwin"
       "/Library/TeX/texbin")))
   
   ;; Linux paths
   ((eq system-type 'gnu/linux)
    (my/add-to-exec-path 
     '("/usr/local/bin"
       "/usr/bin"
       "/usr/texbin")))))

;; Initialize paths on load
(my/setup-system-paths)

(provide 'path-utils)
