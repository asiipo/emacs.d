;;; path-utils.el --- Cross-platform path utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Cross-platform path management for essential tools (LaTeX, Git).

;;; Code:

(defun my/add-to-exec-path (paths)
  "Add PATHS to exec-path and PATH if they exist."
  (let ((separator (if (eq system-type 'windows-nt) ";" ":")))
    (dolist (path paths)
      (when (and path 
                 (file-directory-p path)
                 (not (member path exec-path)))
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat (getenv "PATH") separator path))))))

(defun my/find-texlive-paths ()
  "Find TeX Live paths for current system."
  (let ((base-patterns (cond
                        ((eq system-type 'darwin)
                         '("/usr/local/texlive/*/bin/universal-darwin"
                           "/usr/local/texlive/*/bin/x86_64-darwin"))
                        ((eq system-type 'gnu/linux)
                         '("/usr/local/texlive/*/bin/x86_64-linux"))
                        ((eq system-type 'windows-nt)
                         '("C:/texlive/*/bin/win32"
                           "C:/texlive/*/bin/windows")))))
    (when base-patterns
      (apply #'append 
             (mapcar (lambda (pattern)
                       (file-expand-wildcards pattern))
                     base-patterns)))))

(defun my/setup-system-paths ()
  "Set up system-specific paths with TeX Live detection."
  (let ((texlive-paths (my/find-texlive-paths)))
    (cond
     ((eq system-type 'windows-nt)
      (my/add-to-exec-path 
       (append '("C:/Program Files/MiKTeX/miktex/bin/x64"
                 "C:/Program Files/Git/bin")
               texlive-paths)))
     ((eq system-type 'darwin)
      (my/add-to-exec-path 
       (append (list (expand-file-name "~/.juliaup/bin"))
               '("/opt/homebrew/bin" 
                 "/usr/local/bin"
                 "/Library/TeX/texbin")
               texlive-paths)))
     ((eq system-type 'gnu/linux)
      (my/add-to-exec-path 
       (append '("/usr/local/bin"
                 "/usr/bin"
                 "/usr/texbin")
               texlive-paths))))))

(defun my/check-essential-tools ()
  "Check if essential tools are available."
  (interactive)
  (let ((tools '(("LaTeX" . ("pdflatex" "latex"))
                 ("Git" . ("git"))
                 ("Python" . ("python3" "python"))
                 ("Julia" . ("julia"))
                 ("Spell checking" . ("enchant-2" "aspell"))))
        (results '()))
    (dolist (tool-info tools)
      (let ((tool-name (car tool-info))
            (executables (cdr tool-info))
            (found nil))
        (dolist (exe executables)
          (when (and (not found) (executable-find exe))
            (setq found exe)))
        (push (cons tool-name found) results)))
    
    (if (called-interactively-p 'interactive)
        (with-temp-buffer-window "*Path Diagnostics*" nil nil
          (insert "Essential Tools Status:\n")
          (insert "========================\n\n")
          (dolist (result (reverse results))
            (let ((name (car result))
                  (exe (cdr result)))
              (insert (format "%-15s %s\n" 
                            (concat name ":")
                            (if exe 
                                (format "✓ %s" exe)
                              "✗ Not found"))))))
      results)))

(my/setup-system-paths)

(provide 'path-utils)
;;; path-utils.el ends here
