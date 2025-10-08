;;; journal.el --- Simple DateTree journal -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal journaling helper using a single DateTree file at ~/org/areas/journal.org.
;; Provides:
;;  - Capture template "j": Journal (today) → DateTree in journal.org
;;  - Command `my/journal-capture-today` (bound to C-c j)
;;  - (Optional) `my/journal-open-today` to jump to today's node
;; 
;; Based on Org Mode Guide section 8.1 (Timestamps) and 9.1 (Capture)

;;; Code:

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)
(require 'org-datetree)
(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

;; Path to journal file in Areas directory
(defvar my/org-journal-file (expand-file-name "areas/journal.org" org-directory)
  "Path to the journal file (DateTree) in the Areas directory.")

;; ============================================================================
;; FILE MANAGEMENT
;; ============================================================================

(defun my/journal--ensure-file ()
  "Create the journal file with proper structure if it doesn't exist."
  (when (not (file-exists-p my/org-journal-file))
    ;; Ensure directory exists
    (let ((journal-dir (file-name-directory my/org-journal-file)))
      (unless (file-directory-p journal-dir)
        (make-directory journal-dir t)))
    ;; Create file with title
    (with-temp-file my/org-journal-file
      (insert "#+TITLE: Journal\n"))))

;; ============================================================================
;; CAPTURE TEMPLATE INTEGRATION
;; ============================================================================

;; Add journal capture template to existing templates
;; Based on Org Mode Guide section 9.1
(with-eval-after-load 'org
  (my/journal--ensure-file)
  (let ((journal-template '("j" "Journal (today)" entry
                           (file+datetree my/org-journal-file)
                           "* %<%H:%M> %?\n")))
    ;; Avoid duplicate templates
    (setq org-capture-templates 
          (cons journal-template 
                (cl-remove-if (lambda (tpl) (equal (car tpl) "j")) 
                              (or org-capture-templates '()))))))

;; ============================================================================
;; JOURNAL FUNCTIONS
;; ============================================================================

(defun my/journal-capture-today ()
  "Capture a journal entry into today's DateTree using template 'j'.
Opens the capture interface with the journal template pre-selected."
  (interactive)
  (my/journal--ensure-file)
  (org-capture nil "j"))

(defun my/journal-open-today ()
  "Open the journal file and jump/create today's DateTree node.
Creates the date node if it doesn't exist, then shows the entry."
  (interactive)
  (my/journal--ensure-file)
  (find-file my/org-journal-file)
  (org-datetree-find-date-create (calendar-current-date))
  (org-show-entry))

(defun my/journal-open-file ()
  "Open the journal file."
  (interactive)
  (my/journal--ensure-file)
  (find-file my/org-journal-file))

(defun my/journal-search ()
  "Search through journal entries."
  (interactive)
  (my/journal--ensure-file)
  (let ((default-directory (file-name-directory my/org-journal-file)))
    (if (fboundp 'consult-ripgrep)
        (consult-ripgrep default-directory)
      (if (fboundp 'rg)
          (rg (read-string "Search journal: ") "*.org" default-directory)
        (grep (format "grep -n \"%s\" %s" 
                     (read-string "Search journal: ") 
                     my/org-journal-file))))))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

;; ============================================================================
;; NOTES
;; ============================================================================

;; Journal is an Area; keep it out of agenda and let PARA refile settings apply globally
;; This ensures the journal stays focused on daily entries rather than actionable tasks

(provide 'journal)
