;;; journal.el --- DateTree journal and literature notes -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal journaling helpers in ~/org/areas/.
;;
;; DAILY JOURNAL (journal.org):
;;  - Capture template "j": Journal (today) → DateTree in journal.org
;;  - Command `my/journal-capture-today` (bound to C-c j)
;;  - (Optional) `my/journal-open-today` to jump to today's node
;;
;; LITERATURE NOTES (literature/ folder):
;;  - Capture template "l": Literature → Creates new file per paper
;;  - Each paper gets its own file in areas/literature/
;;  - Filename: Authors-Year-Title.org
;;  - Access via org-capture menu: C-c c → l
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

;; Path to literature notes directory
(defvar my/org-literature-dir 
  (expand-file-name "areas/literature/" org-directory)
  "Directory for individual literature note files.")

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

(defun my/literature--ensure-dir ()
  "Ensure the literature directory exists."
  (unless (file-directory-p my/org-literature-dir)
    (make-directory my/org-literature-dir t)))

(defun my/literature--sanitize-filename (string)
  "Convert STRING to a safe filename component."
  (let ((s (replace-regexp-in-string "[^[:alnum:] -]" "" string)))
    (replace-regexp-in-string "[ ]+" "-" (string-trim s))))

(defun my/literature--capture-file ()
  "Generate filename for literature capture based on user input.
Prompts for authors, year, and title, then creates filename."
  (my/literature--ensure-dir)
  (let* ((authors (read-string "Authors: "))
         (year (read-string "Year: "))
         (title (read-string "Title: "))
         (filename (format "%s-%s.org"
                          (my/literature--sanitize-filename authors)
                          year)))
    ;; Store in temp variables for template expansion
    (setq my/literature--temp-authors authors
          my/literature--temp-year year
          my/literature--temp-title title)
    (expand-file-name filename my/org-literature-dir)))

;; Temporary variables for capture template
(defvar my/literature--temp-authors nil)
(defvar my/literature--temp-year nil)
(defvar my/literature--temp-title nil)

;; ============================================================================
;; CAPTURE TEMPLATE INTEGRATION
;; ============================================================================

;; Add journal and literature capture templates to existing templates
;; Based on Org Mode Guide section 9.1
(with-eval-after-load 'org
  (my/journal--ensure-file)
  (my/literature--ensure-dir)
  (let ((journal-template '("j" "Journal (today)" entry
                           (file+datetree my/org-journal-file)
                           "* %<%H:%M> %?\n"))
        (literature-template '("l" "Literature" plain
                              (file my/literature--capture-file)
                              "#+TITLE: %(identity my/literature--temp-title)\n#+AUTHORS: %(identity my/literature--temp-authors)\n#+YEAR: %(identity my/literature--temp-year)\n#+DATE: %U\n#+SEQ_TODO: QUESTION(q) | ANSWERED(a)\n\n* Notes\n\n"
                              :empty-lines 0
                              :jump-to-captured t)))
    ;; Avoid duplicate templates
    (setq org-capture-templates 
          (cons journal-template
                (cons literature-template
                      (cl-remove-if (lambda (tpl) (member (car tpl) '("j" "l")))
                                    (or org-capture-templates '())))))))

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

;; Both journal and literature are Areas; keep them out of agenda
;; - Daily journal: focused on daily personal entries (DateTree)
;; - Literature notes: one file per paper for detailed reading notes

(provide 'journal)

;;; journal.el ends here
