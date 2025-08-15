;;; welcome.el --- Startup cheatsheet and reading dashboard -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'reading-tracker)  ;; Provides my/org-reading-file, collectors, helpers
(require 'org)               ;; org-entry-get, etc.
(require 'org-table)         ;; org-table-align

;; ============================================================================
;; STARTUP CONFIGURATION
;; ============================================================================

;; Don't show the GNU splash or a scratch message
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; ============================================================================
;; CHEATSHEET CONFIGURATION
;; ============================================================================

;; Curate your key cheatsheet here (add/remove as you like).
;; Format: either (:section "Title") or ("KEYS" "Description" COMMAND-or-nil)
(defvar my/cheatsheet-keys
  '((:section "Org & Agenda")
    ("C-c a"   "Open agenda"                         org-agenda)
    ("C-c A"   "Refresh agenda"                      my/refresh-agenda)
    ("C-c i"   "Go to inbox"                         my/goto-inbox)
    ("C-c c"   "Capture menu"                        org-capture)
    ("C-c C-w" "Refile item"                         org-refile)
    ("C-c d"   "Go to /org"                    (lambda () (interactive) (dired org-directory)))
    ("C-c e"   "Go to .emacs.d"                      (lambda () (interactive) (dired user-emacs-directory)))
    ("C-c C-x a" "Archive file"                      my/org-archive-file)

    (:section "Reading tracker")
    ("C-c r o" "Open reading.org"                    my/org-reading-open)
    ("C-c r b" "Open books.org"                      my/org-reading-open-books)
    ("C-c r n" "Open book notes"                     my/org-reading-open-book-notes)
    ("C-c r a" "Add a book"                          my/org-reading-add-book)
    ("C-c r u" "Update current page"                 my/org-reading-set-current-page)
    ("C-c r c" "Complete a book"                     my/org-reading-complete-book)
    ("C-c r d" "Delete a book"                       my/org-reading-delete-book)

    (:section "Git Sync")
    ("C-c g g" "Git status (org)"                    my/magit-org-status)
    ("C-c g s" "Sync org to remote now"              my/org-sync-now)
    ("C-c g t" "Toggle auto-sync"                    my/org-toggle-auto-sync)
    ("C-x g"   "Magit status (any repo)"             magit-status)

    (:section "Spell Checking")
    ("C-c s c" "Correct word"                        jinx-correct)
    ("C-c s b" "Correct buffer"                      my/jinx-correct-buffer)
    ("C-c s l" "Switch language"                     jinx-languages)
    ("C-c s n" "Next misspelling"                    jinx-next)
    ("C-c s p" "Previous misspelling"                jinx-previous)
    ("M-x my/jinx-switch-to-english" "English only"  my/jinx-switch-to-english)
    ("M-x my/jinx-switch-to-finnish" "Finnish only"  my/jinx-switch-to-finnish)
    ("M-x my/jinx-switch-to-bilingual" "EN + FI"     my/jinx-switch-to-bilingual))
  "Rows for the startup cheatsheet buffer.")

;; ============================================================================
;; CHEATSHEET MODE
;; ============================================================================

;; A tiny major mode for the cheatsheet buffer
(define-derived-mode my/cheatsheet-mode special-mode "Personal Workspace"
  "Mode for the personal productivity workspace dashboard.
Shows reading progress, key bindings, and quick access to important functions."
  (read-only-mode 1)
  (setq truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1)
  (orgtbl-mode 1)
  (define-key my/cheatsheet-mode-map (kbd "g") #'my/cheatsheet-refresh)
  (define-key my/cheatsheet-mode-map (kbd "q") #'quit-window))

;; ============================================================================
;; CHEATSHEET RENDERING
;; ============================================================================

(defun my/cheatsheet--insert-line (key desc cmd)
  "Insert one cheatsheet row. CMD parameter is ignored.
Creates a formatted row with key and description only."
  (let ((col-key 18)    ;; Width for Key column
        (col-desc 42))  ;; Width for Description column
    (insert (format (format "%%-%ds %%s" col-key) key desc))
    (insert "\n")))

;; ============================================================================
;; READING DASHBOARD INTEGRATION
;; ============================================================================

;; Reading dashboard on Welcome page (reusing reading-tracker.el)

(defun my/welcome--visit-marker (mk)
  "Jump to book at marker MK in reading.org.
Switches to the reading file and shows the specific book entry."
  (interactive)
  (switch-to-buffer (marker-buffer mk))
  (goto-char mk)
  (org-show-entry))

(defun my/welcome--collect-reading-rows ()
  "Return list of (TITLE LEFT PCT-STR PPD-STR) using reading-tracker helpers.
Collects reading progress data for display in the welcome dashboard."
  (let (rows)
    (dolist (it (my/org--collect-reading-headings))
      (let* ((mk    (cdr it))
             (title (car it)))
        (with-current-buffer (marker-buffer mk)
          (org-with-wide-buffer
            (goto-char mk)
            (let* ((total     (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0")))
                   (pages-read (string-to-number (or (org-entry-get (point) "CURRENT_PAGE") "0")))
                   (left      (max 0 (- total pages-read)))
                   (pct       (if (> total 0) (* 100.0 (/ pages-read (float total))) 0.0))
                   (dleft     (my/org--days-left-at-point))
                   (ppd       (and (> left 0) (numberp dleft)
                                   (ceiling (/ (float left) dleft))))
                   (avg-ppd   (my/org--average-pages-per-day-at-point)))
              (when (> total 0)
                (push (list
                       ;; Avoid breaking the table if title contains '|'
                       (replace-regexp-in-string "|" "/" title)
                       left
                       (format "%.1f" pct)
                       (if ppd (number-to-string ppd) "—")
                       (if avg-ppd (format "%.1f" avg-ppd) "—"))
                      rows)))))))
    (nreverse rows)))

(defun my/welcome--insert-reading-dashboard ()
  "Insert an Org-style table dashboard and align it.
Creates a reading progress table showing title, pages left, progress %, and pages/day needed."
  (let ((rows (my/welcome--collect-reading-rows)))
    (insert "\n📖 READING PROGRESS\n")
    (insert "═══════════════════\n\n")
    (if (null rows)
        (progn
          (insert "No books in progress yet\n")
          (insert "Use C-c r a to add your first book\n\n"))
      (progn
        (insert (format "%-30s %6s %8s %10s %8s\n" "Title" "Left" "Progress" "Pages/day" "Avg/day"))
        (insert (make-string 30 ?-) " " (make-string 6 ?-) " " (make-string 8 ?-) " " (make-string 10 ?-) " " (make-string 8 ?-) "\n")
        (dolist (r rows)
          (insert (format "%-30s %6d %7s%% %10s %8s\n"
                          (truncate-string-to-width (nth 0 r) 30 nil nil "…")
                          (nth 1 r) 
                          (nth 2 r) 
                          (nth 3 r)
                          (nth 4 r))))))))

;; ============================================================================
;; MAIN CHEATSHEET FUNCTIONS
;; ============================================================================

(defun my/cheatsheet--render ()
  "Render the cheatsheet content in the current buffer.
Creates the complete welcome page with reading dashboard and key bindings."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "📚 Personal Workspace\n")
    (insert "═════════════════════\n\n") 
    (insert (propertize "Productivity system based on PARA methodology + GTD principles\n" 
                        'face '(:foreground "dim gray")))
    (insert (propertize "Press 'g' to refresh • 'q' to close\n" 
                        'face '(:foreground "dim gray")))
       
    (my/welcome--insert-reading-dashboard)
    (insert "\n")
    
    ;; Split key bindings into sections
    (let ((org-section '())
          (reading-section '())
          (git-section '())
          (spell-section '())
          (current-section nil))
      
      ;; Separate the sections
      (dolist (row my/cheatsheet-keys)
        (cond
         ((and (consp row) (keywordp (car row)) (eq (car row) :section))
          (setq current-section (cadr row)))
         ((string-equal current-section "Org & Agenda")
          (push row org-section))
         ((string-equal current-section "Reading tracker")
          (push row reading-section))
         ((string-equal current-section "Git Sync")
          (push row git-section))
         ((string-equal current-section "Spell Checking")
          (push row spell-section))))
      
      ;; Reverse to maintain original order
      (setq org-section (nreverse org-section))
      (setq reading-section (nreverse reading-section))
      (setq git-section (nreverse git-section))
      (setq spell-section (nreverse spell-section))
      
      ;; Insert 2x2 grid layout
      (insert "Key Bindings\n")
      (insert "════════════\n\n")
      (insert (format "%-40s %s\n" "ORG & AGENDA" "READING TRACKER"))
      (insert (format "%-40s %s\n" (make-string 12 ?-) (make-string 15 ?-)))
      
      ;; Print org and reading sections side by side (top row)
      (let ((max-rows (max (length org-section) (length reading-section))))
        (dotimes (i max-rows)
          (let ((org-row (nth i org-section))
                (reading-row (nth i reading-section)))
            (insert (format "%-40s %s\n"
                            (if org-row
                                (format "%-15s %s" (nth 0 org-row) (nth 1 org-row))
                              "")
                            (if reading-row
                                (format "%-15s %s" (nth 0 reading-row) (nth 1 reading-row))
                              ""))))))
      
      ;; Add Git Sync and Spell Checking sections below (bottom row)
      (insert "\n")
      (insert (format "%-40s %s\n" "GIT SYNC" "SPELL CHECKING"))
      (insert (format "%-40s %s\n" (make-string 8 ?-) (make-string 14 ?-)))
      
      (let ((max-rows (max (length git-section) (length spell-section))))
        (dotimes (i max-rows)
          (let ((git-row (nth i git-section))
                (spell-row (nth i spell-section)))
            (insert (format "%-40s %s\n"
                            (if git-row
                                (format "%-15s %s" (nth 0 git-row) (nth 1 git-row))
                              "")
                            (if spell-row
                                (format "%-15s %s" (nth 0 spell-row) (nth 1 spell-row))
                              "")))))))
    
    (goto-char (point-min))))

(defun my/cheatsheet-show ()
  "Display the welcome cheatsheet in a new buffer.
Creates or switches to the welcome buffer and renders the content."
  (interactive)
  (let ((buf (get-buffer-create "*Personal Workspace*")))
    (with-current-buffer buf
      (my/cheatsheet-mode)
      (my/cheatsheet--render))
    (switch-to-buffer buf)))     ;; Use the current window

(defun my/cheatsheet-refresh ()
  "Re-render the cheatsheet buffer.
Updates the content with current reading progress and key bindings."
  (interactive)
  (when (derived-mode-p 'my/cheatsheet-mode)
    (my/cheatsheet--render)))

;; ============================================================================
;; STARTUP INTEGRATION
;; ============================================================================

;; Show welcome page on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (my/cheatsheet-show)
            (delete-other-windows)))  ;; Ensure a single window

;; Global shortcuts for common directories
(global-set-key (kbd "C-c d") (lambda () (interactive) (dired org-directory)))
(global-set-key (kbd "C-c e") (lambda () (interactive) (dired user-emacs-directory)))
(global-set-key (kbd "C-c h") #'my/cheatsheet-show)

(provide 'welcome)
