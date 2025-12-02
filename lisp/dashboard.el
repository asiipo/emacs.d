;;; dashboard.el --- Consolidated dashboard functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a consolidated dashboard interface for the personal
;; workspace, including reading progress, key bindings, and system status.

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)
(require 'org-table)
(require 'subr-x)

;; ============================================================================
;; INBOX STATUS DISPLAY
;; ============================================================================

(defconst dashboard--seconds-per-day 86400
  "Number of seconds in a day (24 * 60 * 60).")

(defconst dashboard--title-max-length 40
  "Maximum length for book titles before truncation.")

(defvar dashboard-upcoming-events-days 14
  "Number of days ahead to show upcoming events in the dashboard.")

(defun dashboard--with-inbox-file (func)
  "Execute FUNC with inbox.org buffer if it exists.
FUNC is called with the buffer in org-mode. Returns result of FUNC or nil if file doesn't exist."
  (let ((inbox-file (expand-file-name "inbox.org" org-directory)))
    (when (file-exists-p inbox-file)
      (with-temp-buffer
        (insert-file-contents inbox-file)
        (org-mode)
        (funcall func)))))

(defun dashboard--count-inbox-items ()
  "Count unprocessed TODO items in inbox.org under * Tasks heading."
  (or (dashboard--with-inbox-file
       (lambda ()
         (let ((count 0))
           (goto-char (point-min))
           ;; Find the "* Tasks" heading
           (when (re-search-forward "^\\* Tasks\\b" nil t)
             ;; Count TODO items only within the Tasks subtree
             (let ((tasks-end (save-excursion
                               (org-end-of-subtree t t)
                               (point))))
               (while (re-search-forward org-todo-line-regexp tasks-end t)
                 (when (member (match-string 2) '("TODO" "NEXT" "WAIT"))
                   (setq count (1+ count))))))
           count)))
      0))

(defun dashboard--get-upcoming-events ()
  "Get events from inbox.org * Events heading scheduled in the next N days.
N is defined by `dashboard-upcoming-events-days'."
  (dashboard--with-inbox-file
   (lambda ()
     (let ((events '())
           (now (current-time))
           ;; Calculate start of today (midnight)
           (seconds-today (* (string-to-number (format-time-string "%H" (current-time))) 3600))
           (today-start (time-subtract (current-time) 
                                      (+ (* (string-to-number (format-time-string "%H" (current-time))) 3600)
                                         (* (string-to-number (format-time-string "%M" (current-time))) 60)
                                         (string-to-number (format-time-string "%S" (current-time))))))
           (days-later (time-add (current-time) (* dashboard-upcoming-events-days dashboard--seconds-per-day))))
       (goto-char (point-min))
       ;; Find the "* Events" heading
       (when (re-search-forward "^\\* Events\\b" nil t)
         (let ((events-start (point))
               (events-end (save-excursion
                            (org-end-of-subtree t t)
                            (point))))
           ;; Find all ** and *** sub-headlines in Events section
           (goto-char events-start)
           (while (re-search-forward "^\\(\\*\\{2,3\\}\\) \\(.+\\)$" events-end t)
             (let* ((level (length (match-string 1)))
                    (title (string-trim (match-string 2)))
                    (entry-end (save-excursion
                                (or (and (outline-next-heading) (point))
                                    events-end)))
                    (scheduled nil)
                    (parent-title nil))
               ;; Get parent title for level 3 headlines
               (when (= level 3)
                 (save-excursion
                   (when (re-search-backward "^\\*\\* \\(.+\\)$" events-start t)
                     (setq parent-title (string-trim (match-string 1))))))
               ;; Look for SCHEDULED in the entry content
               (save-excursion
                 (when (re-search-forward "SCHEDULED: *<\\([^>]+\\)>" entry-end t)
                   (setq scheduled (match-string 1))))
               (when scheduled
                 (let ((sched-time (org-time-string-to-time (concat "<" scheduled ">")))
                       (full-title (if parent-title
                                      (format "%s → %s" parent-title title)
                                    title)))
                   ;; Show events from today onwards (compare with midnight, not current time)
                   (when (and (time-less-p today-start sched-time)
                             (time-less-p sched-time days-later))
                     (push (cons full-title sched-time) events))))))))
       ;; Sort by time
       (sort events (lambda (a b) (time-less-p (cdr a) (cdr b))))))))

(defun dashboard--insert-inbox-status ()
  "Insert inbox status with item count and upcoming events."
  (let ((count (dashboard--count-inbox-items))
        (events (dashboard--get-upcoming-events)))
    (insert "\n📥 INBOX STATUS\n")
    (insert (make-string 50 ?─) "\n")
    
    ;; Tasks count
    (cond
     ((= count 0)
      (insert "  ✅ Inbox clear — Great job!\n"))
     ((< count 5)
      (insert (format "  ⚡ %d task%s to process\n" count (if (= count 1) "" "s"))))
     ((< count 10)
      (insert (format "  ⚠️  %d tasks — Time to process!\n" count)))
     (t
      (insert (format "  🚨 %d tasks — Inbox needs attention!\n" count))))
    
    ;; Upcoming events
    (if events
        (progn
          (insert (format "\n  📅 Upcoming Events (next %d days):\n" dashboard-upcoming-events-days))
          ;; Calculate time boundaries once for all events
          (let* ((now (current-time))
                 (seconds-today (* (string-to-number (format-time-string "%H" now)) 3600))
                 (today-start (time-subtract now (+ seconds-today
                                                    (* (string-to-number (format-time-string "%M" now)) 60)
                                                    (string-to-number (format-time-string "%S" now)))))
                 (tomorrow-start (time-add today-start dashboard--seconds-per-day))
                 (day-after-start (time-add tomorrow-start dashboard--seconds-per-day)))
            (dolist (event events)
              (let* ((title (car event))
                     (time (cdr event))
                     (is-today (and (time-less-p today-start time)
                                   (time-less-p time tomorrow-start)))
                     (is-tomorrow (and (time-less-p tomorrow-start time)
                                      (time-less-p time day-after-start)))
                     (date-str (format-time-string "%a %b %d" time))
                     (time-str (format-time-string "%H:%M" time)))
                ;; Insert with color based on day
                (insert "    • ")
                (cond
                 (is-today
                  (insert (propertize (format "%s %s" date-str time-str) 
                                     'face 'success
                                     'font-lock-face 'success)))
                 (is-tomorrow
                  (insert (propertize (format "%s %s" date-str time-str) 
                                     'face 'warning
                                     'font-lock-face 'warning)))
                 (t
                  (insert (format "%s %s" date-str time-str))))
                (insert (format " — %s\n" title))))))
      (insert (format "\n  No events scheduled in the next %d days\n" dashboard-upcoming-events-days)))
    
    ))

;; ============================================================================
;; READING PROGRESS DISPLAY
;; ============================================================================

(defun dashboard--insert-reading-progress ()
  "Insert reading progress into dashboard."
  (when (fboundp 'my/reading-get-current-books)
    (let ((books (my/reading-get-current-books)))
      (if books
          (progn
            (insert "\n📚 CURRENT READING\n")
            (insert (make-string 50 ?─) "\n")
            (dolist (book books)
              (let ((title (nth 0 book))
                    (author (nth 1 book))
                    (current (nth 2 book))
                    (total (nth 3 book))
                    (progress (nth 4 book))
                    (daily-target (nth 5 book)))
                (insert (format "  %s\n" 
                               (if (> (length title) dashboard--title-max-length) 
                                   (concat (substring title 0 (- dashboard--title-max-length 3)) "...")
                                 title)))
                (insert (format "  by %s | %d/%d pages (%.0f%%)%s\n" 
                               author current total progress
                               (if daily-target
                                   (format " | Target: %.1f pages/day" daily-target)
                                 "")))))
            (insert "\n"))
        (insert "\n📚 No books currently being read\n\n")))))


;; ============================================================================
;; DASHBOARD MODE
;; ============================================================================

;; Hook run when dashboard is refreshed
(defvar dashboard-refresh-hook nil
  "Hook run when the dashboard is refreshed.")

;; A major mode for the dashboard buffer
(define-derived-mode dashboard-mode special-mode "Personal Workspace"
  "Mode for the personal productivity workspace dashboard.
Shows reading progress, time tracking, key bindings, and quick access to important functions."
  (read-only-mode 1)
  (setq truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1)
  (orgtbl-mode 1)
  (define-key dashboard-mode-map (kbd "g") #'dashboard-refresh)
  (define-key dashboard-mode-map (kbd "q") #'quit-window))

;; ============================================================================
;; CHEATSHEET CONFIGURATION
;; ============================================================================

;; Curate your key cheatsheet here (add/remove as you like).
;; Format: either (:section "Title") or ("KEYS" "Description" COMMAND-or-nil)
(defvar dashboard-cheatsheet-keys
  '(("Daily Essentials" (("C-c a" "Org agenda")
                          ("C-c c" "Org capture")
                          ("C-c G" "GTD menu")
                          ("C-c i" "View inbox")
                          ("C-c s" "Someday/Maybe")
                          ("C-c A" "Refresh agenda")
                          ("C-c C-w" "Refile current entry")
                          ("C-c d" "Open org workspace")
                          ))

    ("Org-Roam & Research" (("C-c n f" "Find/create note")
                             ("C-c n c" "Capture roam note")
                             ("C-c n i" "Insert note link")
                             ("C-c n b" "Toggle roam buffer")
                             ("C-c n o" "Open roam directory")
                             ("C-c z i" "Insert citation link")
                             ("C-c z c" "Check bibliography")
                             ("C-c z o" "Open bibliography")))

    ("Reading & Journal" (("C-c r o" "Open reading tracker")
                           ("C-c r a" "Add book")
                           ("C-c r u" "Update progress")
                           ("C-c r c" "Complete book")
                           ("C-c r d" "Set book deadline")
                           ("C-c j" "Journal capture")))

    ("System & Utilities" (("C-c g g" "Magit status (org dir)")
                            ("C-c g s" "Sync org repository")
                            ("C-c g t" "Toggle auto-sync")
                            ("C-x g" "Magit status (prompt)")
                            ("C-c e" "Open Emacs config")
                            ("C-;" "Jinx correct word")
                            ("C-M-;" "Switch Jinx languages")
                            ("C-c l" "Store org link")
                            ("C-c C-l" "Insert org link")
                            ("C-c D" "Diagnose configuration")
                            ("C-c T" "Show module timing")))))

;; ============================================================================
;; GIT STATUS HELPER
;; ============================================================================

(defun dashboard--format-git-status ()
  "Return formatted git status indicator string."
  (let ((org-dir (or org-directory (expand-file-name "~/org"))))
    (if (and (file-directory-p org-dir)
             (file-exists-p (expand-file-name ".git" org-dir)))
        (let ((default-directory org-dir))
          (condition-case nil
              (if (string-empty-p (string-trim (shell-command-to-string "git status --porcelain")))
                  "Git ✅"
                "Git ⚠️")
            (error "Git ❓")))
      "Git —")))

;; ============================================================================
;; CHEATSHEET RENDERING
;; ============================================================================

(defun dashboard--render-keybindings ()
  "Render the key bindings section in a 2x2 grid layout."
  ;; Split key bindings into sections
  (let ((daily-section '())
        (roam-section '())
        (reading-section '())
        (system-section '())
        (sections-list dashboard-cheatsheet-keys))
    
    ;; Extract sections directly from the updated structure
    (setq daily-section (cadr (assoc "Daily Essentials" sections-list)))
    (setq roam-section (cadr (assoc "Org-Roam & Research" sections-list)))
    (setq reading-section (cadr (assoc "Reading & Journal" sections-list)))
    (setq system-section (cadr (assoc "System & Utilities" sections-list)))
    
    ;; Insert 2x2 grid layout
    (insert "Key Bindings\n")
    (insert "════════════\n\n")
    (insert (format "%-40s %s\n" "DAILY ESSENTIALS" "ORG-ROAM & RESEARCH"))
    (insert (format "%-40s %s\n" (make-string 16 ?-) (make-string 20 ?-)))
    
    ;; Print daily and roam sections side by side (top row)
    (let ((max-rows (max (length daily-section) (length roam-section))))
      (dotimes (i max-rows)
        (let ((daily-row (nth i daily-section))
              (roam-row (nth i roam-section)))
          (insert (format "%-40s %s\n"
                          (if daily-row
                              (format "%-15s %s" (nth 0 daily-row) (nth 1 daily-row))
                            "")
                          (if roam-row
                              (format "%-15s %s" (nth 0 roam-row) (nth 1 roam-row))
                            ""))))))
    
    ;; Add Reading & Journal and System & Utilities sections below (bottom row)
    (insert "\n")
    (insert (format "%-40s %s\n"
            "READING & JOURNAL"
            "SYSTEM & UTILITIES"))
    (insert (format "%-40s %s\n" (make-string 17 ?-) (make-string 18 ?-)))
    
    (let ((max-rows (max (length reading-section) (length system-section))))
      (dotimes (i max-rows)
        (let ((reading-row (nth i reading-section))
              (system-row (nth i system-section)))
          (insert (format "%-40s %s\n"
                          (if reading-row
                              (format "%-15s %s" (nth 0 reading-row) (nth 1 reading-row))
                            "")
                          (if system-row
                              (format "%-15s %s" (nth 0 system-row) (nth 1 system-row))
                            ""))))))))

;; ============================================================================
;; MAIN DASHBOARD FUNCTIONS
;; ============================================================================

(defun dashboard-render ()
  "Render the complete dashboard content in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "📚 Personal Workspace — %s\n" 
                    (format-time-string "%Y-%m-%d %H:%M")))
    (insert "═════════════════════\n") 
    (insert (propertize (format "Press 'g' to refresh • 'q' to close • %s • %s\n" 
                                (my/format-startup-time)
                                (dashboard--format-git-status))
                        'face '(:foreground "dim gray")))
    
    ;; Insert inbox status
    (dashboard--insert-inbox-status)
    
    ;; Insert reading progress
    (dashboard--insert-reading-progress)
    
    (insert "\n")
    (dashboard--render-keybindings)
    (goto-char (point-min))))

(defun dashboard-show ()
  "Display the dashboard in a new buffer.
Creates or switches to the dashboard buffer and renders the content."
  (interactive)
  (let ((buf (get-buffer-create "*Personal Workspace*")))
    (with-current-buffer buf
      (dashboard-mode)
      (dashboard-render))
    (switch-to-buffer buf)))

(defun dashboard-refresh ()
  "Re-render the dashboard buffer.
Updates the content with current reading progress, time tracking, and key bindings."
  (interactive)
  (when (derived-mode-p 'dashboard-mode)
    (run-hooks 'dashboard-refresh-hook)
    (dashboard-render)))

;; ============================================================================
;; PUBLIC ALIASES & STARTUP CONFIGURATION
;; ============================================================================

;; Public aliases for backward compatibility
(defalias 'my/cheatsheet-show 'dashboard-show
  "Display the personal workspace dashboard.")

(defalias 'my/cheatsheet-refresh 'dashboard-refresh
  "Refresh the personal workspace dashboard.")

(defalias 'my/cheatsheet-mode 'dashboard-mode
  "Dashboard mode for the personal workspace.")

;; Show dashboard on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (dashboard-show)
            (delete-other-windows)))  ;; Ensure a single window

(provide 'dashboard)
