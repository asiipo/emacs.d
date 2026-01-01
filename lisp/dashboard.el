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
(require 'svg-lib nil t)

;; ============================================================================
;; CUSTOMIZATION GROUP
;; ============================================================================

(defgroup dashboard nil
  "Consolidated dashboard interface for personal workspace."
  :group 'applications
  :prefix "dashboard-")

(defcustom dashboard-inbox-file "inbox.org"
  "Path to the inbox.org file (relative to `org-directory').
Contains unprocessed tasks and upcoming events."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-gtd-file "gtd.org"
  "Path to the gtd.org file (relative to `org-directory').
Contains habit tracking data."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-reading-file "areas/reading.org"
  "Path to the reading.org file (relative to `org-directory').
Contains reading progress tracking."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-reading-fetcher-function 'my/reading-get-current-books
  "Function to fetch current reading list.
Should return a list of book plists with :title, :author, :pages, :current-page."
  :type 'function
  :group 'dashboard)

(defcustom dashboard-git-status-cache-ttl 30
  "Time-to-live for git status cache in seconds.
Set to 0 to disable caching."
  :type 'integer
  :group 'dashboard)

;; ============================================================================
;; INBOX STATUS DISPLAY
;; ============================================================================

(defconst dashboard--seconds-per-day 86400
  "Number of seconds in a day (24 * 60 * 60).")

(defcustom dashboard-title-max-length 40
  "Maximum length for book titles before truncation."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-upcoming-events-days 14
  "Number of days ahead to show upcoming events in the dashboard."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-debug-mode nil
  "Enable debug logging for dashboard operations.
When non-nil, logs debug messages to *dashboard-debug* buffer."
  :type 'boolean
  :group 'dashboard)

(defun dashboard--log (format-string &rest args)
  "Log debug message if `dashboard-debug-mode' is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when dashboard-debug-mode
    (let ((msg (apply #'format format-string args)))
      (with-current-buffer (get-buffer-create "*dashboard-debug*")
        (goto-char (point-max))
        (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
        (insert msg "\n")))))

;; Habit tracking constants
(defcustom dashboard-habit-history-days 364
  "Number of days to track for habit history (52 weeks, 1 year)."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-habit-grid-cols 52
  "Number of columns in GitHub-style habit grid (52 weeks, 1 year)."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-habit-grid-rows 7
  "Number of rows in habit grid (Monday through Sunday)."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-habit-cell-size 12
  "Size of each cell in the habit activity graph (in pixels)."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-habit-cell-gap 2
  "Gap between cells in the habit activity graph (in pixels)."
  :type 'integer
  :group 'dashboard)

;; Habit graph color scheme - Face-based (theme-aware automatically)

(defface dashboard-habit-none
  '((((background dark)) :foreground "#2a2a2a")
    (((background light)) :foreground "#f5f5f5"))
  "Face for days with no habit completions."
  :group 'dashboard)

(defface dashboard-habit-level-1
  '((((background dark)) :foreground "#26543d")
    (((background light)) :foreground "#c8e6c9"))
  "Face for days with one habit completion."
  :group 'dashboard)

(defface dashboard-habit-level-2
  '((((background dark)) :foreground "#39853f")
    (((background light)) :foreground "#81c784"))
  "Face for days with two habit completions."
  :group 'dashboard)

(defface dashboard-habit-level-3
  '((((background dark)) :foreground "#50c878")
    (((background light)) :foreground "#2e7d32"))
  "Face for days with three or more habit completions."
  :group 'dashboard)

(defface dashboard-habit-today
  '((((background dark)) :foreground "#000000")
    (((background light)) :foreground "#ffffff"))
  "Face for today's cell (high contrast)."
  :group 'dashboard)

(defface dashboard-habit-grid-stroke
  '((((background dark)) :foreground "#1a1b26")
    (((background light)) :foreground "#ffffff"))
  "Face for grid cell stroke color."
  :group 'dashboard)

(defun dashboard--habit-grid-background ()
  "Background color for the habit activity graph (theme-aware)."
  (face-attribute 'dashboard-habit-grid-stroke :foreground nil t))

(defun dashboard--safe-file-exists-p (file)
  "Safely check if FILE exists, returning nil on any error.
Validates FILE is a non-empty string before checking existence."
  (and file
       (stringp file)
       (not (string-empty-p file))
       (condition-case nil
           (file-exists-p file)
         (error nil))))

(defun dashboard--validate-habit (habit)
  "Validate HABIT data structure.
Returns t if HABIT has valid structure: (TITLE INTERVAL HISTORY NEXT-DUE).
TITLE must be a non-empty string, INTERVAL a positive integer,
HISTORY a list of booleans, and NEXT-DUE a time value or nil."
  (and (listp habit)
       (= (length habit) 4)
       (let ((title (nth 0 habit))
             (interval (nth 1 habit))
             (history (nth 2 habit)))
         (and (stringp title)
              (not (string-empty-p title))
              (integerp interval)
              (> interval 0)
              (listp history)
              (cl-every (lambda (x) (or (eq x t) (eq x nil))) history)))))

;; Keybinding cheatsheet configuration
(defconst dashboard-cheatsheet-keys
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
                            ("C-c T" "Show module timing"))))
  "Keybinding cheatsheet organized by category.
Each entry is (CATEGORY-NAME BINDINGS) where BINDINGS is a list of
\(KEY DESCRIPTION) pairs.")

;; Habit tracking cache
(defvar dashboard--habit-cache nil
  "Cached habit data. Structure: (mod-time . habits-list).")

(defvar dashboard--habit-cache-file nil
  "Path to the habits file used for cache invalidation.
Initialized from `dashboard-gtd-file' at runtime.")

;; Inbox status cache
(defvar dashboard--inbox-cache nil
  "Cached inbox data. Structure: (mod-time count events).")

(defvar dashboard--inbox-cache-file nil
  "Path to the inbox file used for cache invalidation.
Initialized from `dashboard-inbox-file' at runtime.")

;; Memoized file path expansion
(defvar dashboard--inbox-file-expanded nil
  "Cached expanded path to inbox file.")

(defvar dashboard--gtd-file-expanded nil
  "Cached expanded path to GTD file.")

(defvar dashboard--reading-file-expanded nil
  "Cached expanded path to reading file.")

(defun dashboard--get-inbox-file ()
  "Get expanded inbox file path (cached)."
  (or dashboard--inbox-file-expanded
      (setq dashboard--inbox-file-expanded
            (expand-file-name dashboard-inbox-file org-directory))))

(defun dashboard--get-gtd-file ()
  "Get expanded GTD file path (cached)."
  (or dashboard--gtd-file-expanded
      (setq dashboard--gtd-file-expanded
            (expand-file-name dashboard-gtd-file org-directory))))

(defun dashboard--get-reading-file ()
  "Get expanded reading file path (cached)."
  (or dashboard--reading-file-expanded
      (setq dashboard--reading-file-expanded
            (expand-file-name dashboard-reading-file org-directory))))

;; ============================================================================
;; COLUMN LAYOUT UTILITIES
;; ============================================================================

(defun dashboard--truncate-string (str max-length &optional ellipsis)
  "Truncate STR to MAX-LENGTH, adding ELLIPSIS if truncated.
 ELLIPSIS defaults to '...'."
  (let ((ell (or ellipsis "...")))
    (if (<= (length str) max-length)
        str
      (concat (substring str 0 (- max-length (length ell))) ell))))

;; Generic cache validation helper
(defun dashboard--file-cache-valid-p (cache cached-file-var file-path)
  "Check if file-based CACHE is still valid.
CACHE is the cache variable, CACHED-FILE-VAR is the cached file path variable,
FILE-PATH is the actual file path to check against."
  (and cache
       (symbol-value cached-file-var)
       (equal (symbol-value cached-file-var) file-path)
       (file-exists-p file-path)
       (equal (car cache)
              (file-attribute-modification-time
               (file-attributes file-path)))))

(defun dashboard--get-start-of-day (&optional time)
  "Get the start of the day (midnight) for TIME.
If TIME is nil, use current time. Returns a time value."
  (let* ((now (or time (current-time)))
         (decoded (decode-time now))
         (seconds-today (+ (* (decoded-time-hour decoded) 3600)
                          (* (decoded-time-minute decoded) 60)
                          (decoded-time-second decoded))))
    (time-subtract now seconds-today)))

(defun dashboard--with-inbox-file (func)
  "Execute FUNC with inbox buffer if it exists.
FUNC is called with the buffer in org-mode. Returns result of FUNC or nil if file doesn't exist."
  (let ((inbox-file (dashboard--get-inbox-file)))
    (when (file-exists-p inbox-file)
      (with-temp-buffer
        (insert-file-contents inbox-file)
        (org-mode)
        (funcall func)))))

(defun dashboard--inbox-cache-valid-p ()
  "Check if the inbox cache is still valid."
  (let ((inbox-file (dashboard--get-inbox-file)))
    (dashboard--file-cache-valid-p dashboard--inbox-cache
                                   'dashboard--inbox-cache-file
                                   inbox-file)))

(defun dashboard--invalidate-inbox-cache ()
  "Invalidate the inbox cache, forcing a refresh on next access."
  (setq dashboard--inbox-cache nil
        dashboard--inbox-cache-file nil))

(defun dashboard--count-inbox-items ()
  "Count unprocessed TODO items under * Tasks heading."
  (if (dashboard--inbox-cache-valid-p)
      (cadr dashboard--inbox-cache)  ;; Return cached count
    ;; Rebuild cache - parse inbox once for both count and events
    (let* ((inbox-data (dashboard--parse-inbox-data))
           (count (car inbox-data))
           (events (cdr inbox-data))
           (inbox-file (dashboard--get-inbox-file)))
      (when (file-exists-p inbox-file)
        (setq dashboard--inbox-cache-file inbox-file
              dashboard--inbox-cache (list (file-attribute-modification-time
                                           (file-attributes inbox-file))
                                          count
                                          events)))
      count)))

(defun dashboard--parse-inbox-data ()
  "Parse inbox file once and return (count . events).
Extracts both TODO count and upcoming events in a single pass."
  (dashboard--with-inbox-file
   (lambda ()
     (let ((count 0)
           (events '())
           (today-start (dashboard--get-start-of-day))
           (days-later (time-add (current-time) 
                                (* dashboard-upcoming-events-days dashboard--seconds-per-day))))
       
       ;; Parse Tasks section for TODO count
       (goto-char (point-min))
       (when (re-search-forward "^\\* Tasks\\b" nil t)
         (let ((tasks-marker (point-marker)))
           (goto-char tasks-marker)
           (setq count (length
                       (org-map-entries
                        (lambda () (point))
                        "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAIT\""
                        'tree)))))
       
       ;; Parse Events section for upcoming events
       (goto-char (point-min))
       (when (re-search-forward "^\\* Events\\b" nil t)
         (let ((events-start (point)))
           (goto-char events-start)
           (org-map-entries
            (lambda ()
              (let* ((level (org-outline-level))
                     (title (org-get-heading t t t t))
                     (scheduled (org-entry-get nil "SCHEDULED"))
                     (parent-title nil))
                (when (= level 3)
                  (save-excursion
                    (org-up-heading-safe)
                    (setq parent-title (org-get-heading t t t t))))
                (when scheduled
                  (let ((sched-time (org-time-string-to-time scheduled))
                        (full-title (if parent-title
                                       (format "%s → %s" parent-title title)
                                     title)))
                    (when (and (time-less-p today-start sched-time)
                              (time-less-p sched-time days-later))
                      (push (cons full-title sched-time) events))))))
            nil
            'tree)))
       
       ;; Return combined data
       (cons count (sort events (lambda (a b) (time-less-p (cdr a) (cdr b)))))))))

(defun dashboard--get-upcoming-events ()
  "Get events from * Events heading scheduled in the next N days.
N is defined by `dashboard-upcoming-events-days'."
  (if (dashboard--inbox-cache-valid-p)
      (caddr dashboard--inbox-cache)  ;; Return cached events (3rd element)
    ;; Reparse if cache invalid (will update cache)
    (cdr (dashboard--parse-inbox-data))))

(defun dashboard--insert-inbox-status ()
  "Insert inbox status with item count and upcoming events."
  (let ((count (dashboard--count-inbox-items))
        (events (dashboard--get-upcoming-events)))
    (insert "\n📥 INBOX STATUS\n")
    (insert (make-string 50 ?─) "\n")
    
    ;; Tasks count
    (let ((msg (cond
                ((= count 0) "  ✅ Inbox clear — Great job!\n")
                ((< count 5) (format "  ⚡ %d task%s to process\n" count (if (= count 1) "" "s")))
                ((< count 10) (format "  ⚠️  %d tasks — Time to process!\n" count))
                (t (format "  🚨 %d tasks — Inbox needs attention!\n" count)))))
      (insert msg))
    
    ;; Upcoming events
    (if events
        (progn
          (insert (format "\n  📅 Upcoming Events (next %d days):\n" dashboard-upcoming-events-days))
          ;; Calculate time boundaries once for all events
          (let* ((today-start (dashboard--get-start-of-day))
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

(defun dashboard--render-progress-bar (current total &optional width)
  "Render a text-based progress bar for CURRENT out of TOTAL.
WIDTH is the bar width in characters (default 10)."
  (let* ((w (or width 10))
         (filled (round (* w (/ (float current) total))))
         (empty (- w filled)))
    (concat "[" 
            (make-string filled ?=)
            (make-string empty ?.)
            "]")))

(defun dashboard--insert-reading-progress ()
  "Insert reading progress into dashboard as a compact table."
  (when (fboundp dashboard-reading-fetcher-function)
    (let ((books (funcall dashboard-reading-fetcher-function)))
      (if books
          (progn
            (insert "\n📚 CURRENT READING\n")
            (insert (make-string 80 ?─) "\n")
            ;; Table header
            (insert (format "  %-30s | %-20s | %-20s\n" 
                           "Title" "Progress" "Target"))
            (insert (format "  %s-+-%s-+-%s\n"
                           (make-string 30 ?-)
                           (make-string 20 ?-)
                           (make-string 20 ?-)))
            ;; Table rows
            (dolist (book books)
              (let* ((title (nth 0 book))
                     ;; (author (nth 1 book))  ; Author not displayed in table
                     (current (nth 2 book))
                     (total (nth 3 book))
                     (progress (nth 4 book))
                     (daily-target (nth 5 book))
                     (title-display (dashboard--truncate-string title 30))
                     (progress-text (format "%d/%d (%.0f%%)" current total progress))
                     (target-text (if daily-target
                                     (format "%.1f pages/day" daily-target)
                                   "No deadline")))
                (insert (format "  %-30s | %-20s | %-20s\n"
                               title-display
                               progress-text
                               target-text))))
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
;; GIT STATUS HELPER
;; ============================================================================

(defvar dashboard--git-status-cache nil
  "Cached git status. Structure: (timestamp . status-string).")

;; Generic TTL cache validation helper
(defun dashboard--ttl-cache-valid-p (cache ttl)
  "Check if time-based CACHE is still valid within TTL seconds."
  (and cache
       (let ((cache-time (car cache))
             (now (float-time)))
         (< (- now cache-time) ttl))))

(defun dashboard--git-status-cache-valid-p ()
  "Check if git status cache is still valid."
  (dashboard--ttl-cache-valid-p dashboard--git-status-cache
                                dashboard-git-status-cache-ttl))

(defun dashboard--format-git-status ()
  "Return formatted git status indicator string.
Uses caching to avoid blocking on git operations.
Cache expires after `dashboard-git-status-cache-ttl' seconds."
  (dashboard--log "Checking git status (cached: %s)" (dashboard--git-status-cache-valid-p))
  (if (dashboard--git-status-cache-valid-p)
      (progn
        (dashboard--log "Using cached git status")
        (cdr dashboard--git-status-cache))
    ;; Cache invalid or missing - refresh
    (let ((org-dir (or org-directory (expand-file-name "~/org")))
          (status "Git —"))
      (when (and (file-directory-p org-dir)
                 (dashboard--safe-file-exists-p (expand-file-name ".git" org-dir)))
        (let ((default-directory org-dir))
          (condition-case err
              (with-timeout (2 "Git ⏱️")
                (setq status
                      (if (string-empty-p (string-trim (shell-command-to-string "git status --porcelain 2>/dev/null")))
                          "Git ✅"
                        "Git ⚠️"))
                (dashboard--log "Git check completed: %s" status))
            (error
             (setq status "Git ❓")
             (dashboard--log "Git check error: %s" err)))))
      ;; Cache the result
      (setq dashboard--git-status-cache (cons (float-time) status))
      status)))

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
;; HABIT TRACKING
;; ============================================================================

(defun dashboard--habit-cache-valid-p ()
  "Check if the habit cache is still valid.
Returns t if cache exists and habits file hasn't been modified since caching."
  (let ((gtd-file (dashboard--get-gtd-file)))
    (dashboard--file-cache-valid-p dashboard--habit-cache
                                   'dashboard--habit-cache-file
                                   gtd-file)))

(defun dashboard--invalidate-habit-cache ()
  "Invalidate the habit cache, forcing a refresh on next access."
  (setq dashboard--habit-cache nil
        dashboard--habit-cache-file nil))

(defun dashboard--normalize-timestamp-for-org-day (timestamp)
  "Normalize TIMESTAMP to account for org-extend-today-until.
Returns a time value representing the 'org day' for the timestamp.
If timestamp is in early morning hours (before org-extend-today-until),
it's considered part of the previous calendar day."
  (let* ((extend-until (or (bound-and-true-p org-extend-today-until) 0))
         (hour (string-to-number (format-time-string "%H" timestamp))))
    (if (and (> extend-until 0) (< hour extend-until))
        ;; Early morning - belongs to previous day
        (time-subtract timestamp 86400)
      timestamp)))

(defun dashboard--get-habit-completion-dates (&optional cutoff-time)
  "Parse LOGBOOK for the current entry and return a list of completion dates.
Returns a list of strings in YYYY-MM-DD format.
Respects org-extend-today-until setting.
If CUTOFF-TIME is provided, ignore entries older than this time."
  (let ((dates '()))
    (save-excursion
      (let ((end (save-excursion (org-end-of-subtree t t) (point))))
        (when (re-search-forward ":LOGBOOK:" end t)
          (while (re-search-forward "- State \"DONE\".*?\\[\\([^]]+\\)\\]" end t)
            (let* ((log-timestamp-str (match-string 1))
                   (log-timestamp (org-time-string-to-time log-timestamp-str)))
              ;; Optimization: Skip if older than cutoff
              (when (or (not cutoff-time)
                        (time-less-p cutoff-time log-timestamp))
                (let ((log-date-str (format-time-string "%Y-%m-%d"
                                                        (dashboard--normalize-timestamp-for-org-day log-timestamp))))
                  (push log-date-str dates))))))))
    dates))

(defun dashboard--get-habit-interval-days ()
  "Return the repeater interval in days for the habit at point.
For habits with min/max format (.+1d/3d), returns the maximum interval.
Defaults to 1 day if no repeater is found or if parsing fails."
  (condition-case nil
      (let ((sched (org-entry-get nil "SCHEDULED")))
        (cond
         ;; Match min/max format: .+1d/3d
         ((and sched (string-match "\\([.+]+\\)\\([0-9]+\\)\\([dwmy]\\)/\\([0-9]+\\)\\([dwmy]\\)" sched))
          (let ((max-val (string-to-number (match-string 4 sched)))
                (max-unit (match-string 5 sched)))
            (pcase max-unit
              ("d" max-val)
              ("w" (* max-val 7))
              ("m" (* max-val 30))
              ("y" (* max-val 365))
              (_ 1))))
         ;; Match simple format: .+1d
         ((and sched (string-match "\\([.+]+\\)\\([0-9]+\\)\\([dwmy]\\)" sched))
          (let ((val (string-to-number (match-string 2 sched)))
                (unit (match-string 3 sched)))
            (pcase unit
              ("d" val)            ;; Days
              ("w" (* val 7))      ;; Weeks
              ("m" (* val 30))     ;; Months (approx)
              ("y" (* val 365))    ;; Years
              (_ 1))))
         ;; No repeater found
         (t 1)))
    (error 1))) ;; Return 1 day on any error

(defun dashboard--get-next-due-date ()
  "Get the scheduled date for the habit at point.
Returns a time value or nil if not scheduled."
  (let ((sched (org-entry-get nil "SCHEDULED")))
    (when sched
      (org-time-string-to-time sched))))

(defun dashboard--get-habit-history ()
  "Check DONE status for the last N days (defined by `dashboard-habit-history-days`)."
  (let* ((history '())
         (now (current-time))
         ;; Calculate cutoff time (N+1 days ago to be safe)
         (cutoff-time (time-subtract now (* (1+ dashboard-habit-history-days) dashboard--seconds-per-day)))
         (completion-dates (dashboard--get-habit-completion-dates cutoff-time)))
    (dotimes (i dashboard-habit-history-days)
      (let* ((day-offset (- (1- dashboard-habit-history-days) i)) ;; N-1 days ago to today
             (target-date (time-subtract now (* day-offset dashboard--seconds-per-day)))
             (target-date-str (format-time-string "%Y-%m-%d" 
                                                  (dashboard--normalize-timestamp-for-org-day target-date)))
             (done (member target-date-str completion-dates)))
        (push (if done t nil) history)))
    history))

(defun dashboard--get-habits ()
  "Retrieve habits and their status with caching.
Returns list of (TITLE INTERVAL HISTORY NEXT-DUE) where INTERVAL is days between repetitions.
Uses cache if habits file hasn't been modified since last read."
  (if (dashboard--habit-cache-valid-p)
      ;; Return cached data
      (cdr dashboard--habit-cache)
    ;; Cache invalid or missing - rebuild
    (dashboard--log "Fetching habits from %s..." dashboard-gtd-file)
    (let ((habits-file (dashboard--get-gtd-file))
          (results '()))
      (when (file-exists-p habits-file)
        (with-temp-buffer
          (insert-file-contents habits-file)
          (org-mode)
          ;; First collect all habit positions
          (let ((habit-positions '()))
            (org-map-entries
             (lambda ()
               (push (point) habit-positions))
             "STYLE=\"habit\"")
            ;; Then process each habit
            (dolist (pos (nreverse habit-positions))
              (goto-char pos)
              (let* ((title (org-get-heading t t t t))
                     (interval (dashboard--get-habit-interval-days))
                     (history (dashboard--get-habit-history))
                     (next-due (dashboard--get-next-due-date))
                     (habit (list title interval history next-due)))
                (if (dashboard--validate-habit habit)
                    (progn
                      (dashboard--log "Valid habit: %s (interval: %d days, history length: %d)"
                                     title interval (length history))
                      (push habit results))
                  (dashboard--log "WARNING: Invalid habit data for: %s" title)))))) ; close push, let*, dolist, inner let, with-temp-buffer
        ;; Cache the results (use reverse to avoid destroying the list)
        (setq results (nreverse results))
        (setq dashboard--habit-cache-file habits-file
              dashboard--habit-cache (cons (file-attribute-modification-time
                                           (file-attributes habits-file))
                                          results))) ; close when
      results))) ; close let, close if, close defun


(defun dashboard--get-daily-habit-counts ()
  "Get habit completion counts for the last N days.
Returns a list of N integers (defined by `dashboard-habit-history-days`).
Uses vectors internally for better performance (O(1) access vs O(n))."
  (dashboard--log "Calculating daily habit counts for %d days" dashboard-habit-history-days)
  (let ((habits (dashboard--get-habits))
        (daily-counts (make-vector dashboard-habit-history-days 0)))
    (dolist (habit habits)
      (let ((history (nth 2 habit)))  ; Third element is history
        (dotimes (day dashboard-habit-history-days)
          (when (nth day history)
            (aset daily-counts day (1+ (aref daily-counts day)))))))
    (dashboard--log "Daily counts calculated: total habits=%d" (length habits))
    ;; Convert vector to list for compatibility with existing code
    (append daily-counts nil)))

(defun dashboard--calculate-habit-streak (interval history)
  "Calculate current streak from HISTORY respecting INTERVAL grace period.
INTERVAL is the number of days allowed between completions.
HISTORY is a list of booleans (most recent first, index 0 = today).
Returns number of streak days (actual completions, not calendar days)."
  (let ((streak 0)
        (gap-between-completions 0)
        (days-from-today-to-most-recent nil))
    (catch 'break
      ;; Iterate through history from today backwards
      (dotimes (i dashboard-habit-history-days)
        (if (nth i history)
            ;; Found a completion
            (progn
              ;; Record how far back the most recent completion is
              (when (null days-from-today-to-most-recent)
                (setq days-from-today-to-most-recent i))
              ;; Check if gap from previous completion is too large
              (when (and (> streak 0) (> gap-between-completions interval))
                ;; Gap too large, streak is broken
                (throw 'break streak))
              ;; Valid completion, increment streak and reset gap counter
              (setq streak (1+ streak))
              (setq gap-between-completions 0))
          ;; No completion this day, increment gap
          (setq gap-between-completions (1+ gap-between-completions)))))
    ;; Check if the most recent completion is too old (beyond grace period from today)
    (when (and days-from-today-to-most-recent
               (> days-from-today-to-most-recent interval))
      (setq streak 0))
    streak))

(defun dashboard--get-habit-streaks ()
  "Get current streak for each habit respecting their individual intervals.
Returns list of (TITLE . STREAK-DAYS)."
  (let ((habits (dashboard--get-habits)))
    (mapcar (lambda (habit)
              (let ((title (nth 0 habit))
                    (interval (nth 1 habit))
                    (history (nth 2 habit)))
                (cons title
                      (dashboard--calculate-habit-streak interval history))))
            habits)))

(defun dashboard--calculate-expected-completions (interval days)
  "Calculate expected number of completions for a habit.
INTERVAL is days between repetitions, DAYS is the tracking window.
Returns a float representing expected completions."
  (max 1.0 (/ (float days) interval)))

(defun dashboard--get-habit-age-days (history)
  "Get the age of a habit in days based on its first completion.
HISTORY is a list of booleans (most recent first, index 0 = today).
Returns number of days since first completion, or nil if no completions."
  (let ((first-completion-idx nil))
    (dotimes (i (length history))
      (when (nth i history)
        (setq first-completion-idx i)))
    first-completion-idx))

(defun dashboard--calculate-consistency-percentage (actual-count interval days history)
  "Calculate consistency percentage for a habit.
ACTUAL-COUNT is number of completions, INTERVAL is days between repetitions,
DAYS is the tracking window, HISTORY is the completion history.
Returns percentage (0-100+). Adjusts for new habits to avoid unfair penalties."
  (let* ((habit-age (dashboard--get-habit-age-days history))
         (effective-window (if habit-age
                              (min days (1+ habit-age))  ;; +1 because age is 0-indexed
                            days))
         (expected (dashboard--calculate-expected-completions interval effective-window)))
    (* (/ (float actual-count) expected) 100.0)))

(defun dashboard--get-habit-statistics ()
  "Get detailed statistics for each habit.
Returns list of (TITLE INTERVAL ACTUAL EXPECTED CONSISTENCY% NEXT-DUE HISTORY)."
  (let ((habits (dashboard--get-habits)))
    (mapcar (lambda (habit)
              (let* ((title (nth 0 habit))
                     (interval (nth 1 habit))
                     (history (nth 2 habit))
                     (next-due (nth 3 habit))
                     (actual (cl-count-if #'identity history))
                     (habit-age (dashboard--get-habit-age-days history))
                     (effective-window (if habit-age
                                          (min dashboard-habit-history-days (1+ habit-age))
                                        dashboard-habit-history-days))
                     (expected (dashboard--calculate-expected-completions 
                               interval effective-window))
                     (consistency (dashboard--calculate-consistency-percentage
                                  actual interval dashboard-habit-history-days history)))
                (list title interval actual expected consistency next-due history)))
            habits)))

(defun dashboard--svg-draw-month-labels (svg cell-size gap rows cols days)
  "Draw month labels on the SVG graph."
  (let* ((now (current-time))
         (today-dow (1- (string-to-number (format-time-string "%u" now))))
         (month-boundaries '())
         (seen-months '())
         (label-y (+ (* rows (+ cell-size gap)) gap 10)))
    ;; Find the 1st day of each month going backwards and record column positions
    ;; Use year+month for deduplication to handle year boundaries correctly
    (dotimes (days-ago days)
      (let* ((date (time-subtract now (* days-ago dashboard--seconds-per-day)))
             (day-of-month (string-to-number (format-time-string "%d" date))))
        ;; When we hit the 1st of a month, record its position
        (when (= day-of-month 1)
          (let* ((days-from-current-monday (- days-ago today-dow))
                 (weeks-back (if (<= days-from-current-monday 0)
                                0
                              (/ (+ days-from-current-monday 6) 7)))
                 (col (- (1- cols) weeks-back))
                 (month-name (format-time-string "%b" date))
                 (year-month (format-time-string "%Y-%m" date)))
            (when (and (>= col 0) (not (member year-month seen-months)))
              (push year-month seen-months)
              (push (cons col month-name) month-boundaries))))))
    ;; Calculate centered positions between boundaries and draw labels
    (let ((sorted-boundaries (sort month-boundaries (lambda (a b) (< (car a) (car b))))))
      (dotimes (i (1- (length sorted-boundaries)))
        (let* ((start-col (car (nth i sorted-boundaries)))
               (end-col (car (nth (1+ i) sorted-boundaries)))
               (mid-col (/ (+ start-col end-col) 2))
               (label (cdr (nth i sorted-boundaries))))
          (svg-text svg label
                   :x (+ gap (* mid-col (+ cell-size gap)))
                   :y label-y
                   :fill (face-attribute 'default :foreground nil t)
                   :font-size "9"
                   :font-family "monospace")))
      ;; Handle the leftmost (oldest) month segment before the first boundary
      (when sorted-boundaries
        (let* ((first-boundary (car sorted-boundaries))
               (first-col (car first-boundary)))
          (when (> first-col 0)
            (let* ((start-date (time-subtract now (* days dashboard--seconds-per-day)))
                   (label (format-time-string "%b" start-date))
                   ;; Center between start of grid (0) and first boundary
                   (mid-col (/ first-col 2)))
              (svg-text svg label
                       :x (+ gap (* mid-col (+ cell-size gap)))
                       :y label-y
                       :fill (face-attribute 'default :foreground nil t)
                       :font-size "9"
                       :font-family "monospace"))))))))

(defun dashboard--render-svg-graph (daily-counts)
  "Render SVG activity graph for DAILY-COUNTS with month labels.
Returns an SVG image object."
  (let* ((cell-size dashboard-habit-cell-size)
         (gap dashboard-habit-cell-gap)
         (rows dashboard-habit-grid-rows)
         (cols dashboard-habit-grid-cols)
         (label-height 12)  ;; Space for month labels
         (width (+ (* cols (+ cell-size gap)) gap))
         (height (+ (* rows (+ cell-size gap)) gap label-height))
         (svg (svg-create width height))
         (now (current-time))
         (days dashboard-habit-history-days)
         ;; Calculate dynamic thresholds based on MAX activity in the period
         ;; This ensures the graph scales to your actual performance, not just the number of habits
         (max-count (if daily-counts (apply #'max daily-counts) 0))
         (threshold-1 (/ (float max-count) 3.0))
         (threshold-2 (* 2 threshold-1)))
    ;; Draw background
    (svg-rectangle svg 0 0 width height
                  :fill (dashboard--habit-grid-background) :stroke-width 0)
    ;; Calculate grid: rows are weekdays (0=Mon, 6=Sun), cols are weeks
    ;; Work backwards from today, placing each day in its correct week column
    (let ((today-dow (1- (string-to-number (format-time-string "%u" now)))) ;; Today's day of week (0=Mon)
          (stroke-color (face-attribute 'dashboard-habit-grid-stroke :foreground nil t))
          (color-today (face-attribute 'dashboard-habit-today :foreground nil t))
          (color-none (face-attribute 'dashboard-habit-none :foreground nil t))
          (color-lvl-1 (face-attribute 'dashboard-habit-level-1 :foreground nil t))
          (color-lvl-2 (face-attribute 'dashboard-habit-level-2 :foreground nil t))
          (color-lvl-3 (face-attribute 'dashboard-habit-level-3 :foreground nil t))
          (color-month-start (face-attribute 'font-lock-comment-face :foreground nil t)))
      (dotimes (days-ago days)
        (let* ((date (time-subtract now (* days-ago dashboard--seconds-per-day)))
               (dow (1- (string-to-number (format-time-string "%u" date)))) ;; 0=Mon, 6=Sun
               (row dow)
               (day-of-month (string-to-number (format-time-string "%d" date)))
               ;; Calculate column: how many Mondays ago is this date?
               ;; Monday of current week is today-dow days ago
               ;; If date is >= that Monday, it's in the current week (col 12)
               ;; Otherwise calculate which previous week
               (days-from-current-monday (- days-ago today-dow))
               (weeks-back (if (<= days-from-current-monday 0)
                              0  ;; Current week
                            (/ (+ days-from-current-monday 6) 7)))  ;; Round up to get week number
               (col (- (1- cols) weeks-back))
               ;; Get count: daily-counts[days-ago] = that day
               (count (nth days-ago daily-counts))
               ;; Determine if this is today or a future date
               (is-today (= days-ago 0))
               (is-future (< days-ago 0))
               (is-month-start (= day-of-month 1)))
          ;; Only draw cells for past dates and today, skip future dates
          (when (and (not is-future)
                    (>= col 0) (< col cols) 
                    (>= row 0) (< row rows))
            (let* ((x (+ gap (* col (+ cell-size gap))))
                   (y (+ gap (* row (+ cell-size gap))))
                   (color (if is-today
                             color-today
                           (cond
                            ((= count 0) color-none)
                            ((<= count threshold-1) color-lvl-1)
                            ((<= count threshold-2) color-lvl-2)
                            (t color-lvl-3))))
                   (border-color (if is-month-start
                                    color-month-start
                                  stroke-color))
                   (border-width (if is-month-start 1.5 0.5)))
              (svg-rectangle svg x y cell-size cell-size
                            :fill color
                            :rx 1 :ry 1
                            :stroke border-color
                            :stroke-width border-width))))))
    
    ;; Add month labels at bottom
    (dashboard--svg-draw-month-labels svg cell-size gap rows cols days)
    
    (svg-image svg :ascent 'center)))

(defun dashboard--render-text-graph (daily-counts)
  "Render text-based activity graph for DAILY-COUNTS with month markers.
Returns a string representation."
  (let* ((now (current-time))
         (graph-line (mapconcat (lambda (count)
                                 (cond ((= count 0) "·")
                                       ((= count 1) "▫")
                                       (t "▪")))
                               daily-counts " "))
         (month-line (make-string (length graph-line) ?\s)))
    ;; Add month markers every ~30 days (approximate month boundaries)
    (dotimes (i 12)
      (let* ((days-ago (- 364 (* i 30)))  ; Approximate month start
             (pos (* days-ago 2)))  ; Each day takes 2 chars (symbol + space)
        (when (< pos (length month-line))
          (let* ((date (time-subtract now (* days-ago dashboard--seconds-per-day)))
                 (month (format-time-string "%b" date)))
            (dotimes (j (min 3 (- (length month-line) pos)))
              (when (< (+ pos j) (length month-line))
                (aset month-line (+ pos j) (aref month j))))))))
    (concat graph-line "\n" month-line)))

(defun dashboard--make-github-activity-graph (daily-counts)
  "Create GitHub-style activity graph for DAILY-COUNTS.
Dispatches to SVG or text renderer based on svg-lib availability."
  (if (featurep 'svg-lib)
      (dashboard--render-svg-graph daily-counts)
    (dashboard--render-text-graph daily-counts)))

(defun dashboard--insert-habits ()
  "Insert habit consistency graph into dashboard."
  (let ((habits (dashboard--get-habits)))
    (when habits
      (insert "\n🔄 HABIT ACTIVITY (Last Year)\n")
      (insert (make-string 80 ?─) "\n")
      (let* ((daily-counts (dashboard--get-daily-habit-counts))
             (stats (dashboard--get-habit-statistics))
             (streaks (dashboard--get-habit-streaks))
             (avg-consistency (if stats
                                 (/ (apply #'+ (mapcar (lambda (s) (nth 4 s)) stats))
                                    (float (length stats)))
                               0.0)))
        
        ;; Insert graph
        (insert "  ")
        (if (featurep 'svg-lib)
            (insert-image (dashboard--make-github-activity-graph daily-counts))
          (insert (dashboard--make-github-activity-graph daily-counts)))
        (insert (format "\n  Average Consistency: %.0f%%\n" avg-consistency))
        
        ;; Show per-habit consistency scores
        (insert "\n  📊 Habit Performance:\n")
        (let* ((now (current-time))
               (today-normalized (dashboard--normalize-timestamp-for-org-day now))
               (today-str (format-time-string "%Y-%m-%d" today-normalized)))
          (dolist (stat stats)
            (let* ((title (nth 0 stat))
                   (actual (nth 2 stat))
                   (expected (nth 3 stat))
                   (consistency (nth 4 stat))
                   (next-due (nth 5 stat))
                   (next-due-str (when next-due (format-time-string "%Y-%m-%d" next-due)))
                   (history (nth 6 stat))
                   (streak (cdr (assoc title streaks)))
                   (done-today (nth 0 history))
                   ;; Determine status color
                   (status-face
                    (cond
                     (done-today 'success)  ;; Green: Done today
                     ((not next-due) 'default) ;; No schedule
                     ((string< next-due-str today-str) 'error) ;; Red: Overdue
                     ((string= next-due-str today-str) 'warning) ;; Yellow: Due today
                     (t 'font-lock-constant-face)))) ;; Blue/Cyan: Future
              
              (insert "    • ")
              (insert (propertize (format "%s: %.0f%% (%d/%d)%s\n"
                            title
                            consistency
                            actual
                            (round expected)
                            (if (and streak (> streak 0))
                                (format " | 🔥 %d day%s" streak (if (= streak 1) "" "s"))
                              ""))
                          'face status-face
                          'font-lock-face status-face)))))
        (insert "\n")))))

;; ============================================================================
;; MAIN DASHBOARD FUNCTIONS
;; ============================================================================

(defun dashboard-render ()
  "Render the complete dashboard content in the current buffer."
  (let ((inhibit-read-only t)
        (inbox-count (dashboard--count-inbox-items))
        (events (dashboard--get-upcoming-events)))
    (erase-buffer)
    (insert (format "📚 Personal Workspace — %s\n" 
                    (format-time-string "%Y-%m-%d %H:%M")))
    (insert "═════════════════════\n")
    
    ;; Compact status bar with key metrics
    (insert (propertize 
             (format "[ 📥 %d ] [ 📅 %s ] [ %s ] • Press 'g' to refresh • 'q' to close\n"
                     inbox-count
                     (if events
                         (format "%d upcoming" (length events))
                       "None")
                     (dashboard--format-git-status))
             'face '(:foreground "dim gray")))
    (insert "\n")
    
    ;; Only show detailed inbox if >10 items or has events
    (when (or (> inbox-count 10) events)
      (dashboard--insert-inbox-status))

    ;; Insert habits
    (dashboard--insert-habits)
    
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
Updates the content with current reading progress, time tracking, and key bindings.
Invalidates habit and inbox caches to ensure fresh data."
  (interactive)
  (when (derived-mode-p 'dashboard-mode)
    (dashboard--log "=== Dashboard refresh triggered ===")
    (dashboard--invalidate-habit-cache)
    (dashboard--invalidate-inbox-cache)
    ;; Also invalidate git status cache on manual refresh
    (setq dashboard--git-status-cache nil)
    (dashboard--log "All caches invalidated")
    (run-hooks 'dashboard-refresh-hook)
    (dashboard-render)
    (dashboard--log "=== Dashboard refresh complete ===")))

;; ============================================================================
;; PUBLIC ALIASES & STARTUP CONFIGURATION
;; ============================================================================

;; Show dashboard on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (dashboard-show)
            (delete-other-windows)))  ;; Ensure a single window

(provide 'dashboard)
