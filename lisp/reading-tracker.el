;;; reading-tracker.el --- Personal reading progress tracker -*- lexical-binding: t; -*-

;; Version: 1.0
;; Author: arttusii  
;; Description: Track reading progress with goals and statistics
;;
;; Key Functions:
;;   my/org-reading-add-book      - Add new book to tracker
;;   my/org-reading-set-current-page  - Log reading progress
;;   my/org-reading-view-stats    - View reading statistics
;;
;; Data Storage: Uses org-persist for reliable data management

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================el --- Personal reading progress tracker -*- lexical-binding: t; -*-

;; Version: 1.0
;; Author: arttusii  
;; Description: Track reading progress with goals and statistics
;;
;; Key Functions:
;;   my/org-reading-add-book      - Add new book to tracker
;;   my/org-reading-set-current-page  - Log reading progress
;;   my/org-reading-view-stats    - View reading statistics
;;
;; Data Storage: Uses org-persist for reliable data management

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)         ;; Core Org APIs used throughout
(require 'org-table)   ;; Table alignment and formatting
(require 'org-element) ;; Needed for reliable planning parsing
(require 'subr-x)      ;; string-empty-p and other utilities

;; ============================================================================
;; CONFIGURATION
;; ============================================================================

;; Path to reading tracker file (must be defined before first use)
(defvar my/org-reading-file (expand-file-name "areas/reading.org" org-directory)
  "Path to the reading tracker file in the Areas directory.")

;; ============================================================================
;; FILE BOOTSTRAPPING
;; ============================================================================

(defun my/org--ensure-reading-file ()
  "Create ~/org/areas/reading.org with basic structure if missing.
Ensures the file has the basic structure needed for reading tracking."
  (unless (file-exists-p my/org-reading-file)
    (with-temp-file my/org-reading-file
      (insert "#+TITLE: Reading\n\n"
              "* Books\n"))))

(defun my/org-reading-bootstrap ()
  "Ensure reading file exists and has Books heading.
Creates missing sections and saves the file."
  (my/org--ensure-reading-file)
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      ;; Ensure Books section exists
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Books\\b" nil t)
        (goto-char (point-max))
        (insert "\n* Books\n"))
      (save-buffer))))

;; Bootstrap on load
;; PERFORMANCE: Bootstrap is now lazy - only runs when reading functions are first called
;; (my/org-reading-bootstrap)

;; ============================================================================
;; UTILITY FUNCTIONS
;; ============================================================================

(defun my/org--ensure-ts (s)
  "Ensure S is an active Org timestamp like <YYYY-MM-DD>.
Converts plain dates to Org timestamp format if needed."
  (when (and s (not (string-empty-p s)))
    (if (string-match-p "^<.*>$" s) s (format "<%s>" s))))

(defun my/org--ts-to-time (s)
  "Parse the first active timestamp in S to an Emacs time value.
Uses Org's timestamp regex to find and parse timestamps."
  (when (and s (string-match org-ts-regexp1 s))
    (org-time-string-to-time (match-string 0 s))))

;; Find planning time at the current heading (DEADLINE, SCHEDULED, or textual fallback)
(defun my/org--planning-time-at-point ()
  "Extract planning time from current heading using multiple methods.
Returns Emacs time value or nil if no planning found."
  (save-excursion
    (org-back-to-heading t)
    (or
     ;; 1) Native Org planning API (most reliable)
     (org-get-deadline-time (point))
     (org-get-scheduled-time (point))
     ;; 2) org-element raw values
     (let* ((el (org-element-at-point))
            (dl (org-element-property :deadline el))
            (sc (org-element-property :scheduled el))
            (raw (or (and dl (org-element-property :raw-value dl))
                     (and sc (org-element-property :raw-value sc)))))
       (and raw (my/org--ts-to-time raw)))
     ;; 3) Fallback: scan subtree text for "DEADLINE: <...>"
     (let ((beg (progn (org-back-to-heading t) (point)))
           (end (progn (org-end-of-subtree t t) (point))))
       (goto-char beg)
       (when (re-search-forward "^DEADLINE: *\\(<[^>]+>\\)" end t)
         (org-time-string-to-time (match-string 1)))))))

;; ============================================================================
;; CORE READING TRACKER FUNCTIONS
;; ============================================================================

(defun my/org--days-left-at-point ()
  "Calculate inclusive days left to the planning date; never 0 (today => 1).
Uses planning time from current heading to compute remaining days."
  (let ((ts (my/org--planning-time-at-point)))
    (when ts
      (max 1 (1+ (time-to-number-of-days  ;; Use 1+ for clearer intent
                  (time-subtract ts (current-time))))))))

(defun my/org--average-pages-per-day-at-point ()
  "Calculate average pages per day from START_DATE to now.
Returns nil if no valid dates or if just started."
  (let* ((start-date-str (org-entry-get (point) "START_DATE"))
         (pages-read (string-to-number (or (org-entry-get (point) "CURRENT_PAGE") "0"))))
    (when (and start-date-str (> pages-read 0))
      (condition-case nil
          (let* ((start-time (org-time-string-to-time start-date-str))
                 (days-elapsed (max 1 (time-to-number-of-days 
                                      (time-subtract (current-time) start-time))))
                 (avg (/ pages-read (float days-elapsed))))
            (if (> avg 0.1) avg nil))  ; Return nil for very small averages
        (error nil)))))

;; ============================================================================
;; BOOK MANAGEMENT (CRUD OPERATIONS)
;; ============================================================================

(defun my/org--collect-reading-headings ()
  "Return an alist of (TITLE . MARKER) for headings with :TOTAL_PAGES: property.
Collects all book entries that have page count information and are not completed."
  (my/org-reading-bootstrap)  ;; Lazy bootstrap
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (let (items)
        (org-map-entries
         (lambda ()
           (let* ((title (nth 4 (org-heading-components)))
                  (total (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0")))
                  (completed (org-entry-get (point) "COMPLETED")))
             (when (and (> total 0) (not completed))  ; Exclude completed books
               (push (cons title (point-marker)) items))))
         nil 'file)
        (nreverse items)))))

(defun my/org-reading-add-book ()
  "Add a new book under * Books with properties and optional deadline.
Prompts for title, author, pages, and optional deadline. Creates a properly
formatted book entry with Org properties.

TIP: Create detailed book notes separately using org-roam (C-c n f).
     You can link them by adding :ROAM_NOTES: [[id:xxx][Title]] property."
  (interactive)
  (my/org-reading-bootstrap)  ;; Lazy bootstrap
  (let* ((title   (read-string "Title: "))
         (author  (read-string "Author: "))
         (pages   (read-number "Total pages: " 0))
         (deadline (my/org--ensure-ts
                    (org-read-date nil nil nil "Finish by (deadline), e.g. +2w or 2025-09-01: "))))
    (with-current-buffer (find-file-noselect my/org-reading-file)
      (org-with-wide-buffer
        ;; Ensure * Books exists
        (goto-char (point-min))
        (unless (re-search-forward "^\\*+ Books\\b" nil t)
          (goto-char (point-max))
          (insert "\n* Books\n")
          (goto-char (point-max)))
        ;; Compute child level (one more star than * Books)
        (save-excursion
          (re-search-backward "^\\(\\*+\\) Books\\b")
          (setq-local my/books-child-stars (concat (match-string 1) "*")))
        ;; Append at the end of the Books subtree
        (org-back-to-heading t)            ;; Be on * Books
        (org-end-of-subtree t t)           ;; Go to end of Books subtree
        (unless (bolp) (insert "\n"))      ;; Ensure new line before inserting
        ;; Insert heading + drawer
        (insert (format "%s %s\n:PROPERTIES:\n:AUTHOR: %s\n:TOTAL_PAGES: %d\n:CURRENT_PAGE: 0\n:START_DATE: [%s]\n:END:\n"
                        my/books-child-stars title author pages (format-time-string "%Y-%m-%d %a")))
        (when deadline
          (insert (format "DEADLINE: %s\n" deadline)))
        (save-buffer)))
    (message "Added book: %s. Use C-c n f to create detailed notes in org-roam." title)))

(defun my/org-reading-set-current-page ()
  "Set CURRENT_PAGE for a chosen book.
Updates progress and sets LAST_UPDATED timestamp."
  (interactive)
  (let* ((items (my/org--collect-reading-headings))
         (title (completing-read "Book: " (mapcar #'car items) nil t))
         (m     (cdr (assoc title items)))
         (page  (read-number "Current page: ")))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
        (goto-char m)
        (org-entry-put (point) "CURRENT_PAGE" (number-to-string page))
        (org-entry-put (point) "LAST_UPDATED" (format-time-string "%Y-%m-%d %a %H:%M"))
        (save-buffer)))
    (message "Updated %s to page %d" title page)))

(defun my/org-reading-delete-book ()
  "Delete a book subtree by title.
Prompts for confirmation before deletion."
  (interactive)
  (let* ((items (my/org--collect-reading-headings))
         (title (completing-read "Delete book: " (mapcar #'car items) nil t))
         (m     (cdr (assoc title items))))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
        (goto-char m)
        (org-back-to-heading t)
        (when (y-or-n-p (format "Really delete '%s'? " title))
          (let ((beg (point))
                (end (progn (org-end-of-subtree t t) (point))))
            (delete-region beg end)
            (save-buffer)
            (message "Deleted: %s" title)))))))

(defun my/org-reading-complete-book ()
  "Mark a book as complete (read).
Sets the CURRENT_PAGE to TOTAL_PAGES, adds COMPLETED property with timestamp,
and excludes it from future dashboard display."
  (interactive)
  (let* ((items (my/org--collect-reading-headings))
         (title (completing-read "Mark as complete: " (mapcar #'car items) nil t))
         (m     (cdr (assoc title items))))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
        (goto-char m)
        (let* ((total (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0"))))
          (org-entry-put (point) "CURRENT_PAGE" (number-to-string total))
          (org-entry-put (point) "COMPLETED" (format-time-string "%Y-%m-%d %a %H:%M"))
          (org-entry-put (point) "LAST_UPDATED" (format-time-string "%Y-%m-%d %a %H:%M"))
          (save-buffer)
          (message "Marked '%s' as complete!" title))))))

(defun my/org-reading-open ()
  "Open the reading tracker file in current window."
  (interactive)
  (my/org-reading-bootstrap)  ;; Lazy bootstrap
  (find-file my/org-reading-file))

;; ============================================================================
;; DEADLINE MANAGEMENT
;; ============================================================================

(defun my/org-reading-set-deadline ()
  "Set or clear DEADLINE for a chosen book by title.
Empty date clears the deadline. Uses Org's date reading interface."
  (interactive)
  (let* ((items (my/org--collect-reading-headings))
         (title (completing-read "Book: " (mapcar #'car items) nil t))
         (m     (cdr (assoc title items)))
         (date  (org-read-date nil nil nil
                 "New deadline (e.g., +2w or 2025-09-01). Leave empty to clear: ")))
    (with-current-buffer (marker-buffer m)
      (org-with-wide-buffer
        (goto-char m)
        (org-back-to-heading t)
        (if (string-empty-p date)
            (org-deadline '(4))                        ;; Clear deadline
          (org-deadline nil (my/org--ensure-ts date))) ;; Set deadline
        (save-buffer)))
    (message (if (string-empty-p date)
                 (format "Cleared deadline for: %s" title)
               (format "Deadline for %s set to %s" title date)))))

;; ============================================================================
;; ORG-ROAM INTEGRATION
;; ============================================================================

(defun my/org-reading-open-books ()
  "Open a book note from org-roam books directory.
Filters to show only nodes with :book: tag or in books/ directory."
  (interactive)
  (require 'org-roam)
  (let* ((node (org-roam-node-read
                nil
                (lambda (node)
                  (or
                   ;; Check if file is in books/ subdirectory
                   (string-match-p "/books/" (org-roam-node-file node))
                   ;; Or check if node has :book: tag
                   (member "book" (org-roam-node-tags node))))))
         (file (org-roam-node-file node)))
    (if file
        (find-file file)
      (message "No book selected"))))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

(provide 'reading-tracker)
;;; reading-tracker.el ends here
