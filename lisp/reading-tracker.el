;;; reading-tracker.el --- Reading progress tracking and dashboard -*- lexical-binding: t; -*-

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

;; Path to book notes file
(defvar my/org-books-file (expand-file-name "resources/books.org" org-directory)
  "Path to the book notes file in the Resources directory.")

;; ============================================================================
;; FILE BOOTSTRAPPING
;; ============================================================================

(defun my/org--ensure-reading-file ()
  "Create ~/org/areas/reading.org with dashboard and books sections if missing.
Ensures the file has the basic structure needed for reading tracking."
  (unless (file-exists-p my/org-reading-file)
    (with-temp-file my/org-reading-file
      (insert "#+TITLE: Reading\n\n"
              "* Dashboard\n"
              "#+BEGIN: reading-dashboard\n"
              "#+END:\n\n"
              "* Books\n"))))

(defun my/org--ensure-books-file ()
  "Create ~/org/resources/books.org with basic structure if missing.
Ensures the file exists for book notes and reviews."
  (unless (file-exists-p my/org-books-file)
    (with-temp-file my/org-books-file
      (insert "#+TITLE: Book Notes & Reviews\n"
              "#+CATEGORY: Resource\n\n"
              "This file contains detailed notes and reviews for books.\n\n"))))

(defun my/org-reading-bootstrap ()
  "Ensure reading file exists and has Dashboard and Books headings.
Creates missing sections and saves the file."
  (my/org--ensure-reading-file)
  (my/org--ensure-books-file)
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (goto-char (point-min))
      ;; Ensure Dashboard section exists
      (unless (re-search-forward "^\\* Dashboard\\b" nil t)
        (goto-char (point-min))
        (insert "* Dashboard\n#+BEGIN: reading-dashboard\n#+END:\n\n"))
      ;; Ensure Books section exists
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Books\\b" nil t)
        (goto-char (point-max))
        (insert "\n* Books\n"))
      (save-buffer))))

;; Bootstrap on load
(my/org-reading-bootstrap)

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

(defun my/org--create-book-notes-entry (title author)
  "Create a book entry in books.org with Notes and Review sections.
Creates a structured entry for detailed note-taking and review."
  (with-current-buffer (find-file-noselect my/org-books-file)
    (org-with-wide-buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n:PROPERTIES:\n:AUTHOR: %s\n:CREATED: [%s]\n:END:\n\n"
                      title author (format-time-string "%Y-%m-%d %a")))
      (insert "** Notes\n\n")
      (insert "** Review\n\n")
      (save-buffer))))

(defun my/org--days-left-at-point ()
  "Calculate inclusive days left to the planning date; never 0 (today => 1).
Uses planning time from current heading to compute remaining days."
  (let ((ts (my/org--planning-time-at-point)))
    (when ts
      (max 1 (1+ (time-to-number-of-days  ;; Use 1+ for clearer intent
                  (time-subtract ts (current-time))))))))

;; Debug helper at a book heading
(defun my/org-reading-dump ()
  "Echo detected planning and days-left for the current book heading.
Useful for debugging planning time detection."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((el (org-element-at-point))
           (dl (and (org-element-property :deadline el)
                    (org-element-property :raw-value (org-element-property :deadline el))))
           (sc (and (org-element-property :scheduled el)
                    (org-element-property :raw-value (org-element-property :scheduled el))))
           (days (my/org--days-left-at-point)))
      (message "deadline=%S scheduled=%S => days-left=%S" dl sc days))))

;; ============================================================================
;; BOOK MANAGEMENT (CRUD OPERATIONS)
;; ============================================================================

(defun my/org--collect-reading-headings ()
  "Return an alist of (TITLE . MARKER) for headings with :TOTAL_PAGES: property.
Collects all book entries that have page count information and are not completed."
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
formatted book entry with Org properties."
  (interactive)
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
        ;; Insert heading + drawer in one go so the drawer stays attached to THIS heading
        (insert (format "%s %s\n:PROPERTIES:\n:AUTHOR: %s\n:TOTAL_PAGES: %d\n:CURRENT_PAGE: 0\n:START_DATE: [%s]\n:END:\n"
                        my/books-child-stars title author pages (format-time-string "%Y-%m-%d %a")))
        (when deadline
          (insert (format "DEADLINE: %s\n" deadline)))
        (save-buffer)))
    ;; Also create entry in books.org for notes
    (my/org--create-book-notes-entry title author)
    (message "Added book: %s (tracking in reading.org, notes in books.org)" title)))

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
  (find-file my/org-reading-file))

(defun my/org-reading-open-books ()
  "Open the book notes file in current window."
  (interactive)
  (find-file my/org-books-file))

(defun my/org-reading-open-book-notes ()
  "Open notes for a chosen book in books.org.
Prompts for book selection and jumps to its notes section."
  (interactive)
  (let* ((items (my/org--collect-reading-headings))
         (title (completing-read "Open notes for book: " (mapcar #'car items) nil t)))
    (find-file my/org-books-file)
    (goto-char (point-min))
    (if (re-search-forward (format "^\\* %s\\b" (regexp-quote title)) nil t)
        (progn
          (org-show-subtree)
          (message "Opened notes for: %s" title))
      (message "Notes not found for: %s" title))))

(defun my/org-reading-refresh-dashboard ()
  "Rebuild all dynamic blocks (dashboard) in reading.org.
Updates the reading progress table with current data."
  (interactive)
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (org-update-all-dblocks)
      (save-buffer)))
  (message "Reading dashboard updated."))

;; ============================================================================
;; DASHBOARD DYNAMIC BLOCK
;; ============================================================================

(defun org-dblock-write:reading-dashboard (_params)
  "Build a minimal reading dashboard table from `my/org-reading-file`.
Creates a table showing: Title | Pages Left | Progress % | Pages/day needed.
This is an Org dynamic block that updates automatically."
  (let (rows)
    (with-current-buffer (find-file-noselect my/org-reading-file)
      (org-with-wide-buffer
        (goto-char (point-min))
        (org-map-entries
         (lambda ()
           (let* ((title   (nth 4 (org-heading-components)))
                  (total   (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0")))
                  (current (string-to-number (or (org-entry-get (point) "CURRENT_PAGE") "0")))
                  (left    (max 0 (- total current)))
                  (pct     (if (> total 0) (* 100.0 (/ current (float total))) 0.0))
                  (dleft   (my/org--days-left-at-point))
                  (ppd     (and (> left 0) (numberp dleft) (ceiling (/ (float left) dleft))))
                  (completed (org-entry-get (point) "COMPLETED")))
             (when (and (> total 0) (not completed))  ; Exclude completed books
               (push (list title left (format "%.1f" pct)
                           (if ppd (number-to-string ppd) "—"))
                     rows))))
         nil 'file)))
    ;; Insert the table
    (insert "| Title | Left | % | Pages/day |\n")
    (insert "|-+-----+-----+-----------|\n")
    (dolist (r (nreverse rows))
      (insert (format "| %s | %d | %s | %s |\n"
                      (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))
    (org-table-align)))

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
;; KEYBINDINGS
;; ============================================================================

;; Reading tracker keybindings (C-c r prefix)
(global-set-key (kbd "C-c r o") #'my/org-reading-open)              ;; Open reading.org
(global-set-key (kbd "C-c r b") #'my/org-reading-open-books)        ;; Open books.org
(global-set-key (kbd "C-c r n") #'my/org-reading-open-book-notes)   ;; Open specific book notes
(global-set-key (kbd "C-c r a") #'my/org-reading-add-book)          ;; Add a book
(global-set-key (kbd "C-c r u") #'my/org-reading-set-current-page)  ;; Update current page
(global-set-key (kbd "C-c r c") #'my/org-reading-complete-book)     ;; Complete a book
(global-set-key (kbd "C-c r d") #'my/org-reading-delete-book)       ;; Delete a book
(global-set-key (kbd "C-c r R") #'my/org-reading-refresh-dashboard) ;; Rebuild dashboard
(global-set-key (kbd "C-c r D") #'my/org-reading-set-deadline)      ;; Change deadline

(provide 'reading-tracker)
