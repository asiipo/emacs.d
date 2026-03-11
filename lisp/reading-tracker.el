;;; reading-tracker.el --- Minimal reading progress tracker -*- lexical-binding: t; -*-
;;; Commentary:
;; Reading tracker: add books, track progress, mark complete, view stats.

;;; Code:

(require 'org)
(require 'subr-x)

;; Configuration
(defvar my/reading-file (expand-file-name "areas/reading.org" org-directory)
  "Path to the reading tracker file.")

;; Buffer management
(defun my/reading--get-buffer ()
  "Get or create the reading tracker buffer efficiently."
  (my/reading-ensure-file)
  (or (find-buffer-visiting my/reading-file)
      (find-file-noselect my/reading-file)))

;; File initialization
(defun my/reading-ensure-file ()
  "Ensure reading file exists with basic structure."
  (unless (file-exists-p my/reading-file)
    (let ((dir (file-name-directory my/reading-file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file my/reading-file
      (insert "#+TITLE: Reading Tracker\n\n* Books\n"))))

;; Core functions
(defun my/reading-add-book ()
  "Add a new book to track."
  (interactive)
  (let* ((title (read-string "Book title: "))
         (author (read-string "Author: "))
         (pages (read-number "Total pages: " 0))
         (deadline (read-string "Target completion date (YYYY-MM-DD, optional): ")))
    (with-current-buffer (my/reading--get-buffer)
      (org-with-wide-buffer
        (goto-char (point-min))
        (re-search-forward "^\\* Books\\b")
        (org-end-of-subtree t t)
        (insert (format "\n** %s\n%s:PROPERTIES:\n:AUTHOR: %s\n:TOTAL_PAGES: %d\n:CURRENT_PAGE: 0\n:START_DATE: %s\n:END:\n"
                        title
                        (if (and deadline (not (string-empty-p deadline)))
                            (format "DEADLINE: <%s>\n" deadline)
                          "")
                        author pages (format-time-string "[%Y-%m-%d %a]")))
        (save-buffer)))
    (message "Added: %s by %s" title author)))

(defun my/reading-get-books ()
  "Get list of active books as (title . marker) pairs."
  (with-current-buffer (my/reading--get-buffer)
    (org-with-wide-buffer
      (goto-char (point-min))
      (let (books)
        (org-map-entries
         (lambda ()
           (when (org-entry-get (point) "TOTAL_PAGES")
             (let ((title (nth 4 (org-heading-components)))
                   (completed (org-entry-get (point) "COMPLETED")))
               (unless completed
                 (push (cons title (point-marker)) books)))))
         nil 'file)
        (nreverse books)))))

(defun my/reading-update-progress ()
  "Update reading progress for a book."
  (interactive)
  (let* ((books (my/reading-get-books))
         (title (completing-read "Book: " (mapcar #'car books) nil t))
         (marker (cdr (assoc title books)))
         (page (read-number "Current page: ")))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
        (goto-char marker)
        (org-entry-put (point) "CURRENT_PAGE" (number-to-string page))
        (org-entry-put (point) "LAST_UPDATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
        (save-buffer)))
    (message "Updated %s: page %d" title page)))

(defun my/reading-complete-book ()
  "Mark a book as completed and remove deadline if present."
  (interactive)
  (let* ((books (my/reading-get-books))
         (title (completing-read "Complete book: " (mapcar #'car books) nil t))
         (marker (cdr (assoc title books))))
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
        (goto-char marker)
        (let ((total (org-entry-get (point) "TOTAL_PAGES")))
          (org-entry-put (point) "CURRENT_PAGE" total)
          (org-entry-put (point) "COMPLETED" (format-time-string "[%Y-%m-%d %a]"))
          ;; Remove DEADLINE since book is complete
          (save-excursion
            (org-back-to-heading t)
            (let ((end (save-excursion (org-end-of-subtree t t) (point))))
              (when (re-search-forward "^DEADLINE: <[^>]+>" end t)
                (delete-region (line-beginning-position) (1+ (line-end-position))))))
          (save-buffer))))
    (message "Completed: %s" title)))

(defun my/reading-open ()
  "Open the reading tracker file."
  (interactive)
  (my/reading-ensure-file)
  (find-file my/reading-file))

;; Dashboard integration
(defun my/reading-get-current-books ()
  "Get current reading progress for dashboard display.
Returns list of (title author current total progress daily-target) for each book."
  (with-current-buffer (my/reading--get-buffer)
    (org-with-wide-buffer
      (goto-char (point-min))
      (let (books)
        (org-map-entries
         (lambda ()
           (when-let ((total-str (org-entry-get (point) "TOTAL_PAGES")))
             (let ((title (nth 4 (org-heading-components)))
                   (author (org-entry-get (point) "AUTHOR"))
                   (total (string-to-number total-str))
                   (current (string-to-number (or (org-entry-get (point) "CURRENT_PAGE") "0")))
                   (completed (org-entry-get (point) "COMPLETED"))
                   (deadline-str (org-entry-get (point) "DEADLINE")))
               (unless completed
                 (let* ((progress (if (> total 0) (/ (* 100.0 current) total) 0))
                        (daily-target (my/reading-calculate-daily-target current total deadline-str)))
                   (push (list title author current total progress daily-target) books))))))
         nil 'file)
        (nreverse books)))))

(defun my/reading-calculate-daily-target (current total deadline-str)
  "Calculate daily reading target based on deadline.
Returns nil if no deadline set, or pages per day needed.
Deadline is interpreted as end of day (23:59:59)."
  (when (and deadline-str (not (string-empty-p deadline-str)))
    (condition-case nil
        (let* ((deadline-date (org-time-string-to-time deadline-str))
               ;; Add one full day (86400 seconds) to count deadline day as a reading day
               (deadline-end-of-day (time-add deadline-date 86400))
               (current-date (current-time))
               (days-left (/ (float-time (time-subtract deadline-end-of-day current-date)) 86400))
               (pages-left (- total current)))
          (when (> days-left 0)
            (/ pages-left days-left)))
      (error nil))))

(defun my/reading-set-deadline ()
  "Set or update deadline for a book."
  (interactive)
  (let* ((books (my/reading-get-books))
         (book-title (completing-read "Set deadline for book: " 
                                     (mapcar #'car books))))
    (when book-title
      (let ((deadline (read-string "Target completion date (YYYY-MM-DD): ")))
        (when (and deadline (not (string-empty-p deadline)))
          (with-current-buffer (my/reading--get-buffer)
            (org-with-wide-buffer
              (goto-char (cdr (assoc book-title books)))
              (org-entry-put (point) "DEADLINE" (format "[%s]" deadline))
              (save-buffer)))
          (message "Deadline set for '%s': %s" book-title deadline))))))

(provide 'reading-tracker)
;;; reading-tracker.el ends here
