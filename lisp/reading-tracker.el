;; ---- Reading dashboard (robust) ----
(require 'org)         ;; core org APIs used throughout
(require 'org-table)   ;; org-table-align
(require 'org-element) ;; needed for reliable planning parsing
(require 'subr-x)      ;; string-empty-p, etc.

;; Uses `org-directory` defined centrally in init.el

;; Path to your reading tracker file (must be defined before first use)
(defvar my/org-reading-file (expand-file-name "reading.org" org-directory)
  "Path to the reading tracker file.")

;; -----------------------------------------------------------------------------
;; Bootstrap reading.org (create file + ensure sections exist)
;; -----------------------------------------------------------------------------

(defun my/org--ensure-reading-file ()
  "Create ~/org/reading.org with a dashboard and books section if missing."
  (unless (file-exists-p my/org-reading-file)
    (with-temp-file my/org-reading-file
      (insert "#+TITLE: Reading\n\n"
              "* Dashboard\n"
              "#+BEGIN: reading-dashboard\n"
              "#+END:\n\n"
              "* Books\n"))))

(defun my/org-reading-bootstrap ()
  "Ensure reading file exists and has Dashboard and Books headings."
  (my/org--ensure-reading-file)
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Dashboard\\b" nil t)
        (goto-char (point-min))
        (insert "* Dashboard\n#+BEGIN: reading-dashboard\n#+END:\n\n"))
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Books\\b" nil t)
        (goto-char (point-max))
        (insert "\n* Books\n"))
      (save-buffer))))

(my/org-reading-bootstrap)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defun my/org--ensure-ts (s)
  "Ensure S is an active Org timestamp like <YYYY-MM-DD>."
  (when (and s (not (string-empty-p s)))
    (if (string-match-p "^<.*>$" s) s (format "<%s>" s))))

(defun my/org--ts-to-time (s)
  "Parse the first active timestamp in S to an Emacs time value."
  (when (and s (string-match org-ts-regexp1 s))
    (org-time-string-to-time (match-string 0 s))))

;; Find planning time at the current heading (DEADLINE, SCHEDULED, or textual fallback).
(defun my/org--planning-time-at-point ()
  (save-excursion
    (org-back-to-heading t)
    (or
     ;; 1) Native Org planning API
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

(defun my/org--days-left-at-point ()
  "Inclusive days left to the planning date; never 0 (today => 1)."
  (let ((ts (my/org--planning-time-at-point)))
    (when ts
      (max 1 (ceiling (time-to-number-of-days
                       (time-subtract ts (current-time))))))))

;; Debug helper at a book heading
(defun my/org-reading-dump ()
  "Echo detected planning and days-left for the current book heading."
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

;; -----------------------------------------------------------------------------
;; CRUD: add / list / update / delete books
;; -----------------------------------------------------------------------------

(defun my/org--collect-reading-headings ()
  "Return an alist of (TITLE . MARKER) for headings with :TOTAL_PAGES:."
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (goto-char (point-min))
      (let (items)
        (org-map-entries
         (lambda ()
           (let* ((title (nth 4 (org-heading-components)))
                  (total (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0"))))
             (when (> total 0)
               (push (cons title (point-marker)) items))))
         nil 'file)
        (nreverse items)))))

(defun my/org-reading-add-book ()
  "Add a book under * Books with a property drawer and optional deadline."
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
        (org-back-to-heading t)            ; be on * Books
        (org-end-of-subtree t t)           ; go to end of Books subtree
        (unless (bolp) (insert "\n"))      ; ensure new line before inserting
        ;; Insert heading + drawer in one go so the drawer stays attached to THIS heading
        (insert (format "%s %s\n:PROPERTIES:\n:AUTHOR: %s\n:TOTAL_PAGES: %d\n:CURRENT_PAGE: 0\n:START_DATE: [%s]\n:END:\n"
                        my/books-child-stars title author pages (format-time-string "%Y-%m-%d %a")))
        (when deadline
          (insert (format "DEADLINE: %s\n" deadline)))
        (save-buffer)))
    (message "Added book: %s" title)))


(defun my/org-reading-set-current-page ()
  "Set CURRENT_PAGE for a chosen book."
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
  "Delete a book subtree by title."
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

(defun my/org-reading-open ()
  "Open the reading tracker file."
  (interactive)
  (find-file my/org-reading-file))

(defun my/org-reading-refresh-dashboard ()
  "Rebuild all dynamic blocks (dashboard) in reading.org."
  (interactive)
  (with-current-buffer (find-file-noselect my/org-reading-file)
    (org-with-wide-buffer
      (org-update-all-dblocks)
      (save-buffer)))
  (message "Reading dashboard updated."))

;; -----------------------------------------------------------------------------
;; Dashboard dynamic block: Title | Left | % | Pages/day
;; -----------------------------------------------------------------------------

(defun org-dblock-write:reading-dashboard (_params)
  "Build a minimal reading dashboard from `my/org-reading-file`."
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
                  (ppd     (and (> left 0) (numberp dleft) (ceiling (/ (float left) dleft)))))
             (when (> total 0)
               (push (list title left (format "%.1f" pct)
                           (if ppd (number-to-string ppd) "—"))
                     rows))))
         nil 'file)))
    (insert "| Title | Left | % | Pages/day |\n")
    (insert "|-+-----+-----+-----------|\n")
    (dolist (r (nreverse rows))
      (insert (format "| %s | %d | %s | %s |\n"
                      (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))
    (org-table-align)))

(defun my/org-reading-set-deadline ()
  "Set or clear DEADLINE for a chosen book by title.
Empty date clears the deadline."
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
            (org-deadline '(4))                        ;; clear deadline
          (org-deadline nil (my/org--ensure-ts date))) ;; set deadline
        (save-buffer)))
    (message (if (string-empty-p date)
                 (format "Cleared deadline for: %s" title)
               (format "Deadline for %s set to %s" title date)))))



;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-c r o") #'my/org-reading-open)              ;; open reading.org
(global-set-key (kbd "C-c r a") #'my/org-reading-add-book)          ;; add a book
(global-set-key (kbd "C-c r u") #'my/org-reading-set-current-page)  ;; update current page
(global-set-key (kbd "C-c r d") #'my/org-reading-delete-book)       ;; delete a book
(global-set-key (kbd "C-c r R") #'my/org-reading-refresh-dashboard) ;; rebuild dashboard
(global-set-key (kbd "C-c r D") #'my/org-reading-set-deadline)      ;; change deadline

(provide 'reading-tracker)
