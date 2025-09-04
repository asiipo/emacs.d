;;; reading-dashboard.el --- Reading progress dashboard component -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'reading-tracker)  ;; Provides reading data collection functions
(require 'org)               ;; org-entry-get, etc.

;; ============================================================================
;; READING PROGRESS DATA COLLECTION
;; ============================================================================

(defun reading-dashboard--collect-reading-rows ()
  "Return list of (TITLE CURRENT LEFT PCT-STR PPD-STR AVG-STR) using reading-tracker helpers.
Collects reading progress data for display in the reading dashboard."
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
                       pages-read
                       left
                       (format "%.1f" pct)
                       (if ppd (number-to-string ppd) "—")
                       (if avg-ppd (format "%.1f" avg-ppd) "—"))
                      rows)))))))
    (nreverse rows)))

;; ============================================================================
;; READING DASHBOARD RENDERING
;; ============================================================================

(defun reading-dashboard-insert ()
  "Insert reading progress table into the current buffer.
Creates a table showing current reading progress with titles, pages, and progress metrics."
  (let ((rows (reading-dashboard--collect-reading-rows)))
    (insert "\n📖 READING PROGRESS\n")
    (insert "═══════════════════\n\n")
    (if (null rows)
        (progn
          (insert "No books in progress yet\n")
          (insert "Use C-c r a to add your first book\n\n"))
      (progn
        (insert (format "%-30s %8s %6s %8s %10s %8s\n" "Title" "Current" "Left" "Progress" "Pages/day" "Avg/day"))
        (insert (make-string 30 ?-) " " (make-string 8 ?-) " " (make-string 6 ?-) " " (make-string 8 ?-) " " (make-string 10 ?-) " " (make-string 8 ?-) "\n")
        (dolist (r rows)
          (insert (format "%-30s %8d %6d %7s%% %10s %8s\n"
                          (truncate-string-to-width (nth 0 r) 30 nil nil "…")
                          (nth 1 r)
                          (nth 2 r) 
                          (nth 3 r) 
                          (nth 4 r)
                          (nth 5 r))))))))

(provide 'reading-dashboard)
