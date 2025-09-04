;;; time-dashboard.el --- Time tracking dashboard component -*- lexical-binding: t; -*-

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)

;; ============================================================================
;; TIME TRACKING DATA COLLECTION
;; ============================================================================

(defun time-dashboard--collect-clock-entries ()
  "Collect today's clock entries from all Org files.
Returns list of (FILE-NAME HEADING DURATION-MINUTES DURATION-STR) for display."
  (let ((today-date (format-time-string "%Y-%m-%d"))
        (clock-entries '())
        (org-files (append
                    (list (expand-file-name "inbox.org" org-directory))
                    (when (file-directory-p (expand-file-name "projects" org-directory))
                      (directory-files-recursively (expand-file-name "projects" org-directory) "\\.org$"))
                    (when (file-directory-p (expand-file-name "areas" org-directory))
                      (directory-files-recursively (expand-file-name "areas" org-directory) "\\.org$"))
                    (when (file-directory-p (expand-file-name "resources" org-directory))
                      (directory-files-recursively (expand-file-name "resources" org-directory) "\\.org$")))))
    
    (dolist (file org-files)
      (when (and (file-exists-p file) (file-readable-p file))
        (condition-case nil
          ;; PERFORMANCE: Use temp buffer instead of persistent buffer
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\] =>[ \t]+\\([0-9:]+\\)" nil t)
              (let* ((start-time (match-string 1))
                     (end-time (match-string 2))
                     (duration (match-string 3))
                     (entry-date (when (>= (length start-time) 10)
                                   (substring start-time 0 10)))) ; Extract YYYY-MM-DD safely
                (when (and entry-date (string= entry-date today-date))
                  (save-excursion
                    ;; Find the heading for this clock entry
                    (let ((heading-pos (re-search-backward "^\\*+ " nil t)))
                      (when heading-pos
                        (goto-char heading-pos)
                        (let* ((heading (nth 4 (org-heading-components)))
                               (file-name (file-name-nondirectory file))
                               (duration-mins (time-dashboard--parse-duration duration)))
                          (push (list file-name heading duration-mins duration) clock-entries)))))))))
          (error nil)))) ; Skip files with errors
    
    ;; Sort by file name first, then by duration (descending) within each file
    (sort clock-entries (lambda (a b)
                          (let ((file-a (nth 0 a))
                                (file-b (nth 0 b))
                                (dur-a (nth 2 a))
                                (dur-b (nth 2 b)))
                            (if (string= file-a file-b)
                                (> dur-a dur-b)  ; Same file: sort by duration desc
                              (string< file-a file-b)))))))  ; Different files: sort by filename

(defun time-dashboard--parse-duration (duration-str)
  "Parse duration string like '1:30' to minutes (90).
Handles formats like '0:05', '1:30', '10:45', etc."
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" duration-str)
      (let ((hours (string-to-number (match-string 1 duration-str)))
            (minutes (string-to-number (match-string 2 duration-str))))
        (+ (* hours 60) minutes))
    0))

(defun time-dashboard--format-duration (minutes)
  "Format minutes back to HH:MM string for display."
  (format "%d:%02d" (/ minutes 60) (% minutes 60)))

;; ============================================================================
;; TIME TRACKING DASHBOARD RENDERING
;; ============================================================================

(defun time-dashboard-insert ()
  "Insert today's time tracking table into the current buffer.
Shows clock entries from all Org files for today with file, task, and duration."
  (let ((entries (time-dashboard--collect-clock-entries)))
    (insert "\n⏰ TODAY'S TIME LOG\n")
    (insert "══════════════════\n\n")
    (if (null entries)
        (progn
          (insert "No time logged today yet\n")
          (insert "Use C-c C-x C-i to clock in to tasks\n\n"))
      (let ((total-minutes 0)
            (last-file ""))
        (insert (format "%-30s %-35s %8s\n" "File" "Task" "Duration"))
        (insert (make-string 30 ?-) " " (make-string 35 ?-) " " (make-string 8 ?-) "\n")
        (dolist (entry entries)
          (let ((file (nth 0 entry))
                (task (nth 1 entry))
                (minutes (nth 2 entry))
                (duration-str (nth 3 entry))
                (display-file ""))
            ;; Only show filename if it's different from the previous row
            (if (string= file last-file)
                (setq display-file "")
              (setq display-file file
                    last-file file))
            (setq total-minutes (+ total-minutes minutes))
            (let ((formatted-file (if (string-empty-p display-file)
                                      ""
                                    (if (> (length display-file) 30)
                                        (concat (substring display-file 0 27) "...")
                                      display-file))))
              (insert (format "%-30s %-35s %8s\n"
                              formatted-file
                              (truncate-string-to-width (or task "No title") 35 nil nil "…")
                              duration-str)))))
        (insert (make-string 30 ?-) " " (make-string 35 ?-) " " (make-string 8 ?-) "\n")
        (insert (format "%-30s %-35s %8s\n" "" "TOTAL" (time-dashboard--format-duration total-minutes)))
        (insert "\n")))))

(provide 'time-dashboard)
