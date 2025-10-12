;;; gtd.el --- Simple GTD helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a minimal helper `my/gtd-insert-today' which ensures `gtd.org`
;; exists and inserts today's headline with three subheadings: Routines,
;; Notes, and Summary. The insertion is idempotent: if today's headline
;; already exists, the function will just jump to it.

;;; Code:

(require 'org)

(defvar my/gtd-file
  (expand-file-name "gtd.org" (or (and (boundp 'org-directory) org-directory) "~"))
  "Path to the GTD file used by helper functions.")

(defconst my/gtd--date-headline-re "^\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regex pattern for matching GTD date headlines.")

(defconst my/gtd--clock-entry-re "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\] => +\\([0-9]+:[0-9]+\\)"
  "Regex pattern for matching org-mode clock entries.")

(defun my/gtd--today-string (&optional time)
  "Return a date string for TIME (defaults to now) like 'YYYY-MM-DD Dayname'." 
  (let* ((tm (or time (current-time)))
         (date (format-time-string "%Y-%m-%d" tm))
         (day (format-time-string "%A" tm)))
    (format "%s %s" date day)))

(defun my/gtd--ensure-file ()
  "Ensure `my/gtd-file' exists with a title." 
  (let ((dir (file-name-directory my/gtd-file)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t)))
  (unless (file-exists-p my/gtd-file)
    (with-temp-file my/gtd-file
      (insert "#+TITLE: GTD\n\n"))))

;;;###autoload
(defun my/gtd-insert-today (&optional time)
  "Insert today's GTD headline and minimal subtree into `my/gtd-file'.
If the headline already exists, jump to it instead of inserting.
Opens the file and positions point at the headline.
" 
  (interactive)
  (my/gtd--ensure-file)
  (let* ((headline (my/gtd--today-string time))
         (buf (find-file my/gtd-file))
         (headline-pos nil))
    (goto-char (point-min))
    (setq headline-pos (re-search-forward (regexp-quote headline) nil t))
    (if headline-pos
        (progn
          (goto-char (line-beginning-position))
          (org-show-subtree)
          (recenter)
          (message "GTD: Jumped to today's headline"))
      ;; insert after #+TITLE line (or at end if no title)
      (goto-char (point-min))
      (if (re-search-forward "^#\\+TITLE:" nil t)
          (progn
            (forward-line 1)
            ;; Skip any blank lines after title
            (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
              (forward-line 1)))
        ;; No title found, go to beginning
        (goto-char (point-min)))
      (unless (bolp) (insert "\n"))
      (let ((insert-pos (point)))
        (insert (format "* %s\n" headline))
        (insert "** Routines\n")
        (insert " - [ ] \n")
        (insert "** Notes\n")
        (insert "** Summary\n")
        ;; Insert a clocktable for today's work only
        (insert "#+BEGIN: clocktable :scope tree1 :maxlevel 3 :block today :tags t\n")
        (insert "#+END:\n")
        (save-buffer)
        (goto-char insert-pos)
        (org-show-subtree)
        (recenter)
        (message "GTD: Created today's headline")))))

;;;###autoload
(defun my/gtd-insert-tomorrow ()
  "Insert tomorrow's GTD headline and minimal subtree into `my/gtd-file'.
If the headline already exists, jump to it instead of inserting.
Opens the file and positions point at the headline."
  (interactive)
  (let ((tomorrow (time-add (current-time) (* 24 60 60))))
    (my/gtd-insert-today tomorrow)))

;;;###autoload
(defun my/gtd-open ()
  "Prompt user to choose Today, Tomorrow, weekly review, time reports, or archive.
This is meant to be bound to a key like C-c G."
  (interactive)
  (let ((choice (read-char-choice 
                 "GTD: [t]oday, [T]omorrow, [w]eek review, [r]eports, [a]rchive? " 
                 '(?t ?T ?w ?r ?a))))
    (cond
     ((eq choice ?t)
      (my/gtd-insert-today))
     ((eq choice ?T)
      (my/gtd-insert-tomorrow))
     ((eq choice ?w)
      (my/gtd-weekly-review))
     ((eq choice ?r)
      (my/gtd-time-reports))
     ((eq choice ?a)
      (my/gtd-archive-old-entries))
     (t
      (my/gtd-insert-today)))))

;;;###autoload
(defun my/gtd-weekly-review ()
  "Open GTD file and show the last 7 days for weekly review."
  (interactive)
  (my/gtd--ensure-file)
  (find-file my/gtd-file)
  (goto-char (point-min))
  (org-overview)
  ;; Expand last 7 entries
  (let ((count 0)
        (max-days 7))
    (while (and (< count max-days) 
                (re-search-forward my/gtd--date-headline-re nil t))
      (org-show-entry)
      (org-show-children)
      (setq count (1+ count)))
    (goto-char (point-min))
    (message "GTD: Showing last %d days for weekly review" count)))

;;;###autoload
(defun my/gtd-archive-old-entries (&optional days)
  "Archive GTD entries older than DAYS (default 90).
Creates a 'gtd-archive.org' file with old entries.
When called interactively or without arguments, prompts for the number of days."
  (interactive)
  (let* ((days-old (if (or (called-interactively-p 'interactive) (not days))
                       (read-number "Archive entries older than how many days? " 90)
                     days))
         (cutoff-time (time-subtract (current-time) (* days-old 86400)))
         (archive-file (concat my/gtd-file "_archive"))
         (entries-to-archive '()))
    (my/gtd--ensure-file)
    (with-current-buffer (find-file-noselect my/gtd-file)
      ;; First pass: collect entries to archive
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t)
          (let* ((date-str (match-string 1))
                 (entry-time (org-time-string-to-time date-str)))
            (when (time-less-p entry-time cutoff-time)
              (let ((entry-start (line-beginning-position))
                    (entry-end (save-excursion 
                                (org-end-of-subtree t t)
                                (point))))
                (push (cons entry-start entry-end) entries-to-archive))))))
      ;; Second pass: archive and delete in reverse order (preserves positions)
      (dolist (entry (reverse entries-to-archive))
        (let ((start (car entry))
              (end (cdr entry)))
          ;; Copy to archive
          (append-to-file start end archive-file)
          ;; Delete from current file
          (delete-region start end))))
    (let ((count (length entries-to-archive)))
      (when (> count 0)
        (save-buffer)
        (message "GTD: Archived %d old entries to %s" count archive-file))
      (when (= count 0)
        (message "GTD: No entries older than %d days found" days-old)))))

;;;###autoload
(defun my/gtd-time-reports ()
  "Prompt for which time report to show."
  (interactive)
  (let ((choice (read-char-choice 
                 "Time Report: [w]eek or [m]onth? " 
                 '(?w ?m))))
    (cond
     ((eq choice ?w)
      (my/gtd-time-report-week))
     ((eq choice ?m)
      (my/gtd-time-report-month))
     (t
      (my/gtd-time-report-week)))))

(defun my/gtd--time-report-common (title clock-block days-back)
  "Generate a time report with TITLE using CLOCK-BLOCK for the last DAYS-BACK days.
CLOCK-BLOCK should be 'thisweek' or 'thismonth'.
Returns the report buffer."
  (my/gtd--ensure-file)
  (let* ((buf (get-buffer-create "*GTD Time Report*"))
         (gtd-buf (find-file-noselect my/gtd-file))
         (now (current-time))
         (start-time (time-subtract now (* days-back 24 60 60))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: GTD Time Report - %s\n\n" title))
        (insert (format "#+BEGIN: clocktable :scope file :maxlevel 4 :block %s :tags t :link t\n" clock-block))
        (insert "#+END:\n\n")
        ;; Insert the actual file content temporarily to run clocktable
        (insert-buffer-substring gtd-buf)
        (goto-char (point-min))
        ;; Update the clocktable
        (when (search-forward "#+BEGIN: clocktable" nil t)
          (org-ctrl-c-ctrl-c))
        ;; Remove the file content, keep only the report
        (goto-char (point-min))
        (when (search-forward "#+END:" nil t)
          (forward-line 1)
          (delete-region (point) (point-max)))
        ;; Add aggregate table (pass gtd-buf to avoid re-loading)
        (goto-char (point-max))
        (insert "\n* Time by Tag (Aggregate)\n\n")
        (my/gtd-insert-tag-aggregate start-time now gtd-buf)
        (goto-char (point-min))
        (setq buffer-read-only t)))
    buf))

(defun my/gtd-time-report-week ()
  "Show time tracking report for the current week in a dedicated buffer."
  (interactive)
  (switch-to-buffer (my/gtd--time-report-common "This Week" 'thisweek 7)))

(defun my/gtd-time-report-month ()
  "Show time tracking report for the current month in a dedicated buffer."
  (interactive)
  (switch-to-buffer (my/gtd--time-report-common "This Month" 'thismonth 30)))

(defun my/gtd-collect-tag-times (start-time end-time &optional buffer)
  "Collect all clock times grouped by tag between START-TIME and END-TIME.
Returns a hash table mapping tag -> total-minutes.
If BUFFER is provided, use it instead of loading the file again."
  (let ((tag-minutes (make-hash-table :test 'equal))
        (gtd-buf (or buffer (find-file-noselect my/gtd-file))))
    (with-current-buffer gtd-buf
      (save-excursion
        ;; Parse all clock entries in the range
        (goto-char (point-min))
        (while (re-search-forward my/gtd--clock-entry-re nil t)
          (let* ((start-str (match-string 1))
                 (duration-str (match-string 3))
                 (start-ts (org-time-string-to-time start-str))
                 (minutes (round (org-duration-to-minutes duration-str))))
            ;; Check if within time range
            (when (and minutes
                      (time-less-p start-time start-ts)
                      (time-less-p start-ts end-time))
              ;; Get tags from parent heading
              (save-excursion
                (org-back-to-heading t)
                (let ((tags (org-get-tags)))
                  (if tags
                      ;; Add to each tag
                      (dolist (tag tags)
                        (puthash tag
                                 (+ (gethash tag tag-minutes 0) minutes)
                                 tag-minutes))
                    ;; No tags - count as "untagged"
                    (puthash "untagged"
                             (+ (gethash "untagged" tag-minutes 0) minutes)
                             tag-minutes)))))))))
    tag-minutes))

(defun my/gtd-insert-tag-aggregate (start-time end-time &optional buffer)
  "Insert an aggregate table showing total time per tag.
If BUFFER is provided, use it instead of loading the file again."
  (let* ((tag-hash (my/gtd-collect-tag-times start-time end-time buffer))
         (tag-list '())
         (max-tag-length 5)) ; Minimum of 5 for "TOTAL"
    ;; Convert hash to sorted list and find max tag length
    (maphash (lambda (tag minutes)
               (push (cons tag minutes) tag-list)
               (setq max-tag-length (max max-tag-length (length tag))))
             tag-hash)
    (setq tag-list (sort tag-list (lambda (a b) (> (cdr a) (cdr b)))))
    
    ;; Insert table
    (if tag-list
        (progn
          ;; Calculate total time
          (let ((total-minutes (apply '+ (mapcar 'cdr tag-list)))
                (pad (lambda (str width)
                       (let ((padding (- width (length str))))
                         (concat str (make-string (max 0 padding) ?\s))))))
            (insert (format "| %s | Time  |\n" (funcall pad "Tag" max-tag-length)))
            (insert (format "|-%s-+-------|\n" (make-string max-tag-length ?-)))
            (dolist (entry tag-list)
              (let* ((tag (car entry))
                     (minutes (cdr entry))
                     (hours (/ minutes 60))
                     (mins (% minutes 60)))
                (insert (format "| %s | %2d:%02d |\n" (funcall pad tag max-tag-length) hours mins))))
            (insert (format "|-%s-+-------|\n" (make-string max-tag-length ?-)))
            (let ((total-hours (/ total-minutes 60))
                  (total-mins (% total-minutes 60)))
              (insert (format "| %s | %2d:%02d |\n" (funcall pad "TOTAL" max-tag-length) total-hours total-mins))))
          (insert "\n"))
      (insert "No tagged time entries found in this period.\n"))))

(provide 'gtd)

;;; gtd.el ends here
