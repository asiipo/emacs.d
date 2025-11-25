;;; gtd.el --- Simple GTD helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides GTD daily logging functions for managing `gtd.org`.
;;
;; Main features:
;; - `my/gtd-insert-today': Create/jump to today's headline with customizable
;;   sections (default: Routines, Notes, Summary via `my/gtd-daily-sections')
;; - `my/gtd-weekly-review': Show current week entries (ISO week-based)
;; - `my/gtd-last-week-review': Show last week entries
;; - `my/gtd-time-reports': Generate clocktable reports with tag aggregation
;; - `my/gtd-open': Main entry point bound to C-c G
;;
;; All daily headline operations are idempotent: if a headline exists,
;; the function will jump to it rather than creating a duplicate.

;;; Code:

(require 'org)

(defvar my/gtd-file
  (expand-file-name "gtd.org" (or (and (boundp 'org-directory) org-directory) "~"))
  "Path to the GTD file used by helper functions.")

(defconst my/gtd--date-headline-re "^\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regex pattern for matching GTD date headlines.")

(defconst my/gtd--headline-prefix-length 2
  "Length of '* ' prefix in Org headlines.")

(defconst my/gtd--date-string-length 10
  "Length of 'YYYY-MM-DD' date string.")

(defconst my/gtd--seconds-per-day 86400
  "Number of seconds in a day (24 * 60 * 60).")

(defcustom my/gtd-daily-sections
  '(("Routines" "")
    ("Notes" "")
    ("Summary" ""))
  "List of sections to create under each daily headline.
Each element is a list (SECTION-NAME INITIAL-CONTENT).
SECTION-NAME is the heading text, INITIAL-CONTENT is inserted below."
  :type '(repeat (list (string :tag "Section Name")
                       (string :tag "Initial Content")))
  :group 'org)

(defconst my/gtd--clock-entry-re "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\] => +\\([0-9]+:[0-9]+\\)"
  "Regex pattern for matching org-mode clock entries.")

(defun my/gtd--today-string (&optional time)
  "Return a date string for TIME (defaults to now) like 'YYYY-MM-DD Dayname'."
  (let* ((tm (or time (current-time)))
         (date (format-time-string "%Y-%m-%d" tm))
         (day (format-time-string "%A" tm)))
    (format "%s %s" date day)))

(defun my/gtd--extract-date-from-headline ()
  "Extract 'YYYY-MM-DD' date string from current matched headline.
Assumes point is after a successful `re-search-forward' with `my/gtd--date-headline-re'.
Returns the date string."
  (buffer-substring-no-properties
   (+ (match-beginning 0) my/gtd--headline-prefix-length)
   (+ (match-beginning 0) my/gtd--headline-prefix-length my/gtd--date-string-length)))

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
Opens the file and positions point at the headline."
  (interactive)
  (my/gtd--ensure-file)
  (let* ((headline (my/gtd--today-string time)))
    (find-file my/gtd-file)
    (goto-char (point-min))
    (if-let ((headline-pos (re-search-forward (regexp-quote headline) nil t)))
        ;; Headline exists - jump to it
        (progn
          (goto-char (line-beginning-position))
          (org-show-subtree)
          (recenter)
          (message "GTD: Jumped to today's headline"))
      ;; Headline doesn't exist - create it
      (goto-char (point-min))
      ;; Find insertion point: after #+TITLE and blank lines, or at beginning
      (when (re-search-forward "^#\\+TITLE:" nil t)
        (forward-line 1)
        (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
          (forward-line 1)))
      (unless (bolp) (insert "\n"))
      (let ((insert-pos (point)))
        ;; Insert headline
        (insert (format "* %s\n" headline))
        ;; Insert customizable sections
        (dolist (section my/gtd-daily-sections)
          (let ((section-name (car section))
                (initial-content (cadr section)))
            (insert (format "** %s\n" section-name))
            (when (and initial-content (not (string-empty-p initial-content)))
              (insert initial-content))))
        ;; Insert clocktable for today's work
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
  (let ((tomorrow (time-add (current-time) my/gtd--seconds-per-day)))
    (my/gtd-insert-today tomorrow)))

;;;###autoload
(defun my/gtd-open ()
  "Prompt user to choose Today, Tomorrow, weekly review, or time reports.
This is meant to be bound to a key like C-c G."
  (interactive)
  (let ((choice (read-char-choice 
                 "GTD: [t]oday, [T]omorrow, [w]eek, [l]ast week, [r]eports? " 
                 '(?t ?T ?w ?l ?r))))
    (cond
     ((eq choice ?t)
      (my/gtd-insert-today))
     ((eq choice ?T)
      (my/gtd-insert-tomorrow))
     ((eq choice ?w)
      (my/gtd-weekly-review))
     ((eq choice ?l)
      (my/gtd-last-week-review))
     ((eq choice ?r)
      (my/gtd-time-reports))
     (t
      (my/gtd-insert-today)))))

(defun my/gtd--weekly-review-for (week-offset)
  "Show entries for a specific week based on WEEK-OFFSET from current week.
WEEK-OFFSET of 0 means current week, -1 means last week, etc."
  (my/gtd--ensure-file)
  (find-file my/gtd-file)
  (goto-char (point-min))
  (org-overview)
  
  ;; Calculate target week
  (let* ((target-time (time-add (current-time) (* week-offset 7 my/gtd--seconds-per-day)))
         (target-week (string-to-number (format-time-string "%V" target-time)))
         (target-year (string-to-number (format-time-string "%Y" target-time)))
         (count 0)
         (found-target-week nil))
    
    ;; Find and expand all entries within target week
    ;; Since entries are in descending order (newest first), stop when we've
    ;; passed the target week (found older entries)
    (catch 'done
      (while (re-search-forward my/gtd--date-headline-re nil t)
        (let* ((date-str (my/gtd--extract-date-from-headline))
               (entry-time (org-time-string-to-time date-str))
               (entry-week (string-to-number (format-time-string "%V" entry-time)))
               (entry-year (string-to-number (format-time-string "%Y" entry-time))))
          (cond
           ;; Target week entry - show it
           ((and (= entry-week target-week)
                 (= entry-year target-year))
            (org-show-entry)
            (org-show-subtree)
            (setq count (1+ count))
            (setq found-target-week t))
           ;; Older entry after finding target week - stop searching
           ((and found-target-week
                 (or (< entry-year target-year)
                     (and (= entry-year target-year)
                          (< entry-week target-week))))
            (throw 'done nil))))))
    
    (goto-char (point-min))
    (if (> count 0)
        (message "GTD: Showing %d entries for week %d of %d" count target-week target-year)
      (message "GTD: No entries found for week %d of %d" target-week target-year))))

;;;###autoload
(defun my/gtd-weekly-review ()
  "Open GTD file and show entries for the current week based on week number."
  (interactive)
  (my/gtd--weekly-review-for 0))

;;;###autoload
(defun my/gtd-last-week-review ()
  "Open GTD file and show entries for last week based on week number."
  (interactive)
  (my/gtd--weekly-review-for -1))

;;;###autoload
(defun my/gtd-time-reports ()
  "Prompt for which time report to show."
  (interactive)
  (let ((choice (read-char-choice 
                 "Time Report: [w]eek, [m]onth, or [c]ustom date? " 
                 '(?w ?m ?c))))
    (cond
     ((eq choice ?w)
      (my/gtd-time-report-week))
     ((eq choice ?m)
      (my/gtd-time-report-month))
     ((eq choice ?c)
      (my/gtd-time-report-custom))
     (t
      (my/gtd-time-report-week)))))

(defun my/gtd--time-report-common (title clock-block)
  "Generate a time report with TITLE using CLOCK-BLOCK.
CLOCK-BLOCK should be 'thisweek' or 'thismonth'.
Calculates appropriate start time based on block type.
Returns the report buffer."
  (my/gtd--ensure-file)
  (let* ((buf (get-buffer-create "*GTD Time Report*"))
         (gtd-buf (find-file-noselect my/gtd-file))
         (now (current-time))
         ;; Calculate start time based on block type
         (start-time (cond
                      ((eq clock-block 'thisweek)
                       ;; Start of ISO week (Monday = 1, Sunday = 7 in %u format)
                       ;; Calculate days since Monday to get week start
                       (let* ((iso-day-of-week (string-to-number (format-time-string "%u" now)))
                              (days-since-monday (1- iso-day-of-week))
                              (seconds-since-monday (* days-since-monday my/gtd--seconds-per-day)))
                         (time-subtract now seconds-since-monday)))
                      ((eq clock-block 'thismonth)
                       ;; Start of this month
                       (let* ((day-of-month (string-to-number (format-time-string "%d" now))))
                         (time-subtract now (* (1- day-of-month) my/gtd--seconds-per-day))))
                      (t
                       ;; Fallback: 7 days back
                       (time-subtract now (* 7 my/gtd--seconds-per-day))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: GTD Time Report - %s\n\n" title))
        ;; Add aggregate table first (pass gtd-buf to avoid re-loading)
        (insert "* Time by Tag (Aggregate)\n\n")
        (my/gtd--insert-tag-aggregate start-time now gtd-buf)
        (insert "\n")
        ;; Then add the clocktable
        (insert (format "#+BEGIN: clocktable :scope file :maxlevel 4 :block %s :tags t :link t\n" clock-block))
        (insert "#+END:\n\n")
        ;; Insert the actual file content temporarily to run clocktable
        (insert-buffer-substring gtd-buf)
        (goto-char (point-min))
        ;; Update the clocktable with error handling
        (condition-case err
            (when (search-forward "#+BEGIN: clocktable" nil t)
              (org-ctrl-c-ctrl-c))
          (error
           (message "GTD: Warning - Failed to update clocktable: %s" (error-message-string err))
           (insert "\n⚠ Error updating clocktable. Make sure Org mode is properly loaded.\n")))
        ;; Remove the file content, keep only the report
        (goto-char (point-min))
        (when (search-forward "#+END:" nil t)
          (forward-line 1)
          (delete-region (point) (point-max)))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    buf))

(defun my/gtd-time-report-week ()
  "Show time tracking report for the current week in a dedicated buffer."
  (interactive)
  (switch-to-buffer (my/gtd--time-report-common "This Week" 'thisweek)))

(defun my/gtd-time-report-month ()
  "Show time tracking report for the current month in a dedicated buffer."
  (interactive)
  (switch-to-buffer (my/gtd--time-report-common "This Month" 'thismonth)))

(defun my/gtd-time-report-custom ()
  "Show time tracking report from a custom start date to now."
  (interactive)
  (let* ((date-str (read-string "Start date (YYYY-MM-DD): "))
         (start-time (org-time-string-to-time (concat "<" date-str ">")))
         (now (current-time))
         (buf (get-buffer-create "*GTD Time Report*"))
         (gtd-buf (find-file-noselect my/gtd-file)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: GTD Time Report - From %s\n\n" date-str))
        ;; Add aggregate table first
        (insert "* Time by Tag (Aggregate)\n\n")
        (my/gtd--insert-tag-aggregate start-time now gtd-buf)
        (insert "\n")
        ;; Add detailed clocktable
        (insert (format "#+BEGIN: clocktable :scope file :maxlevel 4 :tstart \"%s\" :tend \"<now>\" :tags t :link t\n" date-str))
        (insert "#+END:\n\n")
        ;; Insert file content temporarily to run clocktable
        (insert-buffer-substring gtd-buf)
        (goto-char (point-min))
        (condition-case err
            (when (search-forward "#+BEGIN: clocktable" nil t)
              (org-ctrl-c-ctrl-c))
          (error
           (message "GTD: Warning - Failed to update clocktable: %s" (error-message-string err))
           (insert "\n⚠ Error updating clocktable. Make sure Org mode is properly loaded.\n")))
        ;; Remove file content, keep only report
        (goto-char (point-min))
        (when (search-forward "#+END:" nil t)
          (forward-line 1)
          (delete-region (point) (point-max)))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    (switch-to-buffer buf)))

(defun my/gtd--collect-tag-times (start-time end-time &optional buffer)
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

(defun my/gtd--insert-tag-aggregate (start-time end-time &optional buffer)
  "Insert an aggregate table showing total time per tag.
If BUFFER is provided, use it instead of loading the file again."
  (let* ((tag-hash (my/gtd--collect-tag-times start-time end-time buffer))
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
