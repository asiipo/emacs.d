;;; -*- lexical-binding: t; -*-
(require 'reading-tracker)  ;; provides my/org-reading-file, collectors, helpers
(require 'org)               ;; org-entry-get, etc.
(require 'org-table)         ;; org-table-align



;;; ----------------------------------------------------------------------
;;; Startup "Welcome / Keys" page
;;; - Shows your important key bindings
;;; - Clickable commands
;;; - Reopen anytime with C-c h
;;; ----------------------------------------------------------------------


;; Don’t show the GNU splash or a scratch message
(setq inhibit-startup-screen t
      initial-scratch-message nil)


;; Curate your key cheatsheet here (add/remove as you like).
;; Format: either (:section "Title") or ("KEYS" "Description" COMMAND-or-nil)
(defvar my/cheatsheet-keys
  '((:section "Org & Agenda")
    ("C-c a"   "Open agenda"                         org-agenda)
    ("C-c c"   "Capture menu"                        org-capture)
    ("C-c c t" "Capture: Task to Tasks ▸ Inbox"      nil)

    (:section "Reading tracker")
    ("C-c r o" "Open reading.org"                    my/org-reading-open)
    ("C-c r a" "Add a book"                          my/org-reading-add-book)
    ("C-c r u" "Update current page"                 my/org-reading-set-current-page)
    ("C-c r d" "Delete a book"                       my/org-reading-delete-book)


    (:section "Everyday")
    ("C-x C-f" "Find/open file"                      find-file)
    ("C-x C-s" "Save current buffer"                 save-buffer)
    ("C-h v user-init-file" "Show init file path"    nil))
  "Rows for the startup cheatsheet buffer.")

;; A tiny major mode for the cheatsheet buffer
(define-derived-mode my/cheatsheet-mode special-mode "Cheatsheet"
  "Mode for the startup key cheatsheet."
  (read-only-mode 1)
  (setq truncate-lines t)
  (define-key my/cheatsheet-mode-map (kbd "g") #'my/cheatsheet-refresh)
  (define-key my/cheatsheet-mode-map (kbd "q") #'quit-window))

(defun my/cheatsheet--insert-line (key desc cmd)
  "Insert one cheatsheet row. If CMD is non-nil, make it clickable."
  (let ((col-key 18)    ;; width for Key column
        (col-desc 42))  ;; width for Description column
    (insert (format (format "%%-%ds %%-%ds " col-key col-desc) key desc))
    (if (and cmd (symbolp cmd))
        (insert-text-button
         (symbol-name cmd)
         'help-echo (format "Run %s" cmd)
         'action (lambda (_btn) (call-interactively cmd)))
      (insert "—"))
    (insert "\n")))

;; Add reading info to the welcome page
;; --- Reading dashboard on Welcome page (reusing reading-tracker.el) ----------

(defun my/welcome--visit-marker (mk)
  "Jump to book at marker MK in reading.org."
  (interactive)
  (switch-to-buffer (marker-buffer mk))
  (goto-char mk)
  (org-show-entry))

(defun my/welcome--collect-reading-rows ()
  "Return list of (TITLE LEFT PCT-STR PPD-STR) using reading-tracker helpers."
  (let (rows)
    (dolist (it (my/org--collect-reading-headings))
      (let* ((mk    (cdr it))
             (title (car it)))
        (with-current-buffer (marker-buffer mk)
          (org-with-wide-buffer
            (goto-char mk)
            (let* ((total   (string-to-number (or (org-entry-get (point) "TOTAL_PAGES") "0")))
                   (current (string-to-number (or (org-entry-get (point) "CURRENT_PAGE") "0")))
                   (left    (max 0 (- total current)))
                   (pct     (if (> total 0) (* 100.0 (/ current (float total))) 0.0))
                   (dleft   (my/org--days-left-at-point))
                   (ppd     (and (> left 0) (numberp dleft)
                                 (ceiling (/ (float left) dleft)))))
              (when (> total 0)
                (push (list
                       ;; avoid breaking the table if title contains '|'
                       (replace-regexp-in-string "|" "/" title)
                       left
                       (format "%.1f" pct)
                       (if ppd (number-to-string ppd) "—"))
                      rows)))))))
    (nreverse rows)))

(defun my/welcome--insert-reading-dashboard ()
  "Insert an Org-style table dashboard and align it."
  (let ((rows (my/welcome--collect-reading-rows)))
    (insert "\nReading dashboard\n=================\n")
    (insert "| Title | Left | % | Pages/day |\n")
    (insert "|-+-----+-----+-----------|\n")
    (if (null rows)
        (insert "| (no books yet — use C-c r a) | | | |\n")
      (dolist (r rows)
        (insert (format "| %s | %d | %s | %s |\n"
                        (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r)))))
    (save-excursion (org-table-align))))


(define-derived-mode my/cheatsheet-mode special-mode "Cheatsheet"
  "Mode for the startup key cheatsheet."
  (read-only-mode 1)
  (setq truncate-lines t)
  (setq-local buffer-face-mode-face 'fixed-pitch)
  (buffer-face-mode 1)
  (orgtbl-mode 1)                                 ;; <— important
  (define-key my/cheatsheet-mode-map (kbd "g") #'my/cheatsheet-refresh)
  (define-key my/cheatsheet-mode-map (kbd "q") #'quit-window))



(defun my/cheatsheet--render ()
  "Render the cheatsheet content in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Welcome 👋  — Reading List and Quick Keys\n")
    (insert "Press RET on a command name to run it • g to refresh • q to close\n")
       
    (my/welcome--insert-reading-dashboard)
    (insert "\n")
    
    (insert (format "%-18s %-42s %s\n" "Key" "What it does" "Command"))
    (insert (make-string 18 ?-) " " (make-string 42 ?-) " " (make-string 10 ?-) "\n")
    (dolist (row my/cheatsheet-keys)
      (if (and (consp row) (keywordp (car row)) (eq (car row) :section))
          (progn
            (insert "\n" (cadr row) "\n")
            (insert (make-string (length (cadr row)) ?=) "\n"))
        (pcase-let ((`(,key ,desc ,cmd) row))
          (my/cheatsheet--insert-line key desc cmd))))
    (goto-char (point-min))))

(defun my/cheatsheet-show ()
  (interactive)
  (let ((buf (get-buffer-create "*Welcome – Keys*")))
    (with-current-buffer buf
      (my/cheatsheet-mode)
      (my/cheatsheet--render))
    (switch-to-buffer buf)))     ;; use the current window


(defun my/cheatsheet-refresh ()
  "Re-render the cheatsheet buffer."
  (interactive)
  (when (derived-mode-p 'my/cheatsheet-mode)
    (my/cheatsheet--render)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (my/cheatsheet-show)
            (delete-other-windows)))  ;; ensure a single window


(provide 'welcome)
