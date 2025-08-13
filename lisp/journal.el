;;; journal.el --- Simple DateTree journal  -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal journaling helper using a single DateTree file at ~/org/journal.org.
;; Provides:
;;  - Capture template "j": Journal (today) → DateTree in journal.org
;;  - Command `my/journal-capture-today` (bound to C-c j)
;;  - (Optional) `my/journal-open-today` to jump to today's node

;; Uses `org-directory` defined centrally in init.el

;;; Code:

(require 'org)
(require 'org-datetree)
(require 'subr-x)


(defvar my/org-journal-file (expand-file-name "journal.org" org-directory)
  "Path to the journal file (DateTree).")

(defun my/journal--ensure-file ()
  "Create the journal file with a title if it doesn't exist."
  (unless (file-exists-p my/org-journal-file)
    (with-temp-file my/org-journal-file
      (insert "#+TITLE: Journal\n"))))

;; Capture template (adds non-destructively to your existing templates)
(with-eval-after-load 'org
  (my/journal--ensure-file)
  (let ((tpl '("j" "Journal (today)" entry
               (file+datetree my/org-journal-file)
               "* %<%H:%M> %?\n")))
    (if (boundp 'org-capture-templates)
        (add-to-list 'org-capture-templates tpl t)
      (setq org-capture-templates (list tpl)))))

(defun my/journal-capture-today ()
  "Capture a journal entry into today's DateTree using template \=j\."
  (interactive)
  (my/journal--ensure-file)
  (org-capture nil "j"))

(defun my/journal-open-today ()
  "Open the journal file and jump/create today's DateTree node."
  (interactive)
  (my/journal--ensure-file)
  (find-file my/org-journal-file)
  (org-datetree-find-date-create (calendar-current-date))
  (org-show-entry))


;; Keybindings: global + inside org-agenda
(global-set-key (kbd "C-c j") #'my/journal-capture-today)



(with-eval-after-load 'org
  (add-to-list 'org-agenda-files my/org-journal-file))


(with-eval-after-load 'org
  ;; Offer journal day nodes as refile targets and prefer single-prompt paths
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  ;; Ensure agenda files are general refile targets, in addition to journal days
  (let* ((agenda-target '(org-agenda-files :maxlevel . 9))
         (journal-target `(,my/org-journal-file :maxlevel . 3)))
    (unless (member agenda-target org-refile-targets)
      (setq org-refile-targets (cons agenda-target org-refile-targets)))
    (unless (member journal-target org-refile-targets)
      (setq org-refile-targets (cons journal-target org-refile-targets))))

  ;; Only allow day headings (level 3) as targets within the journal file
  (defun my/journal--refile-target-day-only ()
    (let ((is-journal (and buffer-file-name
                           (file-equal-p buffer-file-name my/org-journal-file))))
      (if (not is-journal)
          t
        (= (org-outline-level) 3))))

  (let ((prev org-refile-target-verify-function))
    (setq org-refile-target-verify-function
          (if prev
              (lambda () (and (funcall prev)
                              (my/journal--refile-target-day-only)))
            #'my/journal--refile-target-day-only))))

(provide 'journal)
;;; journal.el ends here
