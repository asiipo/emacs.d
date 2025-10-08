;;; org-capture-config.el --- Capture inbox and templates -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures org-capture with PARA-method templates for efficient
;; task and content capture into proper organizational structure.

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)

;; ============================================================================
;; INBOX CONFIGURATION
;; ============================================================================

;; Set inbox as the default capture target
;; Based on Org Mode Guide section 9.1
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

;; Create inbox file if it doesn't exist
(unless (file-exists-p org-default-notes-file)
  (with-temp-file org-default-notes-file
    (insert "#+TITLE: Inbox\n")))

;; ============================================================================
;; CAPTURE HELPER FUNCTIONS
;; ============================================================================

;; Helper system to pass titles from target functions into templates
(defvar my/capture--title nil
  "Temporary storage for capture title during file creation.")

(defun my/capture--set-title (s)
  "Store title S for use in capture template."
  (setq my/capture--title s))

(defun my/capture-pop-title ()
  "Return stored title and clear it for next use."
  (prog1 (or my/capture--title "")
    (setq my/capture--title nil)))

(defun my/capture--unique-filename (dir base-name extension)
  "Generate a unique filename in DIR with BASE-NAME and EXTENSION.
If the file exists, append -2, -3, etc. until we find an unused name."
  (let* ((base-file (expand-file-name (concat base-name extension) dir))
         (counter 2)
         (file base-file))
    (while (file-exists-p file)
      (setq file (expand-file-name 
                  (format "%s-%d%s" base-name counter extension) 
                  dir))
      (setq counter (1+ counter)))
    file))

;; ============================================================================
;; CAPTURE TEMPLATES
;; ============================================================================

;; PARA-method capture templates for comprehensive content organization
(setq org-capture-templates
      `(;; ========== INBOX TEMPLATES ==========
        
        ;; Task: Quick TODO item to inbox
        ("t" "Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nCREATED: %U\n%i\n")
        
        ;; Note: Tagged note to inbox
        ("n" "Note" entry 
         (file ,org-default-notes-file)
         "* %^{Note title} :%^{tags|work|home|research|admin|deep|quick|:}:\nCREATED: %U\n\n%?\n%i\n")

        ;; Scheduled task: Task with specific date/time
        ("s" "Scheduled Task" entry
         (file ,org-default-notes-file)
         "* TODO %?\nSCHEDULED: %^T\nCREATED: %U\n%i\n")

        ;; Task with deadline
        ("d" "Task with Deadline" entry
         (file ,org-default-notes-file)
         "* TODO %?\nDEADLINE: %^T\nCREATED: %U\n%i\n")

        ;; Meeting: Meeting notes with scheduled time
        ("m" "Meeting" entry
         (file ,org-default-notes-file)
         ,(concat "* Meeting: %? :meeting:\n"
                  "SCHEDULED: %^T\n"
                  "CREATED: %U\n"
                  ":PROPERTIES:\n"
                  ":ATTENDEES: %^{Attendees}\n"
                  ":END:\n\n"
                  "** Agenda\n\n** Notes\n\n** Action Items\n"))
        
        ;; ========== PARA FILE TEMPLATES ==========
        
        ;; Project: Create new project file with template
        ("p" "New Project (file)" plain
         (file (lambda ()
                 (let* ((title (read-string "Project title: "))
                        (slug (my/slugify title))
                        (dir (expand-file-name "projects" org-directory))
                        (file (my/capture--unique-filename dir slug ".org")))
                   (make-directory dir t)
                   (my/capture--set-title title)
                   file)))
         ,(concat "#+TITLE: %(my/capture-pop-title)\n"
                  "#+CATEGORY: Project\n"
                  ":PROPERTIES:\n"
                  ":AREA: %^{Area|Research|Teaching|Admin|Personal}\n"
                  ":STATUS: Active\n"
                  ":CREATED: %U\n"
                  ":END:\n\n"
                  "* Overview\n%?\n\n"
                  "* Goals\n- [ ] \n\n"
                  "* Next actions\n- TODO \n\n"
                  "* Waiting for\n\n"
                  "* Notes\n"))
        
        ;; Area: Create new area file with template
        ("a" "New Area (file)" plain
         (file (lambda ()
                 (let* ((title (read-string "Area name: "))
                        (slug (my/slugify title))
                        (dir (expand-file-name "areas" org-directory))
                        (file (my/capture--unique-filename dir slug ".org")))
                   (make-directory dir t)
                   (my/capture--set-title title)
                   file)))
         ,(concat "#+TITLE: %(my/capture-pop-title)\n"
                  "#+CATEGORY: Area\n"
                  ":PROPERTIES:\n"
                  ":PURPOSE: %^{Purpose of this area}\n"
                  ":REVIEW: weekly\n"
                  ":CREATED: %U\n"
                  ":END:\n\n"
                  "* Purpose\n%?\n\n"
                  "* Standards\n"
                  "- What \"good\" looks like for this area\n"
                  "- Key metrics or indicators of success\n\n"
                  "* Current focus\n"
                  "- TODO Review and maintain standards\n"
                  "- TODO Plan next steps\n\n"
                  "* Ongoing maintenance\n"
                  "- TODO Regular review of this area\n\n"
                  "* Resources\n"
                  "- Links, documents, or tools related to this area\n\n"
                  "* Notes\n"))
        
        ;; Resource: Create new resource file with template
        ("r" "Resource (file)" plain
         (file (lambda ()
                 (let* ((topic (read-string "Resource topic: "))
                        (slug (my/slugify topic))
                        (dir (expand-file-name "resources" org-directory))
                        (file (my/capture--unique-filename dir slug ".org")))
                   (make-directory dir t)
                   (my/capture--set-title topic)
                   file)))
         ,(concat "#+TITLE: %(my/capture-pop-title)\n"
                  "#+CATEGORY: Resource\n"
                  ":PROPERTIES:\n"
                  ":SOURCE: %^{Link or source}\n"
                  ":TYPE: %^{Type|Article|Book|Tool|Documentation|Course}\n"
                  ":CREATED: %U\n"
                  ":END:\n\n"
                  "* Summary\n%?\n\n"
                  "* Key Points\n- \n\n"
                  "* Action Items\n"
                  "- TODO Review and extract insights\n\n"
                  "* Notes\n"))))

;; ============================================================================
;; KEYBINDINGS AND SETTINGS
;; ============================================================================

;; Keybindings are now centralized in keybindings.el

;; Enable speed commands for faster Org navigation
;; Based on Org Mode Guide section 2.1
(setq org-use-speed-commands t)

(provide 'org-capture-config)

