;;; llm-assistant.el --- Local LLM Personal Executive Assistant -*- lexical-binding: t; -*-

;; Author: Personal Configuration
;; Version: 5.0
;; Package-Requires: ((emacs "28.1") (gptel "0.9"))
;; Keywords: org, llm, productivity

;;; Commentary:
;; This module integrates a local Ollama LLM into the Org-mode workflow
;; to provide a "Daily Briefing" agent with dual persistent memory.
;;
;; Architecture:
;;   1. Prompts dir (~/org/prompts/) — all LLM files as editable Org:
;;      system.org, briefing.org, agent-notes-update.org, quick-win.org,
;;      memory.org (user profile), agent-notes.org (LLM-owned observations).
;;   2. Context pipeline: Memory → Agent Notes → Agenda → Journal.
;;   3. gptel-request sends the prompt to Ollama asynchronously.
;;   4. Callback renders the response, then auto-updates agent notes.
;;
;; Commands:
;;   M-x my/daily-briefing          (C-c l b) — Generate daily briefing
;;   M-x my/llm-quick-win           (C-c l w) — Single task for 30-min window
;;   M-x my/llm-open-prompts        (C-c l p) — Open prompts directory
;;   M-x gptel                      (C-c l c) — Open LLM chat
;;
;; Phase 2 (deferred): RAG via elisa for org-roam semantic search

;;; Code:

;; ============================================================================
;; DEPENDENCIES
;; ============================================================================

(require 'org)
(require 'org-element)
(require 'org-datetree)
(require 'subr-x)
(require 'cl-lib)

;; ============================================================================
;; CUSTOMIZATION
;; ============================================================================

(defgroup my/llm nil
  "Local LLM assistant for Org-mode workflows."
  :group 'org
  :prefix "my/llm-")

(defcustom my/llm-ollama-host "localhost:11434"
  "Host and port for the Ollama server."
  :type 'string
  :group 'my/llm)

(defcustom my/llm-model 'llama3.1:8b
  "Ollama model to use for the assistant.
Must be already pulled via `ollama pull MODEL'."
  :type 'symbol
  :group 'my/llm)

(defcustom my/llm-context-window 60000
  "Maximum total characters for greedy context stuffing.
This controls our pipeline budget in characters (not tokens).
60000 chars ≈ 15000 tokens — a safe balance for 8B models where
quality degrades after ~16k tokens.
Increase for larger models (e.g. 120000 for 70B).

Note: this is separate from the Ollama-side num_ctx setting
(see `my/llm-ollama-num-ctx') which controls how many tokens
the model can actually process."
  :type 'integer
  :group 'my/llm)

(defcustom my/llm-ollama-num-ctx 32768
  "Context window size in tokens sent to Ollama via num_ctx.
Ollama defaults to 2048 if this is not set, which would silently
truncate most of our assembled context.  32768 (32K) comfortably
covers our ~15K token pipeline plus system prompt and response
generation, while fitting in GPU VRAM.  Increase only if you expand
`my/llm-context-window' significantly or switch to a larger model."
  :type 'integer
  :group 'my/llm)

(defcustom my/llm-journal-lookback-days 90
  "Maximum number of days to look back in journal for context.
The extractor scans backwards day-by-day until the budget is exhausted."
  :type 'integer
  :group 'my/llm)

(defcustom my/llm-ollama-check-timeout 3
  "Timeout in seconds for the Ollama reachability check."
  :type 'integer
  :group 'my/llm)

(defcustom my/llm-prompts-directory nil
  "Directory containing all LLM-related Org files.
Prompts (system, briefing, agent-notes-update, quick-win), the user
profile (memory.org), and agent observations (agent-notes.org) all
live here as editable Org files.
Defaults to ~/org/prompts/ if nil."
  :type '(choice (const nil) directory)
  :group 'my/llm)

;; ============================================================================
;; INTERNAL STATE
;; ============================================================================

(defvar my/llm--backend nil
  "Cached gptel backend object for Ollama.")

(defvar my/llm--briefing-in-progress nil
  "Non-nil while a briefing generation is in progress.")

(defvar my/llm--last-briefing-response nil
  "The last successfully generated briefing response text.")

;; ============================================================================
;; OLLAMA BACKEND SETUP
;; ============================================================================

(defun my/llm--ensure-backend ()
  "Ensure the Ollama gptel backend is configured.  Returns the backend."
  (require 'gptel)
  (require 'gptel-ollama nil t)  ;; might be autoloaded, don't error
  (unless my/llm--backend
    (setq my/llm--backend
          (gptel-make-ollama "Ollama"
            :host my/llm-ollama-host
            :stream t
            :models (list my/llm-model)
            :request-params `(:options (:num_ctx ,my/llm-ollama-num-ctx)))))
  my/llm--backend)

(defun my/llm--ollama-reachable-p ()
  "Return non-nil if the Ollama server at `my/llm-ollama-host' is reachable."
  (condition-case nil
      (let* ((url (format "http://%s/" my/llm-ollama-host))
             (url-request-method "GET")
             (url-show-status nil))
        (with-timeout (my/llm-ollama-check-timeout nil)
          (let ((buf (url-retrieve-synchronously url t nil my/llm-ollama-check-timeout)))
            (when buf
              (prog1 t (kill-buffer buf))))))
    (error nil)))

;; ============================================================================
;; MEMORY FILE — User-owned profile (~/org/prompts/memory.org)
;; ============================================================================

(defun my/llm--ensure-memory-file ()
  "Ensure the memory file exists.  Returns the file path."
  (my/llm--ensure-prompt-file "memory"))

(defun my/llm--extract-memory ()
  "Read the memory file and return its contents as a string."
  (let ((contents (my/llm--safe-file-contents (my/llm--ensure-memory-file))))
    (or contents "(profile not yet initialized)")))

;; ============================================================================
;; AGENT NOTES FILE — LLM-owned observations (~/org/prompts/agent-notes.org)
;; ============================================================================

(defun my/llm--ensure-agent-notes-file ()
  "Ensure the agent notes file exists.  Returns the file path."
  (my/llm--ensure-prompt-file "agent-notes"))

(defun my/llm--extract-agent-notes ()
  "Read the agent notes file and return its contents as a string."
  (let ((contents (my/llm--safe-file-contents (my/llm--ensure-agent-notes-file))))
    (or contents "(agent notes not yet initialized)")))

;; ============================================================================
;; RESPONSE NORMALIZATION
;; ============================================================================

(defun my/llm--normalize-response (response info)
  "Normalize RESPONSE from gptel into a plain content string.
Silently returns nil for reasoning cons cells dispatched by gptel
\(these are not gated by `gptel-include-reasoning' for custom
callbacks).  With `gptel-include-reasoning' set to nil, the main
response text will already be free of <think> blocks."
  (ignore info)
  (cond
   ((and (consp response) (eq (car response) 'reasoning)) nil)
   ((stringp response) response)
   (response (format "%s" response))
   (t nil)))

;; ============================================================================
;; CONTEXT EXTRACTORS
;; ============================================================================

;; Each extractor operates on a temp buffer with the file contents to avoid
;; modifying or locking the user's open buffers.

(defun my/llm--safe-file-contents (file)
  "Return the contents of FILE as a string, or nil if not accessible."
  (when (and file (file-exists-p file) (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun my/llm--extract-journal (char-budget)
  "Extract journal entries going as far back as CHAR-BUDGET allows.
Scans backward from end of file to find date headings efficiently.
Most recent entries first — fills greedily until budget is exhausted."
  (let* ((journal-file (expand-file-name "areas/journal.org" org-directory))
         (contents (my/llm--safe-file-contents journal-file)))
    (if (not contents)
        "(no journal data — file not found)"
      (let ((sections '())
            (total-chars 0)
            (days-found 0)
            (cutoff-date (time-subtract (current-time)
                                        (days-to-time my/llm-journal-lookback-days))))
        (with-temp-buffer
          (insert contents)
          (org-mode)
          ;; Scan backward from end — O(n) single pass instead of O(n·m)
          (goto-char (point-max))
          (catch 'budget-exhausted
            (while (re-search-backward
                    "^\\*\\{1,4\\} .*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
                    nil t)
              (let* ((date-str (match-string 1))
                     (entry-date (date-to-time (concat date-str " 00:00:00"))))
                ;; Stop if we've gone past the lookback window
                (when (time-less-p entry-date cutoff-date)
                  (throw 'budget-exhausted nil))
                ;; Skip today (briefing is about planning, not reflecting on today)
                (unless (string= date-str (format-time-string "%Y-%m-%d"))
                  (let* ((beg (line-beginning-position))
                         (end (save-excursion
                                (org-end-of-subtree t t) (point)))
                         (entry-text (string-trim
                                      (buffer-substring-no-properties beg end)))
                         (entry-len (length entry-text))
                         (label (format-time-string
                                 "%A %Y-%m-%d"
                                 (date-to-time (concat date-str " 00:00:00")))))
                    ;; Check if we still have room
                    (when (> (+ total-chars entry-len 50) char-budget)
                      (throw 'budget-exhausted nil))
                    (push (format "=== %s ===\n%s" label entry-text) sections)
                    (setq total-chars (+ total-chars entry-len 50))
                    (setq days-found (1+ days-found))))))))
        (if sections
            (format "Journal entries found: %d days (lookback: %d days max)\n\n%s"
                    days-found my/llm-journal-lookback-days
                    ;; Reverse to show most recent first (backward scan pushed oldest first)
                    (string-join (nreverse sections) "\n\n"))
          "(no journal entries found)")))))

(defun my/llm--extract-agenda-today ()
  "Extract today's scheduled events and deadlines from org-agenda.
Uses `org-agenda-get-day-entries' for proper repeater support."
  (let* ((today (current-time))
         (decoded (decode-time today))
         (day (nth 3 decoded))
         (month (nth 4 decoded))
         (year (nth 5 decoded))
         (date-list (list month day year))
         (entries '())
         (result-lines '()))
    ;; Gather entries from all agenda files
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (setq entries (append entries
                              (org-agenda-get-day-entries file date-list)))))
    ;; Parse into readable lines
    (dolist (entry entries)
      (let* ((txt (get-text-property 0 'txt entry))
             (type (get-text-property 0 'type entry))
             (time-of-day (get-text-property 0 'time-of-day entry))
             (is-habit (get-text-property 0 'org-habit-p entry)))
        (unless is-habit
          (when (and txt (member type '("scheduled" "deadline" "timestamp")))
            (let* ((time-str (if time-of-day
                                 (format "%02d:%02d" (/ time-of-day 100)
                                         (mod time-of-day 100))
                               "     "))
                   (type-label (cond ((string= type "deadline") "DEADLINE")
                                     ((string= type "scheduled") "SCHEDULED")
                                     (t "EVENT")))
                   (clean-txt (string-trim
                               (replace-regexp-in-string "^[ \t]+" ""
                                (replace-regexp-in-string ":.*:$" "" txt)))))
              (push (format "- %s [%s] %s" time-str type-label clean-txt)
                    result-lines))))))
    (if result-lines
        (format "Events for %s:\n%s"
                (format-time-string "%Y-%m-%d %A")
                (string-join
                 (sort (nreverse result-lines) #'string<)
                 "\n"))
      (format "No scheduled events for %s."
              (format-time-string "%Y-%m-%d %A")))))

(defun my/llm--extract-gtd-today ()
  "Extract GTD entries starting from today, then going backward.
Skips the Habits section at the top of gtd.org.
Strips LOGBOOK drawers to avoid token waste from habit logs.
Fills greedily until char-budget is exhausted."
  (let* ((gtd-file (expand-file-name "gtd.org" org-directory))
         (contents (my/llm--safe-file-contents gtd-file)))
    (if (not contents)
        "(no GTD data — file not found)"
      (let ((sections '())
            (total-chars 0)
            (char-budget 10000)  ; GTD gets 10k chars before journal backfill
            (days-found 0)
            (today-time (current-time)))
        (with-temp-buffer
          (insert contents)
          (org-mode)
          ;; Skip past Habits section (top-level heading)
          (goto-char (point-min))
          (when (re-search-forward "^\\* Habits\\b" nil t)
            (org-end-of-subtree t t))
          ;; Now collect all date entries from this point forward
          (let ((date-entries '())
                (search-start (point)))
            ;; First pass: collect all date entries with their positions
            (goto-char search-start)
            (while (re-search-forward
                    "^\\* \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\b"
                    nil t)
              (let* ((date-str (match-string 1))
                     (entry-date (date-to-time (concat date-str " 00:00:00")))
                     (days-diff (/ (- (float-time today-time)
                                     (float-time entry-date))
                                  86400.0))  ; seconds per day
                     (beg (line-beginning-position))
                     (end (save-excursion (org-end-of-subtree t t) (point))))
                (push (list days-diff date-str beg end) date-entries)))
            ;; Sort by days-diff (ascending: today=0, yesterday=1, etc.)
            (setq date-entries (sort date-entries (lambda (a b) (< (car a) (car b)))))
            ;; Second pass: extract in priority order until budget exhausted
            (catch 'budget-exhausted
              (dolist (entry date-entries)
                (let* ((days-diff (nth 0 entry))
                       (date-str (nth 1 entry))
                       (beg (nth 2 entry))
                       (end (nth 3 entry))
                       (raw-text (buffer-substring-no-properties beg end))
                       ;; Strip LOGBOOK drawers
                       (clean-text (replace-regexp-in-string
                                   ":LOGBOOK:\n\\(?:.*\n\\)*?:END:\n?" "" raw-text))
                       (entry-len (length clean-text))
                       (label (cond
                              ((< days-diff 0.5) "Today")
                              ((< days-diff 1.5) "Tomorrow")
                              ((< days-diff 2) "Yesterday")
                              (t (format-time-string
                                  "%A %Y-%m-%d"
                                  (date-to-time (concat date-str " 00:00:00")))))))
                  ;; Check budget
                  (when (> (+ total-chars entry-len 50) char-budget)
                    (throw 'budget-exhausted nil))
                  (push (format "=== %s: %s ===\n%s" label date-str clean-text) sections)
                  (setq total-chars (+ total-chars entry-len 50))
                  (setq days-found (1+ days-found)))))))
        (if sections
            (format "GTD entries found: %d days\n\n%s"
                    days-found
                    ;; Reverse to show most recent first
                    (string-join (nreverse sections) "\n\n"))
          "(no GTD entries found)")))))

;; ============================================================================
;; GREEDY CONTEXT PIPELINE
;; ============================================================================

(defun my/llm--build-context ()
  "Build context by greedily filling the context window in priority order.
Returns an alist of (LABEL . TEXT) pairs for each context section.
Priority: Memory → Agent Notes → Agenda → GTD → Journal (remaining budget)."
  (message "📋 Extracting context from Org files...")
  (let* ((budget my/llm-context-window)
         (used 0)
         (sections '())
         ;; Extract fixed-size sources upfront
         (memory  (my/llm--extract-memory))
         (notes   (my/llm--extract-agent-notes))
         (agenda  (my/llm--extract-agenda-today))
         (gtd     (my/llm--extract-gtd-today)))
    ;; 1. Memory (human-owned profile) — always included, highest value
    (push (cons "YOUR PROFILE — User-maintained preferences & context" memory) sections)
    (setq used (+ used (length memory)))
    ;; 2. Agent notes (LLM-owned observations) — always included
    (push (cons "AGENT OBSERVATIONS — Patterns & notes from previous sessions" notes) sections)
    (setq used (+ used (length notes)))
    ;; 3. Today's agenda — critical for scheduling
    (push (cons "TODAY'S SCHEDULE" agenda) sections)
    (setq used (+ used (length agenda)))
    ;; 4. GTD today/tomorrow — daily notes and planning
    (push (cons "GTD — Today & Tomorrow" gtd) sections)
    (setq used (+ used (length gtd)))
    ;; 5. Journal — gets ALL remaining budget for maximum history depth
    (let* ((journal-budget (max 2000 (- budget used)))
           (journal (my/llm--extract-journal journal-budget)))
      (push (cons "JOURNAL — Recent history" journal) sections)
      (setq used (+ used (length journal))))
    (message "📋 Context: %d chars (~%.1fk tokens) across %d sections"
             used (/ used 4.0) (length sections))
    (nreverse sections)))

;; ============================================================================
;; PROMPT FILES — Editable Org files in ~/org/prompts/
;; ============================================================================

(defun my/llm--prompts-dir ()
  "Return the prompts directory path."
  (or my/llm-prompts-directory
      (expand-file-name "prompts/" org-directory)))

(defun my/llm--prompt-file (name)
  "Return the full path for prompt NAME (e.g. \"system\")."
  (expand-file-name (concat name ".org") (my/llm--prompts-dir)))

(defconst my/llm--prompt-defaults
  '(("system" . "#+TITLE: System Prompt\n\nYou are a Personal Executive Assistant. Be concise and actionable.\nCustomize this file to define the assistant's role, rules, and behaviour.\n")
    ("briefing" . "#+TITLE: Briefing Prompt\n\nGenerate a daily briefing.  Today is {{DATE}}, current time {{TIME}}.\nCustomize this file to control briefing format and instructions.\n")
    ("agent-notes-update" . "#+TITLE: Agent Notes Update Prompt\n\nYou are updating your observation notes about the user.\nCustomize this file to control how agent notes are maintained.\n")
    ("quick-win" . "#+TITLE: Quick Win Prompt\n\nSuggest the single most effective task for a 30-minute window.\nCurrent time is {{TIME}}.  Customize this file to control quick-win format.\n")
    ("memory" . "#+TITLE: My Profile\n#+STARTUP: showall\n\n* About Me\n\n* Current Focus Areas\n\n* Preferences\n\n* Boundaries\n")
    ("agent-notes" . "#+TITLE: Agent Notes\n#+STARTUP: showall\n\n* Observed Patterns\n\n* Work Rhythm\n\n* Recent Threads\n\n* Flags\n\n* Session Log\n"))
  "Minimal seed content for each file in the prompts directory.\nCreated on first run — customize via C-c l p.")

(defun my/llm--ensure-prompt-file (name)
  "Ensure prompt file NAME exists.  Creates from defaults if missing.\nReturns the file path."
  (let ((file (my/llm--prompt-file name)))
    (unless (file-exists-p file)
      (let ((dir (file-name-directory file)))
        (when dir (make-directory dir t)))
      (with-temp-file file
        (insert (or (alist-get name my/llm--prompt-defaults nil nil #'string=)
                    (format "#+TITLE: %s\n\n(Write your prompt here)\n" name))))
      (message "📝 Created prompt file: %s" file))
    file))

(defun my/llm--extract-prompt (name)
  "Read prompt NAME from its Org file and return the text.\nStrips Org headers (#+KEY: lines).  Returns a fallback string on failure."
  (let* ((file (my/llm--ensure-prompt-file name))
         (contents (my/llm--safe-file-contents file)))
    (if (and contents (> (length contents) 0))
        (string-trim
         (replace-regexp-in-string "^#\\+[A-Z_]+:.*\n?" "" contents))
      (format "(prompt '%s' is empty — edit it via C-c l p)" name))))

(defun my/llm--substitute-prompt (text &optional context)
  "Replace placeholders in TEXT with runtime values.\n{{DATE}}, {{TIME}}, and {{CONTEXT}} are supported.\nCONTEXT is an optional pre-built context string."
  (let ((result text))
    (setq result (replace-regexp-in-string
                  "{{DATE}}" (format-time-string "%A, %B %d, %Y") result t t))
    (setq result (replace-regexp-in-string
                  "{{TIME}}" (format-time-string "%H:%M") result t t))
    (when context
      (setq result (replace-regexp-in-string
                    "{{CONTEXT}}" context result t t)))
    result))

;;;###autoload
(defun my/llm-open-prompts ()
  "Open the prompts directory.  Creates default files if missing."
  (interactive)
  (let ((dir (my/llm--prompts-dir)))
    ;; Ensure all default prompt files exist
    (dolist (entry my/llm--prompt-defaults)
      (my/llm--ensure-prompt-file (car entry)))
    (dired dir)))

;; ============================================================================
;; PROMPT ASSEMBLY
;; ============================================================================

(defun my/llm--build-briefing-prompt ()
  "Build the full briefing prompt with greedy context filling.\nReturns a cons cell (SYSTEM-PROMPT . USER-PROMPT)."
  (let* ((context-sections (my/llm--build-context))
         (context-text
          (mapconcat
           (lambda (section)
             (format "[%s]\n%s" (car section) (cdr section)))
           context-sections
           "\n\n---\n\n"))
         (system  (my/llm--substitute-prompt
                   (my/llm--extract-prompt "system")))
         (briefing (my/llm--substitute-prompt
                    (my/llm--extract-prompt "briefing") context-text)))
    (cons system briefing)))

;; ============================================================================
;; BRIEFING BUFFER & MODE
;; ============================================================================

(defconst my/llm--briefing-buffer-name "*Daily Briefing*"
  "Name of the buffer used to display the daily briefing.")

(defvar briefing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'my/daily-briefing)
    map)
  "Keymap for `briefing-mode'.")

(define-derived-mode briefing-mode org-mode "Briefing"
  "Major mode for the Daily Briefing buffer.
\\{briefing-mode-map}"
  :group 'my/llm
  (setq buffer-read-only t)
  (setq-local header-line-format
              (propertize
               " 📈 Daily Briefing  |  q: quit  g: regen"
               'face '(:inherit mode-line :weight bold))))

(defun my/llm--render-briefing (response)
  "Render RESPONSE in the briefing buffer."
  (let ((buf (get-buffer-create my/llm--briefing-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert (format "#+TITLE: Daily Briefing — %s\n"
                        (format-time-string "%A, %B %d, %Y")))
        (insert (format "#+DATE: %s\n"
                        (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert (format "#+PROPERTY: Generated by %s via Ollama\n\n"
                        (symbol-name my/llm-model)))
        ;; LLM response
        (insert (or response "(No response received from LLM.)"))
        (insert "\n\n")
        ;; Footer
        (insert (format "-----\n/Generated at %s · Model: %s · Press 'g' to regenerate/\n"
                        (format-time-string "%H:%M:%S")
                        (symbol-name my/llm-model))))
      (briefing-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf '((display-buffer-reuse-window
                          display-buffer-same-window)))))

;; ============================================================================
;; MAIN COMMANDS
;; ============================================================================

;;;###autoload
(defun my/daily-briefing ()
  "Generate a Daily Briefing by analyzing Profile, Agent Notes, Agenda, and Journal.
Sends context to the local Ollama LLM and displays the result in a
dedicated Org-mode buffer.  The suggested schedule starts from the
current time, so this works well any time of day.

The briefing includes:
  - Top 3 must-do tasks (from agenda + journal continuity)
  - Suggested time blocks for the rest of the day
  - Context refresher from recent journal entries
  - Observations and pattern detection

After rendering, the LLM auto-updates its agent notes file with
observations from this session.

Requires Ollama running locally with the model specified in
`my/llm-model'."
  (interactive)
  ;; Guard: prevent concurrent briefings
  (when my/llm--briefing-in-progress
    (user-error "A briefing is already being generated — please wait"))
  ;; Guard: check Ollama is reachable
  (message "🔍 Checking Ollama connection at %s..." my/llm-ollama-host)
  (unless (my/llm--ollama-reachable-p)
    (user-error "Cannot reach Ollama at %s.  Is it running? (`ollama serve')"
                my/llm-ollama-host))
  ;; Build the prompt
  (let* ((prompt-pair (my/llm--build-briefing-prompt))
         (system-prompt (car prompt-pair))
         (user-prompt (cdr prompt-pair))
         (backend (my/llm--ensure-backend)))
    ;; Show a working buffer immediately
    (my/llm--render-briefing
     (propertize "⏳ Generating your daily briefing...\n\nAnalyzing profile, agent notes, agenda, and journal history.\nThis usually takes 15–45 seconds depending on context size.\n"
                 'face 'font-lock-comment-face))
    ;; Fire async request
    (setq my/llm--briefing-in-progress t)
    (message "🤖 Sending context to %s..." (symbol-name my/llm-model))
    (let ((gptel-backend backend)
          (gptel-model my/llm-model))
      (gptel-request user-prompt
        :system system-prompt
        :stream nil
        :buffer (get-buffer-create my/llm--briefing-buffer-name)
        :callback
        (lambda (response info)
          (setq my/llm--briefing-in-progress nil)
          (let ((text (my/llm--normalize-response response info)))
            (if (not text)
                (progn
                  (my/llm--render-briefing
                   (format "❌ Briefing generation failed.\n\nStatus: %s\n\nTroubleshooting:\n- Is Ollama running? (ollama serve)\n- Is the model pulled? (ollama pull %s)\n- Check *gptel-log* buffer for details."
                           (plist-get info :status)
                           (symbol-name my/llm-model)))
                  (message "❌ Briefing failed: %s" (plist-get info :status)))
              ;; Success!
              (setq my/llm--last-briefing-response text)
              (my/llm--render-briefing text)
              ;; Auto-update agent notes in the background
              (my/llm--auto-update-agent-notes text)
              (message "✅ Daily Briefing ready!"))))))))
;; ============================================================================
;; AGENT NOTES AUTO-UPDATE (fires after each briefing, no approval needed)
;; ============================================================================



(defun my/llm--sanitize-org-response (raw)
  "Sanitize RAW LLM response into clean Org-mode content.
Strips markdown code fences, conversational preamble, and trailing
chatter.  Returns the cleaned string, or nil if the result doesn't
look like valid Org-mode (missing #+TITLE:)."
  (when (stringp raw)
    (let ((text raw))
      ;; 1. Strip markdown code fences: ```org ... ``` or ``` ... ```
      (setq text (replace-regexp-in-string
                  "```[a-z-]*\\n?" "" text))
      ;; 2. Remove any preamble before #+TITLE:
      (when (string-match "\\(#\\+TITLE:.*\\)" text)
        (setq text (substring text (match-beginning 1))))
      ;; 3. Trim whitespace
      (setq text (string-trim text))
      ;; 4. Validate: must start with #+TITLE:
      (and (> (length text) 0)
           (string-prefix-p "#+TITLE:" text)
           text))))

(defun my/llm--auto-update-agent-notes (briefing-response)
  "Auto-update the agent notes file based on BRIEFING-RESPONSE.
Runs in the background after a successful briefing.  The response is
sanitized before writing; if sanitization fails the existing file is
preserved and a warning is logged."
  (let* ((notes (my/llm--extract-agent-notes))
         (update-instructions (my/llm--extract-prompt "agent-notes-update"))
         (prompt (format "%s\n\n=== YOUR CURRENT NOTES ===\n%s\n\n=== TODAY'S BRIEFING ===\n%s"
                         update-instructions notes briefing-response))
         (backend (my/llm--ensure-backend)))
    (let ((gptel-backend backend)
          (gptel-model my/llm-model))
      (gptel-request prompt
        :system "You are a concise analyst. Output only the requested Org-mode file content."
        :stream nil
        :callback
        (lambda (response info)
          (if (not response)
              (message "⚠️ Agent notes update failed: %s (briefing still valid)"
                       (plist-get info :status))
            (let* ((text (my/llm--normalize-response response info))
                   (file (my/llm--ensure-agent-notes-file))
                   (clean (my/llm--sanitize-org-response text)))
              (if clean
                  (progn
                    (with-temp-file file
                      (insert clean))
                    (message "🗒️ Agent notes updated"))
                (message "⚠️ Agent notes update rejected: LLM response failed sanitization (file preserved)")))))))))

;; ============================================================================
;; QUICK WIN — Single high-value task for a short window
;; ============================================================================

;;;###autoload
(defun my/llm-quick-win ()
  "Suggest a single high-value task for a short time window.
Useful when you have 30 minutes (e.g. baby is sleeping) and need
the agent to eliminate decision fatigue.  Uses agenda + recent
journal to pick the most effective micro-step."
  (interactive)
  (unless (my/llm--ollama-reachable-p)
    (user-error "Cannot reach Ollama at %s.  Is it running? (`ollama serve')"
                my/llm-ollama-host))
  (let* ((backend (my/llm--ensure-backend))
         (agenda  (my/llm--extract-agenda-today))
         (journal (my/llm--extract-journal 3000))
         (memory  (my/llm--extract-memory))
         (notes   (my/llm--extract-agent-notes))
         (context (format "[YOUR PROFILE]\n%s\n\n[AGENT OBSERVATIONS]\n%s\n\n[TODAY'S SCHEDULE]\n%s\n\n[RECENT JOURNAL]\n%s"
                          memory notes agenda journal))
         (prompt  (my/llm--substitute-prompt
                   (my/llm--extract-prompt "quick-win") context)))
    (message "🚀 Finding your quick win...")
    (let ((gptel-backend backend)
          (gptel-model my/llm-model))
      (gptel-request prompt
        :system (my/llm--substitute-prompt
                 (my/llm--extract-prompt "system"))
        :stream nil
        :callback
        (lambda (response info)
          (if (not response)
              (message "❌ Quick win failed: %s" (plist-get info :status))
            (let ((text (my/llm--normalize-response response info)))
              (with-current-buffer (get-buffer-create "*Quick Win*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (format "#+TITLE: 🚀 Quick Win — %s\n\n"
                                  (format-time-string "%H:%M")))
                  (insert text)
                  (insert "\n")
                  (org-mode)
                  (setq buffer-read-only t)
                  (goto-char (point-min)))
                (pop-to-buffer (current-buffer)))
              (message "🚀 Quick win ready!"))))))))
;; ============================================================================
;; GPTEL DEFAULT CONFIGURATION
;; ============================================================================

;; Eagerly configure the Ollama backend so that `C-c l c' (gptel chat)
;; picks up the correct backend immediately.  The `gptel' interactive
;; command reads (default-value 'gptel-backend) in its interactive form,
;; so we must set it before the first call — not in with-eval-after-load.
(my/llm--ensure-backend)  ; requires gptel internally
(setq gptel-backend my/llm--backend
      gptel-model my/llm-model)

;; Streaming, directives, and hooks — safe to defer until gptel is fully
;; loaded (the eager `require' above guarantees this runs immediately,
;; but with-eval-after-load makes intent clear and is idempotent).
(declare-function my/llm--goto-response-end "llm-assistant")
(with-eval-after-load 'gptel
  ;; Add our directive to gptel's directives list
  (add-to-list 'gptel-directives
               '(executive-assistant .
                 "You are my Personal Executive Assistant. You analyze my Org-mode task lists, calendar, and journal entries to help me plan effectively. Be concise and actionable. Output valid Org-mode markup.")
               t)

  ;; Enable streaming for interactive chat (tokens appear as they arrive)
  (setq gptel-stream t)

  ;; Discard reasoning/thinking blocks from deepseek-r1 and similar models.
  ;; This is the official gptel mechanism — our normalize-response function
  ;; acts as a safety net for custom callbacks only.
  (setq gptel-include-reasoning nil)

  ;; Auto-scroll to follow the response during streaming
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; After full response, move cursor to end so user can continue typing
  (defun my/llm--goto-response-end (_beg _end)
    "Move point past the LLM response to the next input position.\nArguments BEG and END are the response boundary positions (unused)."
    (when-let* ((win (get-buffer-window (current-buffer) 'visible)))
      (with-selected-window win
        (goto-char (point-max))
        (recenter -3))))

  (add-hook 'gptel-post-response-functions #'my/llm--goto-response-end 80))

;; ============================================================================
;; PHASE 2 STUBS (deferred — RAG via elisa)
;; ============================================================================

;; These are placeholders for Phase 2 features.  They are NOT active.

;; (defun my/llm-index-roam ()
;;   "Index org-roam directory for RAG semantic search via elisa."
;;   (interactive)
;;   (require 'elisa)
;;   (elisa-async-parse-directory "~/org/resources/roam/" "org-roam"))

;; (defun my/llm-refile-suggest ()
;;   "Suggest refile targets for inbox items using LLM.")

;; (defun my/llm-weekly-review ()
;;   "Generate a weekly review summary from journal + GTD data.")

;; (defun my/llm-ask-roam (query)
;;   "Ask a question answered by your org-roam knowledge base via RAG.")

(provide 'llm-assistant)
;;; llm-assistant.el ends here
