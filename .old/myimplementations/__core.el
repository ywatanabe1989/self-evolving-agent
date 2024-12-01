;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:21:20
;;; Time-stamp: <2024-12-01 20:21:20 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/__core.el


;;; self-evolving-agent/core.el --- Self-evolving agent for Emacs -*- lexical-binding: t -*-

;; Author: Unknown
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (request "0.3.3"))
;; Keywords: ai, tools
;; URL: https://github.com/yourusername/self-evolving-agent

;;; Commentary:

;; A self-evolving agent that can modify its own code and perform tasks
;; using the Anthropic Claude API.  Main entry point is `sea-think'.


;;; Code:

(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup self-evolving-agent nil
  "Self-evolving agent for Emacs."
  :group 'tools
  :prefix "sea-")

(defcustom sea-workspace-dir (expand-file-name "~/sea-workspace")
  "Directory for agent workspace."
  :type 'directory
  :group 'self-evolving-agent)

(defcustom sea-history-file (expand-file-name "history.log" sea-workspace-dir)
  "File to store agent history."
  :type 'file
  :group 'self-evolving-agent)

(defcustom sea-api-timeout 30
  "Timeout in seconds for API calls."
  :type 'integer
  :group 'self-evolving-agent)

(defvar sea-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar sea-context nil
  "Current context of the agent.")

(defvar sea-debug nil
  "Enable debug logging when non-nil.")


(defun sea-think (input)
  "Main entry point for agent thinking process.
INPUT is the task description or command for the agent."
  (interactive "sTask: ")
  (unless sea-anthropic-key
    (user-error "ANTHROPIC_API_KEY not set"))

  (if (string-match-p "Update.*self" input)
      (sea-self-evolve
       (expand-file-name "~/.dotfiles/.emacs.d/lisp/self-evolving-agent/core.el"))
    (condition-case err
        (let* ((context (sea--get-context))
               (prompt (sea--build-prompt input context))
               (commands (sea--run-llm prompt)))
          (when commands
            (sea--execute-commands commands)))
      (error (message "Error in sea-think: %S" err)))))

(defun sea--get-context ()
  "Gather current context including buffer state and history."
  (setq sea-context
        `((buffer . ,(buffer-string))
          (mode . ,major-mode)
          (url . ,(when (bound-and-true-p w3m-current-url)
                   w3m-current-url))
          (history . ,(sea--read-history)))))

(defun sea--build-prompt (input context)
  "Build prompt from INPUT and CONTEXT."
  (format "Context: %s\nTask: %s\nThink and respond with elisp commands:"
          (prin1-to-string context)
          input))


(defun sea--run-llm (prompt)
  "Send PROMPT to LLM and get response."
  (let* ((url "https://api.anthropic.com/v1/messages")
         (headers `(("content-type" . "application/json")
                   ("x-api-key" . ,sea-anthropic-key)
                   ("anthropic-version" . "2023-06-01")))
         (data (json-encode
                `((model . "claude-3-5-sonnet-20241022")
                  (max_tokens . 1024)
                  (messages . [((role . "user")
                              (content . ,prompt))]))))
         (response (request
                    url
                    :type "POST"
                    :headers headers
                    :data data
                    :parser 'json-read
                    :sync t)))
    (when-let* ((data (request-response-data response))
                (content (alist-get 'content data))
                (first-msg (aref content 0)))
      (alist-get 'text first-msg))))


(defun sea--execute-commands (commands)
  "Execute COMMANDS returned by LLM."
  (condition-case err
      (eval (read commands))
    (error
     (message "Error executing commands: %S" err))))

(defun sea--read-history ()
  "Read agent history from file."
  (when (file-exists-p sea-history-file)
    (with-temp-buffer
      (insert-file-contents sea-history-file)
      (buffer-string))))



(defcustom sea-readonly-mode t
  "When non-nil, prevent direct modifications to agent's core code."
  :type 'boolean
  :group 'self-evolving-agent)

(defvar sea-backup-limit 10
  "Maximum number of backups to keep.")

(defun sea--safety-check ()
  "Perform safety checks before self-evolution."
  (and sea-readonly-mode
       (y-or-n-p "Agent is in readonly mode. Allow modification? ")
       (progn
         (setq sea-readonly-mode nil)
         t)))

(defun sea-self-evolve (&optional file)
  "Evolve agent code in FILE with backup and logging."
  (interactive)
  (when (sea--safety-check)
    (let* ((target-file (or file
                           sea-entry-file
                           (buffer-file-name)
                           (user-error "No file specified")))
           (backup-file (sea--create-backup target-file))
           (code (with-temp-buffer
                  (insert-file-contents target-file)
                  (sea--update-timestamp)
                  (buffer-string)))
           (prompt (format "Current code:\n%s\nSuggest improvements:" code))
           (improved-code (sea--run-llm prompt)))
      (when improved-code
        (sea--show-progress "Validating changes...")
        (condition-case err
            (progn
              (with-temp-buffer
                (insert improved-code)
                (emacs-lisp-mode)
                (check-parens))
              (with-temp-file target-file
                (insert improved-code))
              (sea--show-progress "Code updated")
              (sea--show-progress (sea--diff-files backup-file target-file))
              (sea--log-change target-file backup-file "Self-evolution update")
              (sea--git-commit target-file "Self-evolution update")
              (sea--git-push))
          (error
           (sea--show-progress (format "Error: %s" err))
           (copy-file backup-file target-file t)))
        (setq sea-readonly-mode t)))))


;; Network/Server functionality
(defvar sea-server-port 8080
  "Port for agent server.")

(defvar sea-agents nil
  "List of active agents.")

(defun sea-start-server ()
  "Start agent server."
  (interactive)
  (make-network-process
   :name "sea-server"
   :buffer "*sea-server*"
   :service sea-server-port
   :family 'ipv4
   :server t
   :filter 'sea--server-filter))

;; Shell and text processing
(defun sea--shell-command (command)
  "Execute shell COMMAND and return output."
  (with-temp-buffer
    (call-process-shell-command command nil t)
    (buffer-string)))

(defun sea--diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (sea--shell-command (format "diff -u %s %s" file1 file2)))

;; Progress visualization
(defun sea--show-progress (message)
  "Show progress MESSAGE in dedicated window."
  (with-current-buffer (get-buffer-create "*sea-progress*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ")
            message "\n")
    (display-buffer (current-buffer))))

;; Multi-agent coordination
(cl-defstruct sea-agent
  id task status)

(defun sea-spawn-agents (tasks)
  "Spawn multiple agents for TASKS."
  (dolist (task tasks)
    (push (make-sea-agent :id (cl-gensym)
                         :task task
                         :status 'pending)
          sea-agents))
  (sea--coordinate-agents))

(defun sea--coordinate-agents ()
  "Coordinate multiple agents' activities."
  (while sea-agents
    (let ((agent (pop sea-agents)))
      (sea--show-progress
       (format "Agent %s processing: %s"
               (sea-agent-id agent)
               (sea-agent-task agent)))
      (sea-think (sea-agent-task agent)))))



(defun sea--create-backup (file)
  "Create backup of FILE with timestamp."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-name (format "%s_%s" timestamp (file-name-nondirectory file)))
         (backup-path (expand-file-name backup-name
                                      (expand-file-name "backups" sea-workspace-dir))))
    (make-directory (file-name-directory backup-path) t)
    (copy-file file backup-path t)
    backup-path))

(defun sea--update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Time-stamp: <.*>" nil t)
      (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                            (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun sea--log-change (file backup changes)
  "Log changes to FILE with BACKUP and CHANGES description."
  (with-temp-buffer
    (insert (format "\n=== %s ===\nFile: %s\nBackup: %s\nChanges:\n%s\n"
                   (format-time-string "%Y-%m-%d %H:%M:%S")
                   file backup changes))
    (append-to-file (point-min) (point-max) sea-history-file)))

(defun sea--git-commit (file msg)
  "Commit FILE with MSG to git."
  (let ((default-directory (file-name-directory file)))
    (sea--shell-command (format "git add %s && git commit -m '%s'"
                               (file-name-nondirectory file) msg))))

(defun sea--git-push ()
  "Push changes to remote repository."
  (sea--shell-command "git push"))

(defvar sea--server-filter nil
  "Network process filter for sea server.")





(defun sea--server-filter (proc string)
  "Filter function for server process PROC with STRING input."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (when (string-match "\n" string)
      (let ((command (buffer-substring (point-min) (point-max))))
        (erase-buffer)
        (sea-think command)))))


(defvar sea-github-token
  (with-temp-buffer
    (insert-file-contents "~/.bash.d/d/secrets/access_tokens/github.txt")
    (string-trim (buffer-string)))
  "GitHub access token for authentication.")

(defun sea--git-push ()
  "Push changes to remote repository."
  (let ((command (format "git -c credential.helper='!f() { echo username=ywatanabe1989; echo password=%s; }; f' push"
                        sea-github-token)))
    (sea--shell-command command)))


(defun sea--validate-git-status ()
  "Ensure working directory is clean before modifications."
  (string-empty-p (string-trim (sea--shell-command "git status --porcelain"))))

(defun sea--safety-check ()
  "Perform safety checks before self-evolution."
  (and sea-readonly-mode
       (y-or-n-p "Agent is in readonly mode. Allow modification? ")
       (sea--validate-git-status)
       (progn
         (setq sea-readonly-mode nil)
         t)))

(provide 'self-evolving-agent)
;;; self-evolving-agent.el ends here

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
