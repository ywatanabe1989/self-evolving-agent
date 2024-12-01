;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 22:30:32
;;; Time-stamp: <2024-12-01 22:30:33 (ywatanabe)>
;;; File: ./self-evolving-agent/.old/core.el


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


(defvar sea-entry-file (or load-file-name buffer-file-name)
  "The main entry point file for self-evolving agent.")

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


(defcustom sea-git-repo-path "~/.dotfiles"
  "Path to git repository containing the agent code."
  :type 'directory
  :group 'self-evolving-agent)

(defun sea--git-commit (file message)
  "Commit changes in FILE with MESSAGE to git."
  (let ((default-directory sea-git-repo-path))
    (call-process "git" nil nil nil "add" file)
    (call-process "git" nil nil nil "commit" "-m" message)))

(defun sea--git-push ()
  "Push changes to remote repository."
  (let ((default-directory sea-git-repo-path))
    (call-process "git" nil nil nil "push")))


(defun sea-think (input)
  "Main entry point for agent thinking process.
INPUT is the task description or command for the agent."
  (interactive "sTask: ")
  (unless sea-anthropic-key
    (user-error "ANTHROPIC_API_KEY not set"))

  (if (string-match-p "Update.*self" input)
      (sea-self-evolve sea-entry-file)
    (condition-case err
        (let* ((context (sea--get-context))
               (prompt (sea--build-prompt input context))
               (commands (sea--run-llm prompt)))
          (when commands
            (sea--execute-commands commands)))
      (error (message "Error in sea-think: %S" err)))))

(defun sea-self-evolve (&optional file)
  "Evolve agent code in FILE with backup and logging.
If FILE is nil, use sea-entry-file or current buffer's file."
  (interactive)
  (let* ((target-file (or file
                         sea-entry-file
                         (buffer-file-name)
                         (user-error "No file specified and not in a file buffer")))
         (backup-file (sea--create-backup target-file))
         (code (with-temp-buffer
                (insert-file-contents target-file)
                (buffer-string)))
         (prompt (format "Current code:\n%s\nSuggest improvements:" code))
         (improved-code (sea--run-llm prompt)))
    (when improved-code
      (with-temp-file target-file
        (insert improved-code))
      (sea--log-change target-file backup-file "Self-evolution update")
      (sea--git-commit target-file "Self-evolution update")
      (sea--git-push))))

(defalias 'sse 'sea-self-evolve)


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

(defun sea-self-evolve (file)
  "Evolve agent code in FILE."
  (let* ((code (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (prompt (format "Current code:\n%s\nSuggest improvements:" code))
         (improved-code (sea--run-llm prompt)))
    (when improved-code
      (with-temp-file file
        (insert improved-code)))))


(defcustom sea-backup-dir (expand-file-name "backups" sea-workspace-dir)
  "Directory for storing backups."
  :type 'directory
  :group 'self-evolving-agent)

(defun sea--ensure-dirs ()
  "Ensure required directories exist."
  (dolist (dir (list sea-workspace-dir sea-backup-dir))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun sea--create-backup (file)
  "Create a backup of FILE with timestamp."
  (sea--ensure-dirs)
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (basename (file-name-nondirectory file))
         (backup-file (expand-file-name (format "%s_%s" timestamp basename) sea-backup-dir)))
    (copy-file file backup-file t)
    backup-file))

(defun sea--log-change (original-file backup-file changes)
  "Log changes between ORIGINAL-FILE and BACKUP-FILE with CHANGES description."
  (with-temp-buffer
    (insert (format "\n=== %s ===\nBackup: %s\nChanges:\n%s\n"
                   (format-time-string "%Y-%m-%d %H:%M:%S")
                   backup-file
                   changes))
    (append-to-file (point-min) (point-max) sea-history-file)))

(provide 'self-evolving-agent)
;;; self-evolving-agent.el ends here


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
