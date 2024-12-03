;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 11:45:07
;;; Time-stamp: <2024-12-02 11:45:07 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-config.el


;;; Code:

(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup sea nil
  "Self-evolving agent configuration."
  :group 'applications)

(defgroup self-evolving-agent nil
  "Self-evolving agent for Emacs."
  :group 'tools
  :prefix "sea-")

(defvar sea--sudo-password nil
  "Temporary storage for sudo password.")

(defvar sea--sudo-password nil
  "Store sudo password temporarily.")

;; Base directories
(defcustom sea-work-dir "~/.sea"
  "SEA working directory."
  :type 'directory
  :group 'sea)

;; Subdirectories
(defcustom sea-user "sea"
  "SEA system user name."
  :type 'string
  :group 'sea)

(defvar sea-uid
  (string-to-number (shell-command-to-string (format "id -u %s" sea-user)))
  "User ID of SEA system user.")

(defcustom sea-home (format "/home/%s" sea-user)
  "SEA user home directory."
  :type 'directory
  :group 'sea)

(defcustom sea-emacs-bin "/usr/bin/emacs"
  "Path to Emacs binary."
  :type 'file
  :group 'sea)

(defcustom sea-emacs-cli "/usr/bin/emacsclient"
  "Path to emacsclient binary."
  :type 'file
  :group 'sea)

;; (defcustom sea-emacs-srv
;;   (format "/tmp/emacs%d/server"
;;           (string-to-number
;;            (car (process-lines "id" "-u" sea-user))))
;;   "Path to Emacs server socket."
;;   :type 'file
;;   :group 'sea)

(defcustom sea-server-socket-dir (format "/tmp/emacs%d" sea-uid)
  "Directory for SEA Emacs server socket.")

(defcustom sea-server-socket-file (format "/tmp/emacs%d/server" sea-uid)
  "File path for SEA Emacs server socket.")


(defvar sea-workspace-dir (expand-file-name "workspace" sea-work-dir)
  "Directory for main SEA operations.")

(defvar sea-source-dir (expand-file-name "self-evolving-agent/src" sea-workspace-dir)
  "Directory for main SEA operations.")

(defvar sea-backups-dir (expand-file-name "backups" sea-work-dir)
  "Directory for backup files.")

(defvar sea-logs-dir (expand-file-name "logs" sea-work-dir)
  "Directory for log files.")

(defvar sea-requests-dir (expand-file-name "requests" sea-work-dir)
  "Directory for improvement requests.")

(defvar sea-config-dir (expand-file-name "config" sea-work-dir)
  "Directory for configuration files.")

;; Configuration files
(defcustom sea-github-token-file (expand-file-name "github-token" sea-config-dir)
  "Path to GitHub token file. Requires 600 permissions."
  :type 'file
  :group 'sea)

(defcustom sea-user-request-file (expand-file-name "user-request.md" sea-requests-dir)
  "File for user's improvement requests."
  :type 'file
  :group 'sea)

(defcustom sea-request-file (expand-file-name "sea-request.md" sea-requests-dir)
  "File for SEA's improvement suggestions."
  :type 'file
  :group 'sea)

(defcustom sea-history-file (expand-file-name "history.log" sea-logs-dir)
  "File to store agent history."
  :type 'file
  :group 'sea)

;; Operation modes
(defcustom sea-readonly-mode t
  "When non-nil, prevent modifications to core agent files."
  :type 'boolean
  :group 'sea)

(defcustom sea-require-approval t
  "When non-nil, require user approval for critical operations."
  :type 'boolean
  :group 'sea)

(defcustom sea-api-timeout 30
  "Timeout in seconds for API calls."
  :type 'integer
  :group 'self-evolving-agent)

(defcustom sea-readonly-mode t
  "When non-nil, prevent direct modifications to agent's core code."
  :type 'boolean
  :group 'self-evolving-agent)

(defvar sea-anthropic-key (getenv "ANTHROPIC_API_KEY")
  "API key for Anthropic Claude.")

(defvar sea-debug nil
  "Enable debug logging when non-nil.")

(defvar sea-backup-limit 10
  "Maximum number of backups to keep.")

(defvar sea-debug nil
  "Enable debug logging when non-nil.")

(provide 'sea-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
