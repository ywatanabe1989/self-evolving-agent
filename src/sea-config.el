;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 22:55:15
;;; Time-stamp: <2024-12-01 22:55:15 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-config.el


(defgroup sea nil
  "Self-evolving agent configuration."
  :group 'applications)

;; Base directories
(defcustom sea-work-dir "~/.sea"
  "SEA working directory."
  :type 'directory
  :group 'sea)


;; Subdirectories
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

(defcustom sea-sandbox-mode t
  "When non-nil, run operations in isolated environment."
  :type 'boolean
  :group 'sea)

(defcustom sea-require-approval t
  "When non-nil, require user approval for critical operations."
  :type 'boolean
  :group 'sea)

(provide 'sea-config)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
