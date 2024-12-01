;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 21:33:01
;;; Time-stamp: <2024-12-01 21:33:01 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-config.el


(defgroup sea nil
  "Self-evolving agent configuration."
  :group 'applications)

(defcustom sea-workspace-dir "/opt/sea"
  "Base directory for SEA operations."
  :type 'directory
  :group 'sea)

(defcustom sea-config-dir (expand-file-name "~/.config/sea")
  "Directory for SEA configuration files."
  :type 'directory
  :group 'sea)

(defcustom sea-github-token-file (expand-file-name "github-token" sea-config-dir)
  "Path to file containing GitHub token.
Must be readable only by the owner (600 permissions)."
  :type 'string
  :group 'sea)

(defcustom sea-user-request-file (expand-file-name "user-request.md" sea-config-dir)
  "Path to file containing user's improvement requests."
  :type 'string
  :group 'sea)

(defcustom sea-suggestion-file (expand-file-name "sea-suggestion.md" sea-config-dir)
  "Path to file containing SEA's improvement suggestions."
  :type 'string
  :group 'sea)

(defun sea--ensure-config-files ()
  "Ensure SEA configuration files exist."
  (unless (file-exists-p sea-config-dir)
    (make-directory sea-config-dir t)
    (set-file-modes sea-config-dir #o700))

  (dolist (file (list sea-github-token-file
                     sea-user-request-file
                     sea-suggestion-file))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (cond
                ((equal file sea-user-request-file)
                 "# List improvement requests here\n")
                ((equal file sea-suggestion-file)
                 "# SEA improvement suggestions\n")
                (t ""))))
      (set-file-modes file #o600))))

(defcustom sea-history-file (expand-file-name "history.log" sea-workspace-dir)
  "File to store agent history."
  :type 'file
  :group 'sea)

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
