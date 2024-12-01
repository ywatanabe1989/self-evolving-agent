;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:39:54
;;; Time-stamp: <2024-12-01 20:39:54 (ywatanabe)>
;;; File: ./self-evolving-agent/config.el

(defgroup sea nil
  "Self-evolving agent configuration."
  :group 'applications)

(defcustom sea-workspace-dir "/opt/sea"
  "Base directory for SEA operations."
  :type 'directory
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
