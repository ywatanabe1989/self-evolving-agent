;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 21:25:39
;;; Time-stamp: <2024-12-01 21:25:39 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-seed.el


;;; Commentary:
;; Core configuration and dependencies for self-evolving agent

;;; Code:
(require 'sea-config)
(require 'json)
(require 'request)
(require 'w3m nil t)

(defgroup self-evolving-agent nil
  "Self-evolving agent for Emacs."
  :group 'tools
  :prefix "sea-")

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

(provide 'sea-seed)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
