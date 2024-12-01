;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:50:43
;;; Time-stamp: <2024-12-02 06:50:43 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea.el


;;(require 'sea)

;; User installation paths
(defvar sea-user-root-dir (file-name-directory (directory-file-name
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))
  "User's SEA installation root directory.")

(defvar sea-user-source-dir (expand-file-name "src" sea-user-root-dir)
  "User's SEA source directory.")

(add-to-list 'load-path sea-user-root-dir)
(add-to-list 'load-path sea-user-source-dir)

;; Load base configuration first
(require 'sea-config)

;; Load components in order
(require 'sea-install)
(require 'sea-verify-installation)

;; Check installation and run setup if needed
(unless (sea-verify-installation)
  (message "SEA not properly installed. Running installation...")
  (sea-install))

;; Load remaining components after ensuring installation
(require 'sea-seed)
(require 'sea-utils)
(require 'sea-version-control)
(require 'sea-network)
(require 'sea-think)
(require 'sea-self-evolve)

(provide 'sea)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
