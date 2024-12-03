;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 22:43:18
;;; Time-stamp: <2024-12-03 22:43:18 (ywatanabe)>
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
(require 'sea-logging)

;; Load components in order
(require 'sea-install)
(require 'sea-verify-installation)

;; Check installation and run setup if needed
(unless (sea-verify-installation)
  (message "SEA not properly installed. Running installation...")
  (sea-install))



;; Load remaining components after ensuring installation
(require 'sea-mode)
(require 'sea-utils)
(require 'sea-version-control)
(require 'sea-network)
(require 'sea-run)
(require 'sea-self-evolve)
(require 'sea-server)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))


(provide 'sea)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
