;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 23:09:44
;;; Time-stamp: <2024-12-01 23:09:44 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea.el


;; User installation paths
(defvar sea-user-root-dir (file-name-directory (directory-file-name
                                              (file-name-directory
                                               (or load-file-name buffer-file-name))))
  "User's SEA installation root directory.")

(defvar sea-user-source-dir (expand-file-name "src" sea-user-root-dir)
  "User's SEA source directory.")

(add-to-list 'load-path sea-user-root-dir)
(add-to-list 'load-path sea-user-source-dir)

;; Then load components in order
(require 'sea-config)
(require 'sea-seed)
(require 'sea-utils)
(require 'sea-version-control)
(require 'sea-network)
(require 'sea-core)
(require 'sea-self-evolve)

(provide 'sea)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
