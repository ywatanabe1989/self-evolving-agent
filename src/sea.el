;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 21:29:13
;;; Time-stamp: <2024-12-01 21:29:13 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea.el


(defvar sea-root-dir (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of SEA installation.")

(add-to-list 'load-path sea-root-dir)
(add-to-list 'load-path (expand-file-name "src" sea-root-dir))

;; (add-to-list 'load-path
;;              (expand-file-name
;;               (concat (file-name-directory (or load-file-name buffer-file-name))
;;                      "src")))

;; Then load components in order
(require 'sea-config)  ; First load configurations
(require 'sea-seed)    ; Then basic requirements
(require 'sea-utils)
(require 'sea-version-control)
(require 'sea-network)
(require 'sea-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

(provide 'sea)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
