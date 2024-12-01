;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:48:40
;;; Time-stamp: <2024-12-01 20:48:40 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea.el


(add-to-list 'load-path
             (expand-file-name
              (concat (file-name-directory (or load-file-name buffer-file-name))
                     "src")))

;; Load components in dependency order
(require 'sea-config)
(require 'sea-seed)
(require 'sea-utils)
(require 'sea-version-control)
(require 'sea-network)
(require 'sea-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))

(provide 'sea)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
