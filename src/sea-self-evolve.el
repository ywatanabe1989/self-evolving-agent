;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 23:16:33
;;; Time-stamp: <2024-12-01 23:16:33 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-self-evolve.el


(require 'sea-seed)
(require 'sea-config)
(require 'sea-utils)
(require 'sea-core)


(defun sea-self-evolve (&optional file)
  "Update FILE with improvements suggested by LLM.
If FILE is nil, use sea source directory."
  (interactive)
  (message "Starting self-evolution...")
  (sea--init-workspace)
  (let* ((github-token (sea--get-github-token))
         (file (or file (expand-file-name "sea.el" sea-source-dir)))
         (request-file sea-user-request-file)
         (aspects (if (file-exists-p request-file)
                     (with-temp-buffer
                       (insert-file-contents request-file)
                       (buffer-string))
                   (read-string "Aspects to improve (empty for general review): ")))
         (workspace-dir sea-workspace-dir)
         (sea-load-path (file-name-directory (locate-library "sea"))))

    (unless github-token
      (error "GitHub token not available. Check %s" sea-github-token-file))

    (unless (file-exists-p file)
      (error "Source file not found: %s" file))

    (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
           (backup-file (sea--create-backup work-file)))

      (copy-file file work-file t)

      (async-start
       `(lambda ()
          (add-to-list 'load-path ,sea-load-path)
          (require 'sea)
          (let ((default-directory ,default-directory))
            (sea-think
             (format "Review and improve %s\nFocus on these aspects:\n%s"
                     ,work-file
                     ,(if (string-empty-p aspects)
                          "General code review and improvements"
                        aspects)))))
       `(lambda (_)
          (with-current-buffer (find-file-noselect ,work-file)
            (sea--update-timestamp)
            (save-buffer))

          (when (file-exists-p ,backup-file)
            (let ((changes (sea--diff-files ,backup-file ,work-file)))
              (sea--log-change ,work-file ,backup-file changes)))

          (message "Self-evolution completed"))))))

;; (defun sea-self-evolve (&optional file)
;;   "Update FILE with improvements suggested by LLM.
;; If FILE is nil, use sea source directory."
;;   (interactive)
;;   (sea--init-workspace)
;;   (let* ((github-token (sea--get-github-token))
;;          (file (or file (expand-file-name "sea.el" sea-source-dir)))
;;          (request-file sea-user-request-file)
;;          (aspects (if (file-exists-p request-file)
;;                      (with-temp-buffer
;;                        (insert-file-contents request-file)
;;                        (buffer-string))
;;                    (read-string "Aspects to improve (empty for general review): ")))
;;          (workspace-dir sea-workspace-dir))

;;     (unless github-token
;;       (error "GitHub token not available. Check %s" sea-github-token-file))

;;     (unless (file-exists-p file)
;;       (error "Source file not found: %s" file))

;;     (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
;;            (backup-file (sea--create-backup work-file)))

;;       (copy-file file work-file t)

;;       (sea-think
;;        (format "Review and improve %s\nFocus on these aspects:\n%s"
;;                work-file
;;                (if (string-empty-p aspects)
;;                    "General code review and improvements"
;;                  aspects)))

;;       (with-current-buffer (find-file-noselect work-file)
;;         (sea--update-timestamp)
;;         (save-buffer))

;;       (when (file-exists-p backup-file)
;;         (let ((changes (sea--diff-files backup-file work-file)))
;;           (sea--log-change work-file backup-file changes))))))
; (sea-self-evolve)


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
