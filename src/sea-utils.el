;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-03 21:19:40
;;; Time-stamp: <2024-12-03 21:19:40 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-utils.el


;;; Commentary:
;; Utility functions for self-evolving agent

;;; Code:
(defun sea--sudo ()
  "Get sudo password once and store it."
  (interactive)
  (unless sea--sudo-password
    (setq sea--sudo-password (read-passwd "Sudo password: ")))
  sea--sudo-password)

(defun sea--shell-command (command)
  "Execute shell COMMAND and return output or nil on error."
  (condition-case err
      (with-temp-buffer
        (let ((exit-code (call-process-shell-command command nil t)))
          (if (zerop exit-code)
              (buffer-string)
            (error "Command failed with exit code %d: %s" exit-code command))))
    (error (message "Shell command error: %s" err) nil)))

(defun sea--diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (or (sea--shell-command (format "diff -u %s %s" file1 file2))
      "No differences found"))

(defun sea--show-progress (message)
  "Show progress MESSAGE in dedicated window."
  (let ((buffer (get-buffer-create "*sea-progress*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (format "[%s] %s\n"
                       (format-time-string "%H:%M:%S")
                       message)))
      (display-buffer buffer))))



;; (defun sea--ensure-config-files ()
;;   "Ensure SEA configuration files and directories exist."
;;   (let ((dirs (list sea-work-dir sea-workspace-dir sea-backups-dir
;;                     sea-logs-dir sea-requests-dir sea-config-dir
;;                     sea-sandbox-dir))
;;         (files (list sea-github-token-file
;;                     sea-user-request-file
;;                     sea-request-file)))

;;     (dolist (dir dirs)
;;       (unless (file-exists-p dir)
;;         (condition-case err
;;             (progn
;;               (make-directory dir t)
;;               (set-file-modes dir #o700))
;;           (error (message "Failed to create directory %s: %s" dir err)))))

;;     (dolist (file files)
;;       (unless (file-exists-p file)
;;         (condition-case err
;;             (progn
;;               (with-temp-file file
;;                 (insert (cond
;;                         ((equal file sea-user-request-file)
;;                          "# List improvement requests here\n")
;;                         ((equal file sea-request-file)
;;                          "# SEA improvement suggestions\n")
;;                         (t ""))))
;;               (set-file-modes file #o600))
;;           (error (message "Failed to create file %s: %s" file err)))))))


(defun sea--create-backup (file)
  "Create backup of FILE with timestamp."
  (when (and file (file-exists-p file))
    (let* ((base (file-name-sans-extension file))
           (ext (file-name-extension file))
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (backup-name (format "%s-%s.%s"
                              (file-name-nondirectory base)
                              timestamp
                              ext))
           (backup-path (expand-file-name backup-name sea-backups-dir)))
      (condition-case err
          (progn
            (make-directory sea-backups-dir t)
            (copy-file file backup-path t)
            backup-path)
        (error (message "Backup failed for %s: %s" file err) nil)))))

;; (defun sea--create-backup (file)
;;   "Create backup of FILE with timestamp."
;;   (when (and file (file-exists-p file))
;;     (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
;;            (backup-name (format "%s_%s" timestamp (file-name-nondirectory file)))
;;            (backup-path (expand-file-name backup-name sea-backups-dir)))
;;       (condition-case err
;;           (progn
;;             (make-directory sea-backups-dir t)
;;             (copy-file file backup-path t)
;;             backup-path)
;;         (error (message "Backup failed for %s: %s" file err) nil)))))

(defun sea--update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Time-stamp: <.*>" nil t)
      (let ((new-timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
        (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                             new-timestamp))))))

(provide 'sea-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
