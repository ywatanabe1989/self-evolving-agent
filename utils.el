;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:24:53
;;; Time-stamp: <2024-12-01 20:24:53 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/utils.el


;;; Commentary:
;; Utility functions for self-evolving agent

;;; Code:

(require 'sea-seed)

(defun sea--shell-command (command)
  "Execute shell COMMAND and return output."
  (with-temp-buffer
    (call-process-shell-command command nil t)
    (buffer-string)))

(defun sea--diff-files (file1 file2)
  "Get diff between FILE1 and FILE2."
  (sea--shell-command (format "diff -u %s %s" file1 file2)))

(defun sea--show-progress (message)
  "Show progress MESSAGE in dedicated window."
  (with-current-buffer (get-buffer-create "*sea-progress*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ")
            message "\n")
    (display-buffer (current-buffer))))

(defun sea--read-history ()
  "Read agent history from file."
  (when (file-exists-p sea-history-file)
    (with-temp-buffer
      (insert-file-contents sea-history-file)
      (buffer-string))))

(defun sea--create-backup (file)
  "Create backup of FILE with timestamp."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (backup-name (format "%s_%s" timestamp (file-name-nondirectory file)))
         (backup-path (expand-file-name backup-name
                                      (expand-file-name "backups" sea-workspace-dir))))
    (make-directory (file-name-directory backup-path) t)
    (copy-file file backup-path t)
    backup-path))

(defun sea--update-timestamp ()
  "Update timestamp in file header."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Time-stamp: <.*>" nil t)
      (replace-match (format "Time-stamp: <%s (ywatanabe)>"
                            (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun sea--log-change (file backup changes)
  "Log changes to FILE with BACKUP and CHANGES description."
  (with-temp-buffer
    (insert (format "\n=== %s ===\nFile: %s\nBackup: %s\nChanges:\n%s\n"
                   (format-time-string "%Y-%m-%d %H:%M:%S")
                   file backup changes))
    (append-to-file (point-min) (point-max) sea-history-file)))

(provide 'sea-utils)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
