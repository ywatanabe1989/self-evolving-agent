;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 23:02:58
;;; Time-stamp: <2024-12-01 23:02:58 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-version-control.el


;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'sea-seed)
(require 'sea-utils)

(defgroup sea-git nil
  "Git configuration for Self-Evolving Agent."
  :group 'sea)

(defcustom sea-git-user-name "sea-bot"
  "Git user name for SEA commits."
  :type 'string
  :group 'sea-git)

(defcustom sea-git-user-email "sea-bot@example.com"
  "Git email for SEA commits."
  :type 'string
  :group 'sea-git)

(defcustom sea-github-token-file "~/.config/sea/github-token"
  "Path to file containing GitHub token."
  :type 'string
  :group 'sea-git)

(defvar sea--github-token-cache nil
  "Cached GitHub token to avoid frequent file reads.")

(defun sea--validate-github-token (token)
  "Validate TOKEN format and basic structure."
  (when (or (null token)
            (not (stringp token))
            (string-empty-p token)
            (< (length token) 40))
    (error "Invalid GitHub token format")))

(defun sea--load-github-token ()
  "Load GitHub token from file with validation and error handling."
  (condition-case err
      (let* ((token-file (expand-file-name sea-github-token-file))
             (real-file (file-truename token-file)))
        (unless (file-exists-p real-file)
          (error "GitHub token file not found: %s" real-file))
        (unless (file-readable-p real-file)
          (error "GitHub token file not readable: %s" real-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents real-file)
                      (string-trim (buffer-string)))))
          (sea--validate-github-token token)
          token))
    (error
     (message "Failed to load GitHub token: %s" (error-message-string err))
     nil)))

(defun sea--get-github-token ()
  "Get GitHub token, using cache if available."
  (or sea--github-token-cache
      (setq sea--github-token-cache (sea--load-github-token))))

;; Replace existing initialization with:
(sea--get-github-token)

(defun sea--init-workspace ()
  "Initialize SEA workspace with symbolic links."
  (interactive)
  (let* ((user-name (user-login-name))
         (source-dir (directory-file-name sea-user-root-dir))
         (workspace-dir (directory-file-name sea-workspace-dir))
         (target-link (expand-file-name "self-evolving-agent" workspace-dir)))

    ;; Verify user is in sea group
    (unless (member "sea" (split-string (shell-command-to-string
                                       (format "groups %s" user-name))))
      (error "Current user must be in 'sea' group. Run install.sh first"))

    ;; Create base directories
    (dolist (dir (list sea-work-dir
                      sea-workspace-dir
                      sea-backups-dir
                      sea-logs-dir
                      sea-requests-dir
                      sea-config-dir))
      (unless (file-exists-p dir)
        (make-directory dir t)
        (set-file-modes dir #o700)))

    ;; Create symbolic link
    (when (file-exists-p target-link)
      (delete-file target-link))
    (make-symbolic-link source-dir target-link)))

;; ;; Call this before sea-self-evolve
;; (sea--init-workspace)
;; ;; rm ~/.sea/workspace/ -rf
;; ;; tree ~/.sea


(defun sea-self-evolve (&optional file)
  "Update FILE with improvements suggested by LLM.
If FILE is nil, use sea source directory."
  (interactive)
  (sea--init-workspace)
  (let* ((github-token (sea--get-github-token))
         (file (or file (expand-file-name "sea.el" sea-source-dir)))
         (request-file sea-user-request-file)
         (aspects (if (file-exists-p request-file)
                     (with-temp-buffer
                       (insert-file-contents request-file)
                       (buffer-string))
                   (read-string "Aspects to improve (empty for general review): ")))
         (workspace-dir sea-workspace-dir))

    (unless github-token
      (error "GitHub token not available. Check %s" sea-github-token-file))

    (unless (file-exists-p file)
      (error "Source file not found: %s" file))

    (let* ((work-file (expand-file-name (file-name-nondirectory file) workspace-dir))
           (backup-file (sea--create-backup work-file)))

      (copy-file file work-file t)

      (sea-think
       (format "Review and improve %s\nFocus on these aspects:\n%s"
               work-file
               (if (string-empty-p aspects)
                   "General code review and improvements"
                 aspects)))

      (with-current-buffer (find-file-noselect work-file)
        (sea--update-timestamp)
        (save-buffer))

      (when (file-exists-p backup-file)
        (let ((changes (sea--diff-files backup-file work-file)))
          (sea--log-change work-file backup-file changes))))))
; (sea-self-evolve)

(defun sea--ensure-not-main ()
  "Ensure we're not on main branch."
  (let ((current-branch
         (string-trim
          (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
    (when (string= current-branch "main")
      (error "Cannot modify main branch directly"))))

(defun sea-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (sea--ensure-not-main)
  (let ((default-directory (file-name-directory file)))
    (sea--shell-command
     (format "git -c user.name='%s' -c user.email='%s' add %s && git -c user.name='%s' -c user.email='%s' commit -m '%s'"
             sea-git-user-name
             sea-git-user-email
             (file-name-nondirectory file)
             sea-git-user-name
             sea-git-user-email
             msg))))

(defun sea-push-changes ()
  "Push changes to sea-develop branch."
  (sea--ensure-not-main)
  (sea--shell-command
   (concat
    "git checkout -b sea-develop 2>/dev/null || git checkout sea-develop && "
    "git push -u origin sea-develop")))


(defun sea-create-pr (title body)
  "Create pull request with TITLE and BODY from sea-develop to main."
  (let ((token (sea--get-github-token)))
    (unless token
      (error "GitHub token not available"))
    (let ((url "https://api.github.com/repos/owner/repo/pulls")
          (headers `(("Authorization" . ,(concat "token " token))
                    ("Accept" . "application/vnd.github.v3+json")))
          (data (json-encode
                 `((title . ,title)
                   (body . ,body)
                   (head . "sea-develop")
                   (base . "main")))))
      (condition-case err
          (request url
                   :type "POST"
                   :headers headers
                   :data data
                   :parser 'json-read
                   :error (lambda (&rest args)
                           (error "PR creation failed: %S" args)))
        (error
         (setq sea--github-token-cache nil)
         (error "Failed to create PR: %s" (error-message-string err)))))))

(provide 'sea-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
