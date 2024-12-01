;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 21:21:16
;;; Time-stamp: <2024-12-01 21:21:16 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-version-control.el


;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:21:39
;;; Time-stamp: <2024-12-01 20:21:39 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/version_control.el

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

(defvar sea-github-token nil
  "GitHub access token loaded from sea-github-token-file.")

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
      (let ((token-file (expand-file-name sea-github-token-file)))
        (unless (file-exists-p token-file)
          (error "GitHub token file not found: %s" token-file))
        (unless (file-readable-p token-file)
          (error "GitHub token file not readable: %s" token-file))
        (let ((token (with-temp-buffer
                      (insert-file-contents token-file)
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


(defun sea-self-evolve (&optional file)
  "Update FILE with improvements suggested by LLM.
If FILE is nil, use sea source directory.
Improvement aspects are read from user_request file or prompted."
  (interactive)
  (unless sea-github-token
    (error "GitHub token not set. Please set sea-github-token"))

  (let* ((file (or file (expand-file-name "src" sea-workspace-dir)))
         (request-file (expand-file-name "user_request" sea-workspace-dir))
         (aspects (if (file-exists-p request-file)
                     (with-temp-buffer
                       (insert-file-contents request-file)
                       (buffer-string))
                   (read-string "Aspects to improve (empty for general review): ")))
         (workspace-dir "/opt/sea/workspace/self-evolving-agent")
         (relative-path (file-relative-name file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent"))
         (work-file (expand-file-name relative-path workspace-dir))
         (backup-file (sea--create-backup work-file)))

    (make-directory (file-name-directory work-file) t)
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
        (sea--log-change work-file backup-file changes)))))

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
