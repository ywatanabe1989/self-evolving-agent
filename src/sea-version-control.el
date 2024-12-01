;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 21:17:15
;;; Time-stamp: <2024-12-01 21:17:15 (ywatanabe)>
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

(defun sea--load-github-token ()
  "Load GitHub token from file."
  (if (file-exists-p sea-github-token-file)
      (with-temp-buffer
        (insert-file-contents sea-github-token-file)
        (string-trim (buffer-string)))
    (error "GitHub token file not found: %s" sea-github-token-file)))

;; Add to initialization:
(setq sea-github-token (sea--load-github-token))


(defun sea-self-evolve (file)
  "Update FILE with improvements suggested by LLM."
  (unless sea-github-token
    (error "GitHub token not set. Please set sea-github-token"))

  ;; Work with copy in sea workspace
  (let* ((workspace-dir "/opt/sea/workspace/self-evolving-agent")
         (relative-path (file-relative-name file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent"))
         (work-file (expand-file-name relative-path workspace-dir))
         (backup-file (sea--create-backup work-file)))

    ;; Ensure workspace exists
    (make-directory (file-name-directory work-file) t)

    ;; Copy original to workspace
    (copy-file file work-file t)

    ;; Generate improvements on work file
    (sea-think
     (format "Review and improve %s" work-file))

    ;; Update timestamp in work file
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
  (unless sea-github-token
    (error "GitHub token not set. Please set sea-github-token"))
  (let ((url "https://api.github.com/repos/owner/repo/pulls")
        (headers `(("Authorization" . ,(concat "token " sea-github-token))
                  ("Accept" . "application/vnd.github.v3+json")))
        (data (json-encode
               `((title . ,title)
                 (body . ,body)
                 (head . "sea-develop")
                 (base . "main")))))
    (request url
             :type "POST"
             :headers headers
             :data data
             :parser 'json-read)))

(provide 'sea-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
