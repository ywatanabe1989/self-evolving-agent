;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:25:28
;;; Time-stamp: <2024-12-01 20:25:28 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/version_control.el


;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:21:39
;;; Time-stamp: <2024-12-01 20:21:39 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/version_control.el

;;; Commentary:
;; Version control functionality for self-evolving agent

;;; Code:

(require 'sea-seed)
(require 'sea-utils)

(defvar sea-github-token
  (with-temp-buffer
    (insert-file-contents "~/.bash.d/d/secrets/access_tokens/github.txt")
    (string-trim (buffer-string)))
  "GitHub access token for authentication.")

(defun sea-self-evolve (file)
  "Update FILE with improvements suggested by LLM."
  (let ((backup (sea--create-backup file)))
    (sea-think
     (format "Review and improve %s" file))
    (sea--update-timestamp)
    (when (file-exists-p backup)
      (let ((changes (sea--diff-files backup file)))
        (sea--log-change file backup changes)))))

(defun sea-commit-changes (file msg)
  "Commit changes to FILE with commit MSG."
  (let ((default-directory (file-name-directory file)))
    (sea--shell-command
     (format "git add %s && git commit -m '%s'"
             (file-name-nondirectory file)
             msg))))

(defun sea-push-changes ()
  "Push changes to remote repository."
  (sea--shell-command "git push origin main"))

(defun sea-create-pr (title body)
  "Create pull request with TITLE and BODY."
  (let ((url "https://api.github.com/repos/owner/repo/pulls")
        (headers `(("Authorization" . ,(concat "token " sea-github-token))
                  ("Accept" . "application/vnd.github.v3+json")))
        (data (json-encode
               `((title . ,title)
                 (body . ,body)
                 (head . "feature-branch")
                 (base . "main")))))
    (request url
             :type "POST"
             :headers headers
             :data data
             :parser 'json-read)))

(provide 'sea-version-control)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
