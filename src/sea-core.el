;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:43:24
;;; Time-stamp: <2024-12-01 20:43:24 (ywatanabe)>
;;; File: ./self-evolving-agent/core.el


;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-01 20:21:27
;;; Time-stamp: <2024-12-01 20:21:27 (ywatanabe)>
;;; File: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/core.el

;;; Commentary:
;; Core functionality for self-evolving agent

;;; Code:
(require 'sea-config)
(require 'sea-seed)
(require 'sea-utils)
(require 'sea-version-control)

(defvar sea-context nil
  "Current context of the agent.")

(defun sea-think (input)
  "Main entry point for agent thinking process.
INPUT is the task description or command for the agent."
  (interactive "sTask: ")
  (unless sea-anthropic-key
    (user-error "ANTHROPIC_API_KEY not set"))

  (if (string-match-p "Update.*self" input)
      (sea-self-evolve
       (expand-file-name "~/.dotfiles/.emacs.d/lisp/self-evolving-agent/core.el"))
    (condition-case err
        (let* ((context (sea--get-context))
               (prompt (sea--build-prompt input context))
               (commands (sea--run-llm prompt)))
          (when commands
            (sea--execute-commands commands)))
      (error (message "Error in sea-think: %S" err)))))

(defun sea--get-context ()
  "Gather current context including buffer state and history."
  (setq sea-context
        `((buffer . ,(buffer-string))
          (mode . ,major-mode)
          (url . ,(when (bound-and-true-p w3m-current-url)
                   w3m-current-url))
          (history . ,(sea--read-history)))))

(defun sea--build-prompt (input context)
  "Build prompt from INPUT and CONTEXT."
  (format "Context: %s\nTask: %s\nThink and respond with elisp commands:"
          (prin1-to-string context)
          input))

(defun sea--run-llm (prompt)
  "Send PROMPT to LLM and get response."
  (let* ((url "https://api.anthropic.com/v1/messages")
         (headers `(("content-type" . "application/json")
                   ("x-api-key" . ,sea-anthropic-key)
                   ("anthropic-version" . "2023-06-01")))
         (data (json-encode
                `((model . "claude-3-5-sonnet-20241022")
                  (max_tokens . 8192)
                  (messages . [((role . "user")
                              (content . ,prompt))]))))
         (response (request
                    url
                    :type "POST"
                    :headers headers
                    :data data
                    :parser 'json-read
                    :sync t)))
    (when-let* ((data (request-response-data response))
                (content (alist-get 'content data))
                (first-msg (aref content 0)))
      (alist-get 'text first-msg))))

(defun sea--execute-commands (commands)
  "Execute COMMANDS returned by LLM."
  (condition-case err
      (eval (read commands))
    (error
     (message "Error executing commands: %S" err))))

(provide 'sea-core)

(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
