;;; -*- lexical-binding: t -*-
;;; Author: 2024-12-02 06:28:46
;;; Time-stamp: <2024-12-02 06:28:46 (ywatanabe)>
;;; File: ./self-evolving-agent/src/sea-installation-virification.el

(require 'ert)

(ert-deftest test-sea-check-dependencies ()
  "Test dependency checking functionality."
  (should (progn (sea-check-dependencies) t))
  (let ((executable-find (lambda (_) nil)))
    (should-error (sea-check-dependencies))))

(ert-deftest test-sea-create-directories ()
  "Test directory creation."
  (let ((sea-work-dir (make-temp-file "sea-test-" t))
        (sea-workspace-dir (make-temp-file "sea-workspace-" t))
        (sea-source-dir (make-temp-file "sea-source-" t)))
    (unwind-protect
        (progn
          (sea-create-directories)
          (should (file-directory-p sea-work-dir))
          (should (file-directory-p sea-workspace-dir))
          (should (file-directory-p sea-source-dir)))
      (delete-directory sea-work-dir t)
      (delete-directory sea-workspace-dir t)
      (delete-directory sea-source-dir t))))

(ert-deftest test-sea-create-initial-files ()
  "Test creation of initial files."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-user-request-file (expand-file-name "user-request.md" temp-dir))
         (sea-request-file (expand-file-name "request.md" temp-dir))
         (sea-history-file (expand-file-name "history.log" temp-dir)))
    (unwind-protect
        (progn
          (sea-create-initial-files)
          (should (file-exists-p sea-user-request-file))
          (should (file-exists-p sea-request-file))
          (should (file-exists-p sea-history-file)))
      (delete-directory temp-dir t))))

(ert-deftest test-sea-setup-environment ()
  "Test environment setup."
  (let* ((temp-dir (make-temp-file "sea-test-" t))
         (sea-config-dir temp-dir)
         (env-file (expand-file-name ".env" temp-dir)))
    (unwind-protect
        (progn
          (sea-setup-environment)
          (should (file-exists-p env-file))
          (should (string-match-p "SEA_ROOT="
                                (with-temp-buffer
                                  (insert-file-contents env-file)
                                  (buffer-string)))))
      (delete-directory temp-dir t))))


(message "%s was loaded." (file-name-nondirectory (or load-file-name buffer-file-name)))
