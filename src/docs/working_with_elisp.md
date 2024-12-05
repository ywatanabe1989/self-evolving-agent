<!-- ---
!-- title: ./self-evolving-agent/src/docs/working_with_elisp.md
!-- author: ywatanabe
!-- date: 2024-12-05 19:21:31
!-- --- -->


# Working with Elisp

``` elisp
(defun sea-escape-elisp-code (code)
  "Prepare elisp CODE for sea-exec-elisp-code by adding proper escaping."
  (format "'%s'" (prin1-to-string code)))

(sea-exec-elisp-code (sea-escape-elisp-code '(message "hi")))

(sea-exec-elisp-code "'\(message \"Hello SEA!!!\"\)'")

(sea-exec-elisp-code
 "'\(progn
    (find-file \"/tmp/test.txt\")
    (insert \"This is a test script.\")
  \)'")
  
(sea-exec-elisp-code 
  "'\(message \"%s\" \(\+ 2 3\)\)'")
```
