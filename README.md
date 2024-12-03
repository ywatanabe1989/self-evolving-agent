<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-04 04:12:28
!-- --- -->


# Self-Evolving Agent (SEA) for Emacs

Running Emacs by an agent, just like as a human engineer.

NOW, THIS REPOSITORY IS UNDER DEVELOPMENT.

## Installation

1. Clone repository:
```bash
git clone https://github.com/user/self-evolving-agent.git ~/.emacs.d/lisp/self-evolving-agent
```

2. Add to your Emacs config:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/self-evolving-agent/src")
(require 'sea)
```

3. Call the installation function
```elisp
M-x sea-install
```

## Usage

``` elisp
M-x sea-self-evolve
```

## Working with shell script

``` bash
./src/sea_server.sh [start|stop|restart|status|execute]
./src/sea_server.sh start &
./src/sea_server.sh execute '(message "hello")'
./src/sea_server.sh execute '(message "hihi")'
./src/sea_server.sh execute '(progn (with-current-buffer (get-buffer-create "*test*") (insert "hello")) (switch-to-buffer "*test*"))'
```


(shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? ")) | sudo -S ls /tmp/emacs999/"))

``` elisp
(shell-command (format "echo %s | sudo -S %s" (sea--sudo-get-password) "./src/sea_server.sh execute '(message \"hello!!!\")'"))


(shell-command
 (format "echo %s | sudo -S %s"
         (sea--sudo-get-password)
         (expand-file-name "sea_server.sh execute '(message \"hello!!!!!!!!!!!\")'") sea-user-source-dir))
```


<!-- (sea-init-or-connect)
 !-- (sea--run-sudo-command (format "execute %s" "'\(message \"hihihihih\"\)'"))
 !-- 
 !-- (let (runner (sea--run-sudo-command "execute"))
 !--     runner "(message \"hello\")")
 !--     
 !-- (call-process-shell-command "sudo -u sea emacsclient -e \"(message \"hihi\")\" --socket-name \"/tmp/emacs999/server\"")
 !-- 
 !-- # works
 !-- sudo -u sea emacsclient -e "(message \"hihi\")" --socket-name "/tmp/emacs999/server"
 !-- 
 !-- ```
 !-- 
 !-- ## see-exec-command
 !-- (sea--run-sudo-command "start")
 !-- (sea--run-sudo-command (concat "execute " "'\(message \"hihihihihi\""))
 !-- (sea--run-sudo-command "execute \'(insert \"hello\")\'")
 !-- 
 !-- "execute '(message \"Hello SEA!\")'"
 !-- (sea--run-sudo-command (format "execute '%s'" "(message \"hello\")"))
 !-- 
 !-- 
 !-- ``` elisp
 !-- ;; Test 1: Simple message
 !-- (sea--exec-elisp-code "(message \"Hello SEA!\")")
 !-- 
 !-- ;; Test 2: Return value
 !-- (sea--exec-elisp-code "(+ 2 3)")
 !-- 
 !-- ;; Test 3: Variable setting and reading
 !-- (sea--exec-elisp-code "(progn (setq test-var 42) test-var)")
 !-- 
 !-- ;; Test 4: Complex expression
 !-- (sea--exec-elisp-code "(progn 
 !--   (message \"Starting...\")
 !--   (setq x 10)
 !--   (+ x 5))")
 !-- ```
 !-- 
 !-- 
 !-- ## 
 !-- ## Contact
 !-- ywatanabe@alumni.u-tokyo.ac.jp
 !-- 
 !-- 
 !-- 
 !-- 
 !-- # EOF -->



