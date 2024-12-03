<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-04 05:00:23
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

## Calling from Elisp

``` elisp
(defun sea-escape-elisp-code (code)
  "Prepare elisp CODE for sea-exec-elisp-code by adding proper escaping."
  (format "'%s'" (prin1-to-string code)))

(sea-exec-elisp-code (sea-escape-elisp-code '(message "hihihi")))

(sea-exec-elisp-code "'\(message \"Hello SEA!!!\"\)'") # this works

(sea-exec-elisp-code
 "'\(progn
    (find-file \"/tmp/test.txt\")
    (insert \"This is a test script.\")
  \)'")
  
(sea--exec-elisp-code 
  "'\(message \"%s\" \(\+ 2 3\)\)'")
```

## Converts Natural Language into elisp functions in a strict JSON format


## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



