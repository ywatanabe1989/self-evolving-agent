<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-04 04:44:13
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

## Calling from elisp

``` elisp
(sea-exec-elisp-code "'\(message \"Hello SEA!!!\"\)'")

(sea-exec-elisp-code
 "'\(progn
    (find-file \"/tmp/test.txt\")
    (insert \"This is a test script.\")
  \)'")
  
(sea--exec-elisp-code 
  "'\(message \"%s\" \(\+ 2 3\)\)'")
```


## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



