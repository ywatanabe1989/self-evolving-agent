<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-05 19:20:14
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


## Converts Natural Language into elisp code

``` elisp
(sea--prompt-to-elisp "hello world")
```





## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



