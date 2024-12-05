<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-06 03:38:03
!-- --- -->


# Self-Evolving Agent (SEA) for Emacs

Running Emacs by an agent, just like as a human engineer.

NOW, THIS REPOSITORY IS UNDER DEVELOPMENT.

## Installation

```bash
git clone https://github.com/user/self-evolving-agent.git ~/.emacs.d/lisp/self-evolving-agent
```

## Configuration
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/self-evolving-agent/src")
(require 'sea)
(sea-install)
```

## Launch an Emacs Window by the SEA user
``` bash
M-x sea-init-server
```

## Working from your own Emacs session

``` elisp
(sea-run "show welcome message")
(sea-run "open google")
(sea-run "write python code to calculate DMD from EEG demo signal and visualize results.")
(sea-run "write sipmle python code")
```






## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



