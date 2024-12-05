<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-06 01:10:13
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

#### Launch an Emacs session from the SEA user
``` bash
sudo echo aaa && ./src/sea_server.sh init # Fixme: `../src/sea-server.el` should control server setup
```

#### Working from your Emacs session

``` elisp
(sea--sudo-get-password)
(sea-run "show welcome message")
(sea-run "open google")
(sea-run "write python code to calculate DMD from EEG demo signal and visualize results.")


```






## 
## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



