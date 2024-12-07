<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-07 17:19:48
!-- --- -->


# Seamacs: self-evolving agent on Emacs

## Introduction
The integration of Emacs and LLM agents offers unique advantages:

- Purely text-based operations, which are highly compatible with LLMs
- Emacs ecosystem with extensive customization
- Natural interaction with Emacs users
- Central hub for development tools and system operations

Here, we revive Emacs - born in MIT's AI Lab in the 1960s - as a modern platform for AI agents.


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
sudo echo aaa && sudo ./src/sea_server.sh start
sudo ./src/sea_server.sh kill
sudo ./src/sea_server.sh init

(sea-init-server)
```

## Reload source of self-evolving-agent on the SEA emacs
``` bash
<!-- (message (format "%s" default-directory)) -> /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/ -->
sudo chmod 774 -R /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/
sudo chown ywatanabe:sea -R /home/ywatanabe/.emacs.d/lisp/self-evolving-agent/
sudo chmod 770 /home/sea/.emacs.d/server/server
(sea-exec-elisp-code '(load-file "/home/sea/.emacs.d/init.el"))
```

## Edit on the SEA EMACS

``` bash
sudo usermod -a -G sea ywatanabe
```

## Working from your own Emacs session

``` elisp
(sea--sudo-get-password)
(sea-run "show welcome message")
(sea-run "open google")
(sea-run "write python code which calculates DMD from EEG demo signal and visualize results.")
(sea-run "using the internet, perform literature review regarding epilepsy seizure prediction from bio signals")


sudo chmod 775 /home/ywatanabe/.dotfiles/.emacs.d/lisp/self-evolving-agent/
```






## Contact
ywatanabe@alumni.u-tokyo.ac.jp




# EOF



