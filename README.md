<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-01 21:08:23
!-- --- -->


# Self-Evolving Agent (SEA) for Emacs

An Emacs package implementing a self-improving AI agent system.

## Installation

1. Clone repository:
```bash
git clone https://github.com/user/self-evolving-agent.git ~/.emacs.d/lisp/self-evolving-agent
```

2. Run setup script:
```bash
cd ~/.emacs.d/lisp/self-evolving-agent
./docs/install.sh
```

3. Add to your Emacs config:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/self-evolving-agent/src")
(require 'sea)
```

## Security Configuration

```elisp 
;; (setq sea-workspace-dir "/opt/sea")
;; (setq sea-readonly-mode t)
;; (setq sea-sandbox-mode t)
;; (setq sea-require-approval t)
```

## Components

- sea-core.el: Core agent functionality (read-only)
- sea-network.el: Network operations
- sea-seed.el: System initialization
- sea-utils.el: Utility functions
- sea-version-control.el: Version control integration
- sea-config.el: Basic configurations

## Usage

```elisp
;; Main Operation
(sea-self-evolve)

;; Safety-first operations
(sea-self-improve "path/to/file.el")
(sea-spawn-agents '(task1 task2))
```
