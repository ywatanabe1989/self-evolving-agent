<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-02 07:30:15
!-- --- -->


# Self-Evolving Agent (SEA) for Emacs

An Emacs package implementing a self-improving AI agent system.

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

## Components

- sea-core.el: Core agent functionality (read-only)
- sea-network.el: Network operations
- sea-seed.el: System initialization
- sea-utils.el: Utility functions
- sea-version-control.el: Version control integration
- sea-config.el: Basic configurations

## Usage

```elisp
M-x sea-self-evolve
```
