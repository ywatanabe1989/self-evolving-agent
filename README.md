<!-- ---
!-- title: ./self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-01 20:36:59
!-- --- -->


# Self-Evolving Agent for Emacs

An Emacs package that implements a self-improving AI agent system.

## Security & Permissions

### Initial Setup
./docs/install.sh

### Emacs Configuration
```elisp
(setq sea-workspace-dir "/opt/sea")
(setq sea-readonly-mode t)
(setq sea-sandbox-mode t)
(setq sea-require-approval t)
```

## Components

- core.el: Core agent (read-only)
- network.el: Network coordination
- seed.el: Configuration
- utils.el: Utilities
- version_control.el: Git integration

## Usage

```elisp
;; Safety guaranteed operations
(sea-self-improve "path/to/file.el") 
(sea-spawn-agents '(task1 task2))
```

# Check permissions
ls -la /opt/sea
ls -la /home/sea/.config/sea/keys.el

# Verify group memberships
groups sea
groups ywatanabe
