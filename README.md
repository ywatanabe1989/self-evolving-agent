<!-- ---
!-- title: ./.dotfiles/.emacs.d/lisp/self-evolving-agent/README.md
!-- author: ywatanabe
!-- date: 2024-12-01 20:28:38
!-- --- -->


# Self-Evolving Agent for Emacs

An Emacs package that implements a self-improving AI agent system.

## Security & Permissions

### User Setup
```bash
# Create dedicated sea user and group
sudo useradd -r sea
sudo usermod -aG sea ywatanabe

# Set workspace permissions
sudo mkdir -p /opt/sea
sudo chown -R sea:sea /opt/sea
sudo chmod -R 2775 /opt/sea
```

### Emacs Configuration
```elisp
;; Set restricted workspace
(setq sea-workspace-dir "/opt/sea")

;; Enable mandatory safety measures
(setq sea-readonly-mode t)          ; Read-only mode for core files
(setq sea-sandbox-mode t)           ; Isolated evaluation environment
(setq sea-require-approval t)       ; Manual approval for modifications
```

## Components

- core.el: Core agent functionality (read-only)
- network.el: Network coordination (restricted)
- seed.el: Basic configuration
- utils.el: Utility functions
- version_control.el: Git integration

## Installation

1. Set up permissions:
```bash
sudo -u sea mkdir -p /opt/sea/{backups,logs,workspace}
sudo chmod 2775 /opt/sea/*
```

2. Install dependencies:
```elisp
(require 'request)
(require 'json)
(require 'w3m)
```

3. Configure API keys:
```bash
# Store keys in sea user's home
sudo -u sea mkdir -p ~sea/.config/sea
sudo -u sea touch ~sea/.config/sea/keys.el
sudo chmod 600 ~sea/.config/sea/keys.el
```

## Usage (Restricted Mode)

```elisp
;; All operations require explicit user approval
(sea-self-improve "path/to/file.el") ; Prompts for confirmation
(sea-spawn-agents '(task1 task2))    ; Runs in sandbox
```
