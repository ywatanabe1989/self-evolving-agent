#!/bin/bash
# Time-stamp: "2024-12-01 20:36:34 (ywatanabe)"
# File: ./self-evolving-agent/docs/install.sh

#!/bin/bash
# setup-sea.sh
# Author: ywatanabe
# Date: $(date +"%Y-%m-%d-%H-%M")

LOG_FILE="setup-sea.log"

usage() {
    echo "Usage: $0 [-u|--user ] [-h|--help]"
    echo
    echo "Options:"
    echo " -u, --user Main user to grant sea access (required)"
    echo " -h, --help Display this help message"
    echo
    echo "Example:"
    echo " $0 -u ywatanabe"
    exit 1
}

log_msg() {
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] $1" | tee -a "$LOG_FILE"
}

# setup_user() {
#     local main_user=$1

#     # Create sea user if not exists
#     if ! id "sea" &>/dev/null; then
#         log_msg "Creating sea user..."
#         sudo useradd -r -m -d /home/sea sea
#         sudo chmod 755 /home/sea
#     fi

#     # Configure groups
#     log_msg "Configuring groups..."
#     sudo usermod -aG sea "$main_user"
#     sudo usermod -aG "$main_user" sea

#     # Fix home permissions
#     log_msg "Setting home directory permissions..."
#     sudo chown -R sea:sea /home/sea
# }

setup_user() {
    local main_user=$1

    # Create sea user if not exists
    if ! id "sea" &>/dev/null; then
        log_msg "Creating sea user..."
        sudo useradd -r -m -d /home/sea sea || {
            log_msg "Error: Failed to create sea user"
            exit 1
        }
        sudo chmod 755 /home/sea
    else
        # Ensure home directory exists even if user exists
        sudo mkdir -p /home/sea || {
            log_msg "Error: Failed to create /home/sea directory"
            exit 1
        }
    fi

    # Configure groups
    log_msg "Configuring groups..."
    sudo usermod -aG sea "$main_user"
    sudo usermod -aG "$main_user" sea

    # Fix home permissions
    log_msg "Setting home directory permissions..."
    sudo chown -R sea:sea /home/sea
}

setup_workspace() {
    log_msg "Setting up workspace..."

    # Create directory structure
    sudo mkdir -p /opt/sea/{workspace,backups,logs}
    sudo chown -R sea:sea /opt/sea
    sudo chmod -R 2775 /opt/sea

    # Configure sea user directory
    sudo -u sea mkdir -p /home/sea/.config/sea
    sudo -u sea touch /home/sea/.config/sea/keys.el
    sudo chmod 600 /home/sea/.config/sea/keys.el
}

verify_setup() {
    log_msg "Verifying setup..."

    ls -la /opt/sea
    ls -la /home/sea/.config/sea/keys.el
    groups sea
    groups "$1"
}

main() {
    local main_user=""

    while [[ $# -gt 0 ]]; do
        case $1 in
            -u|--user)
                main_user="$2"
                shift 2
                ;;
            -h|--help)
                usage
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done

    if [[ -z "$main_user" ]]; then
        echo "Error: User parameter is required"
        usage
    fi

    setup_user "$main_user"
    setup_workspace
    verify_setup "$main_user"

    log_msg "Setup completed successfully"
}

main "$@" 2>&1 | tee "$LOG_FILE"

# EOF
