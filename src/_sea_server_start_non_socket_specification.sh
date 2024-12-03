#!/bin/bash
# Time-stamp: "2024-12-03 22:31:08 (ywatanabe)"
# File: ./self-evolving-agent/src/sea_server_start.sh

# Environmental Variables
SEA_UID=$(id -u sea)
SEA_EMACS_SERVER_DIR=/tmp/emacs"$SEA_UID"
SEA_EMACS_SERVER_FILE="$SEA_EMACS_SERVER_DIR"/server

sea_kill_server() {
    if _sea_is_server_running; then
        sudo -u sea emacsclient -e '(kill-emacs)' && sleep 1
        if _sea_is_server_running; then
            sudo pkill -u sea && sleep 1
        fi
    fi
}

_sea_is_server_running() {
    pgrep -u sea emacs >/dev/null
    return $?
}

_sea_setup_server_dir() {
    sudo rm -rf "$SEA_EMACS_SERVER_DIR"
    sudo -u sea mkdir -p "$SEA_EMACS_SERVER_DIR"
    sudo chmod 700 "$SEA_EMACS_SERVER_DIR"
    sudo chown sea:sea "$SEA_EMACS_SERVER_DIR"
}

sea_init_server() {
    sea_kill_server
    _sea_setup_server_dir
    sudo -u sea emacs --daemon
}

_sea_connect_server() {
    sudo -u sea emacsclient -c
}

sea_init_or_connect() {
    if ! _sea_is_server_running; then
        sea_init_server
        while ! _sea_is_server_running || ! sudo test -e "$SEA_EMACS_SERVER_FILE"; do
            sleep 0.1
            echo "waiting for server..."
        done
        sleep 1

    fi
    _sea_connect_server
}

sea_init_or_connect &

# EOF
