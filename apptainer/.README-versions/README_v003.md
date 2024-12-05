<!-- ---
!-- title: ./self-evolving-agent/apptainer/README.md
!-- author: ywatanabe
!-- date: 2024-12-05 02:27:59
!-- --- -->


## Apptainer Installation


``` bash
# Add Apptainer repository
sudo add-apt-repository ppa:apptainer/ppa
sudo apt update

# Install Apptainer
sudo apt install apptainer

<!-- # Install required package
 !-- sudo apt install -y rpm2cpio
 !-- 
 !-- # Main
 !-- curl -s https://raw.githubusercontent.com/apptainer/apptainer/main/tools/install-unprivileged.sh | \
 !--     bash -s - ~/.local/bin/apptainer
 !-- 
 !-- # Link to PATH
 !-- ln -s ~/.local/bin/apptainer/bin/apptainer ~/.bin/ -->
```

## Enables fakeroot

``` bash
# Install required package
sudo apt-get install uidmap

# Add your user to subuid and subgid
sudo usermod --add-subuids 100000-165536 $USER
sudo usermod --add-subgids 100000-165536 $USER

# Verify the mappings
grep $USER /etc/subuid
grep $USER /etc/subgid
```

## Build SEA

``` python
# Build
apptainer build --fakeroot ./apptainer/sea.sif ./apptainer/sea.def
apptainer build --fakeroot --sandbox ./apptainer/sea.sandbox ./apptainer/sea.def

# Run interactive shell
apptainer shell sea.sif

# Run SEA server
apptainer instance start sea.sif sea
```

