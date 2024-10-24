#!/usr/bin/env bash

# Ensure that there's a persistent GC root for the nix-shell
# and launch cabal repl

# List of current variants of nix-shell specification files
SHELLS=("default" "shell0" "shell1")

# Base paths
GC_ROOTS="$(pwd)/.nix-gc-roots/"
NIX_DIR="$(pwd)/nix"

# Function to display a menu and get user's selection
select_shell() {
    printf "Please select a Nix shell file:\n"

    local num_shells=${#SHELLS[@]}
    local quit_option=$((num_shells + 1))

    select shell in "${SHELLS[@]}" "Quit"; do
        case $REPLY in
        [1-$num_shells]) # Valid shell selection
            printf "You have selected: %s\n" "$shell"
            SHELL=$shell
            break
            ;;
        $quit_option) # Quit option
            printf "Exiting...\n"
            exit 0
            ;;
        *) # Invalid selection
            printf "Invalid selection. Please try again.\n"
            ;;
        esac
    done
}

# Function to create a GC root
create_gc_root() {
    local shell_root
    shell_root="$GC_ROOTS/$SHELL"
    if [[ "$SHELL" == "default" ]]; then
        nix_file_path="$(pwd)/$SHELL.nix" # Special case: default.nix needs to be at the project root
    else
        nix_file_path="$NIX_DIR/$SHELL.nix"
    fi

    if [ ! -d "$GC_ROOTS" ]; then
        mkdir -p "$GC_ROOTS" || {
            printf "Failed to create GC_ROOTS directory\n"
            exit 1
        }
        nix-instantiate "$nix_file_path" --add-root "$shell_root" --indirect &&
            printf "A new GC root has been created\n"
    elif [ ! -L "$shell_root" ]; then
        nix-instantiate "$nix_file_path" --add-root "$shell_root" --indirect &&
            printf "A new GC root has been created\n"
    else
        printf "A GC root already exists, use 'make clean' to erase all previous gc roots\n" &&
            printf "in case the cabal file is modified or the pinned nixpkgs is updated\n"
    fi
}

# TODO: Trap to handle cleanup on exit or interruption
cleanup() {
    printf "Cleaning up...\n"
    # Cleanup commands here
}
trap cleanup EXIT

# Prompt user to select the shell file
select_shell

# Create or reuse the GC root
create_gc_root

# Launch the nix-shell
printf "Launching RTG inside a nix-shell with a persistent GC root\n"
nix-shell "$nix_file_path" --run 'cabal repl' || {
    printf "Failed to launch nix-shell\n"
    exit 1
}
