#!/usr/bin/env bash

# Ensure that there's a persistent GC root for the Nix environment
GC_ROOT="$(pwd)/.nix-gc-root/"

# Create the GC root directory if it doesn't exist
mkdir -p "$GC_ROOT"

# Launch nix-shell with a persistent GC root
nix-shell --add-root "$GC_ROOT"/RTG --indirect shell.nix
