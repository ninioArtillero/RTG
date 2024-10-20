#!/usr/bin/env bash

# Ensure that there's a persistent GC root for the Nix environment
# and launch cabal repl

GC_ROOT="$(pwd)/.nix-gc-root/"

if [ ! -d "$GC_ROOT" ]; then
    mkdir -p "$GC_ROOT" && echo "A gc-root directory has been created"
fi

echo "Launching RTG inside a nix-shell with a persistent gc-root" &&
    nix-instantiate --add-root "$GC_ROOT"/RTG1 shell1.nix &&
    nix-shell shell1.nix
