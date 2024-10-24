#!/usr/bin/env bash

# Ensure that there's a persistent GC root for the Nix environment
# and launch cabal repl
GC_ROOTS="$(pwd)/.nix-gc-roots/"

if [ ! -d "$GC_ROOTS" ]; then
    mkdir -p "$GC_ROOTS" && echo "A gc-root directory has been created"
fi

echo "Launching RTG inside a nix-shell with a persistent GC root" &&
    nix-instantiate --add-root "$GC_ROOTS"/RTG ./nix/shell0.nix &&
    nix-shell ./nix/shell0.nix --run 'cabal repl'
