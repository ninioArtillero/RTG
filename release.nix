let pkgs = import <nixpkgs> { };

in { project0 = pkgs.haskellPackages.callPackage ./project.nix { }; }
