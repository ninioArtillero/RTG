{ compiler ? "default" }:

let
  # Pinning nixpkgs with nixpkgs tools
  bootstrap = import <nixpkgs> { };

  # Nix packages git revision information in JSON generated by
  # REV = nix-instantiate --eval --expr 'builtins.readFile <nixpkgs/.git-revision>'
  # to get the commit hash and
  # nix-prefetch-git https://github.com/NixOS/nixpkgs.git $REV > nixpkgs.json
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in { project = haskellPackages.callPackage ./project.nix { }; }