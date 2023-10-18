# Soporte de Nix para proyecto Cabal.
# Referencias:
# "Nix recipes for Haskellers": https://srid.ca/haskell-nix
# Fijar una version de nixpkgs: https://nix.dev/tutorials/first-steps/towards-reproducibility-pinning-nixpkgs
# Determinar commit-hash de asociado al canal de nixos: https://discourse.nixos.org/t/how-to-see-what-commit-is-my-channel-on/4818
#
# Este archivo es utilizado por defecto por `nix-build` y `nix-shell`.
# Al correr `nix-shell` se carga un shell con todas las dependencias
# declaradas en el archivo `ritmoTG.cabal`, desde el cual
# de pueden utilizar todos los comandos `cabal` usuales.
#
# Una ventaja de utilizar Nix en lugar de Stack para hacer
# builds reproducibles es que Nix puede manejar también paquetes que
# no son de haskell.
#
# Canal de Nix utilizado: nixos-23.05 (stable)
# Version de ghc: 9.2.8 <-> Versión de base: 4.16.4.0
# Hash corto del commit correspondiente a mi versión local del canal: 18784aac1013
#
let
  # pkgs = import <nixpkgs> { };
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") { };
  # pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/18784aac1013.tar.gz") { };
in 
  pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv
    (with pkgs.haskellPackages; [ cabal-install ]);
  }
