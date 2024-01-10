# Soporte de Nix para proyecto Cabal.
# Referencias:
# "Nix recipes for Haskellers": https://srid.ca/haskell-nix
# Fijar una version de nixpkgs: https://nix.dev/tutorials/first-steps/towards-reproducibility-pinning-nixpkgs
# Determinar commit-hash de asociado al canal de nixos: https://discourse.nixos.org/t/how-to-see-what-commit-is-my-channel-on/4818
# Fetch nixpkgs: https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
#
# Este archivo es utilizado por defecto por `nix-build` y `nix-shell`.
# Al correr `nix-shell` se carga una sesión de terminal con todas las dependencias
# declaradas en el archivo `ritmoTG.cabal`.
# Desde esta sesión se pueden utilizar los comandos `cabal` para interactuar con la biblioteca.
# Utilizo Nix de esta manera para abstraer la instalación de Haskell.
#
# Aquí refiero al commit de nixpkgs correspondiente a mi versión local (canal),
# mediante el número de commit corto obtenido de la parte final de `sudo nixos-version`
# Actual:
# Canal de Nix: nixos-23.11 (stable)
# Version de ghc: 9.4.8
# Versión de base: 4.17.2.1
#
let
  pkgs = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/b0b2c5445c64.tar.gz";
    sha256 = "1kvscvl8in723440i0w0sv2j2pawdxjkbiqgx85jda2cz92l6rpm";
  }) { };
in pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv
    (with pkgs.haskellPackages; [ cabal-install ]);
}
