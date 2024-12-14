# Rhythm, Time and Geometry

RTG (Rhythm, Time and Geometry) is a Haskell library for the creation and manipulation of rhythmic patterns
implementing a creative interface exploring

1. The two way relation between musical rhythm and geometric structure
1. Computational abstractions and implementations of musical time.

It is part of my doctoral research project on the affordances of programming language abstractions in music language design and implementation, particularly in the context of live coding.
It addresses the following question: how programming language expressiveness translates to musical expressiveness for composition and live performance.

## TODO

**Current implementation**

* [x] Asynchronous evaluation of patterns
* [ ] Make patterns addressable so they can be stopped and updated(¿in global state?)
* [ ] Implement pattern sync alternatives
* [ ] A syntax to make new scheduled patterns affect current playing patterns
* [ ] Homogenize rhythmic pattern types show function. Each type should be tagged appropriately and include geometric properties info.
* [ ] Implement well-formed rhythms (¿3 parameters?)
* [ ] Continuous morphing of well-formed rhythms using ratio parameter
* [ ] Have _signals_ for parameter control (for morphing)
* [ ] Geometrically informed continuous morphing between two arbitrary rhythms
* [ ] Add `doctest`/`doctest-extract` for automatic in-documentation property testing (QuickCheck)
* [ ] Fix haddocks (see Polygon module for example header)


**Alternative implementations and options**

* [ ] FRP implementations.
  * [ ] MUIs from "The Haskell School of Music"
  * [ ] Tidal Cycles
  * [ ] AFRP: Yampa
* [ ] API design
  * [ ] Parsing: parsec library
* [ ] Audio
  * [x] OSC: `hosc`
  * [x] MIDI: `Euterpea` 
  * [ ] Embed MIDI functionality using `fluidsynth`

**Midterm**

* [ ] Expose options to change/modify message streams to make the system modular (regarding changing sound engine or using other digital instrument).
* [ ] Create a Tidal Cycles module version.

## Installation

This project uses the [Nix](https://nixos.org/manual/nix/stable/) package manager to achieve a reproducible cross-platform development environment, necessary for interacting with the library.
Another advantage of this method is that it abstracts away the installation of [Haskell](https://www.haskell.org/downloads/) and reduces dependency compilation thanks to the [Nix binary cache](https://cache.nixos.org), which covers a large part of the [packages in its repository](https://search.nixos.org/packages). Below are the steps to install the required components.

1. [Install Nix Package Manager](https://nixos.org/download). Nix provides installation scripts that can be run with the following commands:
   1. On Linux: `sh <(curl -L https://nixos.org/nix/install) --daemon`.
   1. On MacOS: `sh <(curl -L https://nixos.org/nix/install)`.
   1. On Windows, you need [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) with [systemd enabled](https://devblogs.microsoft.com/commandline/systemd-support-is-now-available-in-wsl/): `sh <(curl -L https://nixos.org/nix/install) --daemon`.
1. Install SuperDirt (audio engine).
   1. Install [SuperCollider](https://supercollider.github.io/downloads.html) and, to have all the predefined synths, the [sc3-plugins](https://supercollider.github.io/sc3-plugins/). Both are accessible through the package manager in various Linux distributions.
   1. With the SuperCollider interpreter (`sclang`) running, execute `Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.3"); thisProcess.recompile()})`.
1. Clone this repository.
1. From the root of the repository, run `nix-shell --run 'cabal repl'`.
Nix will proceed to download (and, if necessary, compile) the library dependencies.
This process may take some time, but subsequent invocations will be almost immediate
as long as the Nix store is not cleaned (with `nix-collect-garbage` or `nix-store --gc` for example).
If the process completes successfully, a _shell_ environment will start with the necessary tools and dependencies for RTG,
it will compile the library and open a Haskell interpreter (`ghci`) session with the library loaded.
   1. In case of error, please [open an issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) with the output.
1. Follow the [usage](#usage) instructions to test the functions exported by each of the modules.
1. To exit the session, run `:quit` in the command line.

## Usage

### Preparations

Start the SuperDirt: Open SuperCollider (or run `sclang` from a terminal) and runt (Ctrl+Enter) `SuperDirt.start`.
This will load the audio engine and load its standard samples.
Alternatively, or in case of SuperCollider error messages regarding the buffer or late messages, run the following SuperDirt configuration file containing optimization options:
[archivo](https://raw.githubusercontent.com/musikinformatik/SuperDirt/develop/superdirt_startup.scd)

Open a terminal at the repository root (where `this` file is located) and run `nix-shell --run 'cabal repl'` to open an interpreter loaded with the library.

### TODO: Current API functions

## Development

The project is being developed with GHC 9.4.8

Use `nix-shell` to load a development environment using nix.

The current `shell.nix` has depends on the files created by the following sequence of commands

``` sh
cabal2nix . > project.nix

REV = nix-instantiate --eval --expr 'builtins.readFile <nixpkgs/.git-revision>'

nix-prefetch-git https://github.com/NixOS/nixpkgs.git $REV > nixpkgs.json
```

For comparison, a default `shell.nix` file can be created with

```sh
cabal2nix . --shell > shell.nix
```

To build the project using nix:

``` sh
nix-build --attr project release.nix
```

The compiler can be given as an argument for the build:

```sh
nix-build --argstr compiler ghc965 --attr project release.nix
```
