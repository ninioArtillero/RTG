# Rhythm, Time and Geometry

RTG (Rhythm, Time and Geometry) is a music EDSL for the creation and manipulation of rhythmic patterns
implementing a geometrically informed API exploring:

1. The two way relation between musical rhythm and geometric structure
1. Computational abstractions and implementations of musical time.

It is part of my doctoral research project on the affordances of programming language abstractions in music language design and implementation, particularly in the context of live coding.
It addresses the following general question:
How programming language design and features translate to musical expressiveness for composition and live performance?

> [!IMPORTANT]
> The library is in a _provisional_ state and still not released on [Hackage](https://hackage.haskell.org/).
> The API is bloated (most module functions are exported for experimentation)
> and still subject to redesign.

## Related Papers

- [Rhythm, Time and Geometry](https://doi.org/10.21428/108765d1.e65cd604) @ Algorithmic Pattern Salon (2023)
- [Demo: A Geometric Approach to Generate Musical Rhythmic Patterns in Haskell](https://doi.org/10.1145/3677996.3678295) @ FARM (2024)

## Installation

### Using GHCup

This method installs Haskell tools system wide and is the most direct way of using the library.
In case of problems, you may try [using nix](#using-nix).

1. [Install GHCup](https://www.haskell.org/ghcup/) to install the Haskell toolchain (you may select all the default options).
1. Clone this repository.
1. Run `cabal build` from the root of the repository.
   In case of error, please [open an issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) with the output.
1. Follow the [usage](#usage) instructions below to use RTG.

### Using Nix

The alternative installation uses the [Nix](https://nixos.org/manual/nix/stable/) package manager to achieve a reproducible cross-platform development environment, necessary for interacting with the library.
Another advantage of this method is that it abstracts away the installation of [Haskell](https://www.haskell.org/downloads/) and reduces dependency compilation thanks to the [Nix binary cache](https://cache.nixos.org), which covers a large part of the [packages in its repository](https://search.nixos.org/packages). Below are the steps to install the required components.

1. [Install Nix Package Manager](https://nixos.org/download). Note that on Windows, you'll need [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) with [systemd enabled](https://devblogs.microsoft.com/commandline/systemd-support-is-now-available-in-wsl/).
1. Clone this repository.
1. From the root of the repository, run `./run.sh` and choose the "default" option: it uses nix to build and install the library.
   If the process completes successfully the interpreter (`ghci`) prompt will be waiting for a command.
   In case of error, please [open an issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) with the output.
1. Exit with the `:quit` command.
1. Follow the [usage](#usage) instructions below to use RTG.

> [!WARNING]
> Nix will download (and, if necessary, build) all the library dependencies.
> This process may take a while to finish, but subsequent invocations will be almost immediate
> as long as the Nix store is not cleaned (with `nix-collect-garbage` or `nix-store --gc` for example).
> If you need to clean the store to free up some space and want to avoid a subsequent rebuild,
> add the following lines to your system's nix configuration file (in Linux this is located at `/etc/nix/nix.conf`):
>
> ```
> keep-outputs = true
> keep-derivation = true
> ```
>
> With this options, the garbage-collection root created automatically by the `run.sh` script
> at `.nix-gc-roots/` will make the build persistent.

### Audio dependencies

1. Install SuperDirt (audio engine):
   1. Install [SuperCollider](https://supercollider.github.io/downloads.html) and, to have all the predefined synths, the [sc3-plugins](https://supercollider.github.io/sc3-plugins/). Both are accessible through the package manager in various Linux distributions.
   1. In the SuperCollider IDE (or in a terminal at the `sclang` shell prompt) run: `Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.4"); thisProcess.recompile()})`.
1. [Install FluidSynth](https://github.com/FluidSynth/fluidsynth/wiki/Download) (virtual midi synth).
1. You might need a (virtual) MIDI connection and routing application, such as `qjackctl` (for Jack or Pipewire on Linux), `qpwgraph` (Pipewire on Linux) or Audio MIDI Setup (default on MacOS). For Windows, this [article](https://www.donyaquick.com/midi-on-windows/#x1-80002.3) suggests to the "loopMIDI" application (which currently support Windows 7 to 10).

## Usage

### Preparations

- Start SuperDirt: Open SuperCollider (or run `sclang` from a terminal) and run (Ctrl+Enter) `SuperDirt.start`.
  This will load the audio engine and load its standard samples.
  - Alternatively, or in case of SuperCollider error messages regarding the buffer or late messages, run with the SuperDirt configuration file provided (it contains configuration and optimization options):
    `sclang superdirt_startup.scd`
- Run `fluidsynth` its own terminal window and make sure is connected to your default midi output device (through the routing application of your system).
  - In Linux (using pipewire-jack or jack), you may run the following command to make the midi connections automatically:
    `fluidsynth --server --audio-driver jack --midi-driver jack --connect-jack-outputs`
- Open a terminal at the repository root (where _this_ file is located)
  - Run `cabal repl` or
  - If installed using Nix: run `./run.sh`, select `default` and wait for the input prompt.
- Sample names used in some pattern functions match those of the `DirtSamples` quark installed by `SuperDirt`.
  a list of them can be printed by running `~dirt.postSampleInfo;` in SuperCollider.

### Current API functions

RTG has several funcions to generate rhythmic patterns
that are interpreted as MIDI and OSC message streams.
In the first case the default MIDI output device is used.
The list of active MIDI devices can be displayed using the `devices` command at the prompt:

`λ> devices`

For OSC message playback, an active SuperDirt instance,
with its standard sample library loaded, is needed.
To create a pattern use any of the following:

```haskell
λ> o <sample> <rhythm>          -- OSC  | play rhythm once
λ> l <sample> <rhythm>          -- OSC  | play rhythm in loop
λ> p <root> <rhythmA> <rhythmB> -- MIDI | play rhythmA as an scale over rhythmB (loop)
λ> s <root> <rhythmA>           -- MIDI | play rhythmA as a scale (loop)
```

The playback speed of the patterns can be changed on the fly.

```haskell
λ> setcps <new-cps>             -- Change CPS value
λ> readcps                      -- View current CPS value
```

To stop playback make sure to bind all patterns to a name
with the following syntax:

```haskell
λ> name <- ...
λ> stop name
```

If a name is given again to a new pattern, the former becomes headless
and the ghci session needs to be ended to kill all active pattern.

`λ> :quit`

For examples, try running the following commands in sequence:

```haskell
λ> pat1 <- l "cp" $ rumba <> japanese
λ> o "blip" $ e'(7,13,0) & amiotScale
λ> pat2 <- p (Fs,3) wholeTone (e'(3,8,2))
λ> o "can" $ e'(3,7,0) <> e'(4,8,3)
λ> readcps
λ> setcps 160
λ> stop pat1
λ> stop pat3
```

## Development

> [!NOTE]
> The project is being developed with GHC 9.4.8 using regular `cabal` commands.
> The `run.sh` script and the nix-shell itself are intended for end users.

### About the nix-shell environment

The current default `shell` environment can be invoked directly by `nix-shell nix/shell0.nix`.
It depends on the files created by the following sequence of commands,
which should be called to update them any time the `.cabal` file is changed.

```sh
# Translate cabal file into a nix function
cabal2nix ./. > nix/project.nix

REV = nix-instantiate --eval --expr 'builtins.readFile <nixpkgs/.git-revision>'

# Version reference to pin packages
nix-prefetch-git https://github.com/NixOS/nixpkgs.git $REV > nix/nixpkgs.json
```

For comparison, a default `shell-example.nix` file can be created with

```sh
cabal2nix . --shell > nix/shell-example.nix
```

To build the project using nix:

```sh
nix-build --attr project nix/release.nix
```

The compiler version can be given as an argument for the build:

```sh
nix-build --argstr compiler ghc964 --attr project nix/release.nix
```

## TODO

**Current implementation**

- [x] Asynchronous evaluation of patterns
- [x] Make patterns addressable so they can be stopped and updated(¿in global state?)
- [x] Add cps to sequencer
- [ ] Soloing many patterns together
- [ ] Global pattern operations and transformations
- [x] Fix pattern update timing (per cycle to avoid jumps)
- [ ] Fix pattern resizing -> add feature
- [ ] Make sequencer inform of current pattern length, balance and evenness.
- [ ] Implement pattern sync alternatives
- [ ] A syntax to make new scheduled patterns affect current playing patterns
- [x] Homogenize rhythmic pattern types show function. Each type should be tagged appropriately.
- [ ] Add perfectly balance rhythm library (no efficient algorithm on-sight)
- [ ] Implement well-formed rhythms (¿3 parameters?)
- [ ] Continuous morphing of well-formed rhythms using ratio parameter
- [ ] Have _signals_ for parameter control (for morphing)
- [ ] Geometrically informed continuous morphing between two arbitrary rhythms
- [ ] Add `doctest`/`doctest-extract` for automatic in-documentation property testing (QuickCheck)
- [x] Fix haddocks (see Polygon module for example module header)
- [x] Use HashMaps for sequences.
- [ ] Implement tests for bjorklund / LH specifications
- [x] Preserve Sequencer State between `ghci` reloads.
- [ ] Elaborate graph of execution model
- [ ] Breakdown `Sequencer` module. Execution, PatterPool, SequencerPattern and Output Values. Abstract over the _Fiber Bundle_ structure.
- [ ] Fix implementation for simultaneous events.
- [ ] Currently timing comes from the Haskell runtime (Timed IO Monad). Get detailed timing using timestamps on OSC messages.
- [ ] Fix Nix installation.

**Alternative implementations and further work**

- [ ] FRP implementations.
  - [ ] Tidal Cycles pattern representation
  - [ ] AFRP: Yampa, Euterpea MUI library
- [ ] New interpreter (for `v.0.2.0.0`)
  - [ ] Parsing: parsec/megaparsec
- [ ] Audio
  - [x] OSC: `hosc`
  - [x] MIDI: `Euterpea`
  - [ ] Embed MIDI functionality using `fluidsynth`
- [ ] Create a minimal sampler for RTG

**Midterm**

- [ ] Expose options to change/modify event streams to make the system modular (regarding changing sound engine or using other digital instrument).
- [ ] Create a Tidal Cycles module version.
