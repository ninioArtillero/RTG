# Rhythm, Time and Geometry

RTG (Rhythm, Time and Geometry) is a music EDSL for the creation and manipulation of rhythmic patterns
implementing a geometrically informed API exploring:

1. The two way relation between musical rhythm and geometric structure.
1. Computational abstractions and implementations of musical time.

It is part of a broad research project on the affordances of programming language
abstractions in music language design and implementation, particularly in the context
of live coding. As part of my PhD research, it addresses the following general question:

> How to design a live coding language that leverages the geometric structure of 
> rhythmic patterns via algebraic transformations/operations, ensures compositional
> coherence of these operations, and rigorously formalizes computational representation
> of musical time?

> [!IMPORTANT]
> The library is in a _experimental_ state.

## Related Papers

- [Rhythm, Time and Geometry](https://doi.org/10.21428/108765d1.e65cd604) @ Algorithmic Pattern Salon (2023)
- [Demo: A Geometric Approach to Generate Musical Rhythmic Patterns in Haskell](https://doi.org/10.1145/3677996.3678295) @ FARM (2024)

## Installation

### Haskell tool-chain

1. [Install GHCup](https://www.haskell.org/ghcup/) —the recommended tool
   to administer the Haskell tool-chain— to get `cabal` and `ghc`. You may select all
   the default options of the installation script.
1. Clone this repository.
1. Run `cabal build` from the root of the repository.
   In case of error, please [open an issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) with the output.
1. Follow the [usage](#usage) instructions below to use RTG.

### Audio dependencies

1. Install SuperDirt (audio engine):
   1. Install [SuperCollider](https://supercollider.github.io/downloads.html) and,
   to have all the predefined synths, the [sc3-plugins](https://supercollider.github.io/sc3-plugins/).
   Both are accessible through the package manager in various Linux distributions.
   1. In the SuperCollider IDE (or in a terminal at the `sclang` shell prompt) run:
   `Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.4"); thisProcess.recompile()})`.

## Usage

### Preparations

- Start SuperDirt: Open SuperCollider (or run `sclang` from a terminal) and run
(Ctrl+Enter) `SuperDirt.start`. This will load the audio engine and load its standard samples.
  - Alternatively, or in case of SuperCollider error messages regarding the buffer
  or late messages, run with the SuperDirt configuration file provided (it contains
  configuration and optimization options): `sclang superdirt_startup.scd`
- Open a terminal at the repository root (where _this_ file is located)
  - Run `cabal repl`.
- Sample names available for pattern functions match those of the `DirtSamples` quark installed by `SuperDirt`.
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

**Base implementation**

- [ ] Fix names for clear spec and avoid redundancy.
- [ ] Revisit euclidianZip in Rhythm Semigroup.
- [ ] Revisit actionT and other global transformations. Implement ghost events??
- [ ] Make sequencer state updates atomic (STM).
- [ ] Add rhythmic pattern field to sequencer pattern: allow recovering the pattern.
- [ ] Extend patterns beyond cycles (bundle sections might be here).
- [ ] Soloing many patterns together
– [ ] Fix MIDI functionality.
- [ ] Decouple OSC messages from SuperDirt (refine interface for customization).
- [ ] Currently timing comes from the Haskell runtime (Timed IO Monad). Get detailed timing using timestamps on OSC messages.
- [ ] Breakdown `Sequencer` module. Execution, PatterBundle, SequencerPattern and Output Values. Abstract over the _Fiber Bundle_ structure.
- [ ] Fix Nix installation.

- [ ] Have _signals_ for parameter control (for morphing)
- [ ] Implement tests for bjorklund / LH specifications
- [ ] Add `doctest`/`doctest-extract` for automatic in-documentation property testing (QuickCheck)

**Transformations and Patterns**

- [ ] Implement pattern actions and transformations at the cycle level.
- [ ] Global pattern operations and transformations
  - [x] Pattern action / product on fibers
  - [ ] Take advantage of mnng
  - [x] Add hand-fan feature
  - [x] Report balance and evenness.
- [ ] Implement pattern sync (projection) alternatives.
- [ ] A syntax to make new scheduled patterns affect current playing patterns
- [ ] Add perfectly balance rhythm library (no efficient algorithm on-sight)
- [ ] Implement well-formed rhythms (¿3 parameters?)
- [ ] Continuous morphing of well-formed rhythms using ratio parameter
- [ ] Geometrically informed continuous morphing between two arbitrary rhythms
- [ ] Elaborate graph of execution model
  - [x] PatternBundle

**Done**

- [x] Asynchronous evaluation of patterns
- [x] Make patterns addressable so they can be stopped and updated(¿in global state?)
- [x] Use HashMaps for sequences. (Now IntMap)
- [x] Add cps to sequencer
- [x] Make sequencer inform of current pattern length
- [x] Preserve Sequencer State between `ghci` reloads.
- [x] Fix haddocks (see Polygon module for example module header)
- [x] Fix pattern update timing (per cycle to avoid jumps)
- [x] Fix pattern resizing
- [x] Homogenize rhythmic pattern types show function. Each type should be tagged appropriately.
- [x] Fix implementation for simultaneous events.

## Further Work

- [ ] FRP implementations of patterns: make pattern composition scalable.
  - [ ] Tidal Cycles pattern representation
  - [ ] AFRP: Yampa, Euterpea MUI library
- [ ] Stand-alone interpreter (for `v.0.2.0.0`?)
  - [ ] Parsing: parsec/megaparsec
- [ ] Embed MIDI functionality using `fluidsynth`.
- [ ] Create a minimal sampler for RTG in SuperCollider.
