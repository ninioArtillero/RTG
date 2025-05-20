# Rhythm, Time and Geometry

RTG is a music EDSL for the creation and manipulation
of rhythmic patterns implementing a geometrically informed design to explore:

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
> This library is in a _experimental_ state.

Watch some
[vide demos](https://www.youtube.com/playlist?list=PLyQs-cuHecOif7pDYLvRmrbOyjT0DGPay)!

## Related Papers

- [Rhythm, Time and Geometry](https://doi.org/10.21428/108765d1.e65cd604) @ Algorithmic Pattern Salon (2023)
- [Demo: A Geometric Approach to Generate Musical Rhythmic Patterns in Haskell](https://doi.org/10.1145/3677996.3678295) @ FARM (2024)

## Installation

### Haskell tool-chain

1. [Install GHCup](https://www.haskell.org/ghcup/) —the recommended tool
   to administer the Haskell tool-chain— to get `cabal` and `ghc`. You may select all
   the default options of the installation script.
1. Run `ghcup tui` and install (`i`) GHC 9.10.1 and _set it_ (with `s`) afterwards.
1. Clone this repository.
1. Run `cabal build` from the root of the repository.
   In case of error, please [open an issue](https://github.com/ninioArtillero/ritmoTG/issues/new/choose) with the output.
1. Follow the [usage](#usage) instructions below to use RTG.

### Audio dependencies

1. Install SuperDirt (audio engine):
   1. Install [SuperCollider](https://supercollider.github.io/downloads.html).
   1. In the SuperCollider IDE (or in a terminal at the `sclang` shell prompt) run:
      `Quarks.checkForUpdates({Quarks.install("SuperDirt", "v1.7.4"); thisProcess.recompile()})`.

## Usage

### Setup

- Start SuperDirt: Open SuperCollider (or run `sclang` from a terminal) and run
  (Ctrl+Enter) `SuperDirt.start`. This will load the audio engine and load its standard samples.
- Open a terminal at the repository root (where _this_ file is located)
  - Run `cabal repl`.
- Sample names available for pattern functions match those of the `DirtSamples` quark installed by `SuperDirt`.
  A list of them can be printed by running `~dirt.postSampleInfo;` in SuperCollider.

### Mini tutorial

You can execute a pattern like so:

```haskell
p :: Rhythmic a => PatternID -> [SampleName] -> a -> IO PatternBundle

p 1 ["cp", "bd"] $ clave
```

> [!WARNING]
> To recover silence, use the command `hush`.
> If it fails, quit the interpreter using `:q`.
> Go with care, with current behavior long samples can easily blow things up.

The samples in the list are played simultaneously following the pattern of the given
rhythmic value. The library of patterns can be queried with the `patternLibrary`
command.

Alternatively, the operator `a` has the same structure, but assigns the samples in
a sequential fashion to the _onset_ events of the pattern.

Both functions assign a pattern to an index (an integer value aliased as `PatternID`) in a mapping called a
`PatternBundle`, and upon first invocation they trigger the underlying sequencer.

New patterns are created by combination and transformation. Combination is
accomplished using the _semigroup_ operator `<>`.

```haskell
a 2 ["sn", "blip", "can"] $ bossa <> amiotScale
```

Patterns can be transformed applying functions from the `Rhythmic` interface.

```haskell
p 1 ["cp", "bd"] $ reflex $ co clave
```

The available transformations are: `rotate n` (rotation by `n` pulses) , `reflex`
(center reflection), `rev` (reverse) and `co` (complement). You can also use
[euclidean rhythms](https://www-cgrl.cs.mcgill.ca/~godfried/teaching/dm-reading-assignments/Euclidean-Rhythms.pdf)
(another type of rhythmic value) and _combine_ them with other patterns using the `&` operator.

```haskell
p 3 ["rm"] $ shiko & (e(3,8) <> e(7,12))
```

All timing is managed within the interpreter and there is no buffering (yet),
so SuperDirt late messages are expected.

The sequencer gives continuous feedback on its current state, and its
_output pattern_ can grow quite large depending on pattern combinations.
Also, there are three playing modes (triggered by some sequencer operations):

- `Global`: all the patterns in the bundle are merged into the output
- `Solo`: a pattern is selected as output
- `Transform`: the sequencer stops reading the bundle to allow transformations
  on its current output.

A pattern can be soloed by index (`solo 3`), and `unsolo`ed to
go back to the _global_ pattern.

One of the design goals is leveraging combination and transformation at the _bundle_
level.

You can _apply a pattern_ to the global output pattern.

```haskell
actionT $ fiveBalance
```

Entering into `Transform` mode, and go back to the `Global` pattern with.

```haskell
resume
```

More experimental is the bundle transformation, which applies the given pattern
to all elements of the bundle before merging them on the output.

```haskell
actionB amitoScale
```

At the moment, this transformation modifies the state for good, so to recover you'll
need to execute the patterns you had again.

You can `clear` all patterns from the bundle and make then inactive/active using
`stop i`/`start i` (where `i` is the pattern index).

## Development

> [!NOTE]
> The project is being developed with GHC 9.10.1 using regular `cabal` commands.
> The `run.sh` script and the nix-shell itself are intended for end users, but
> is currently down.

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

- [ ] Fix names for clear spec and avoid redundancy
- [ ] Revisit euclidianZip in Rhythm Semigroup
- [ ] Revisit actionT and other global transformations. Implement ghost events?
- [ ] Make sequencer state updates atomic (STM)
- [ ] Add rhythmic pattern field to sequencer pattern: allow recovering the pattern
- [ ] Extend patterns beyond cycles (bundle sections might be here)
- [ ] Soloing many patterns together
- [ ] Fix MIDI functionality
- [ ] Decouple OSC messages from SuperDirt (refine interface for customization)
- [ ] Currently timing comes from the Haskell runtime (Timed IO Monad). Get detailed timing using timestamps on OSC messages
- [ ] Breakdown `Sequencer` module. Execution, PatterBundle, SequencerPattern and Output Values. Abstract over the _Fiber Bundle_ structure
- [ ] Fix Nix installation
- [ ] Have _signals_ for parameter control (for morphing)
- [ ] Implement tests for bjorklund / LH specifications
- [ ] Add `doctest`/`doctest-extract` for automatic in-documentation property testing (QuickCheck)

**Transformations and Patterns**

- [ ] Implement pattern actions and transformations at the cycle level
- [ ] Global pattern operations and transformations
  - [x] Pattern action / product on fibers
  - [ ] Take advantage of mnng
  - [x] Add hand-fan feature
  - [x] Report balance and evenness
- [ ] Implement pattern sync (projection) alternatives
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
