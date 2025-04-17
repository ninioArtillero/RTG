# Module Structure Refactor

The module structure will stop to use ad-hoc divisions by Rhythm, Time and Geometry.
I've move all modules to the top level, in order to reorganize, refactor and clean existing code
conveniently.
This decision has been motivated by the desire to attain _organic grow_ and _habitability_,
as accounted for in "Patterns of Software" by Richard Gabriel, for the library.
I've come to realize that to much focus on a big design master plan was hindering my ability
to implement the needed functionality and to objectively assess what is superficial.

## TODO

+ [x] Flatten structure
+ [x] Format with ormolu
+ [x] Avoid unnecessary dependencies
+ [x] Add missing module metadata
+ [ ] Implement sequencer functionality
+ [ ] Implement global transformations
+ [ ] Remove/hide unnecesary modules.
+ [ ] Redesign API at library entry point (RTG.hs)

## Module List

The module structure before the refactor with a summary of content.

### Rhythm

* Bjorklund: The Bjorklund algorithm.
* PerfectBalance: Calculation of balance and evenness.
* RatioDecons: Deprecated module (remove).
* RhythmicPattern: Main pattern interface. Very tangled: contains instances form external types.
Many functions to extract structure properties.
* TimePatterns: TimePattern type and collection of predefined time patterns.
* Zip: Alternative zip operations on lists (for Monoid operations).

### Time

* OscMessages: Types (alias), OSC messages and ports.
* Sequencer: Pattern sequencer. Global state and reference to existing patterns.
Decide on effect system strategy: IORefs o STA (TimedMonad).
* TemporalMonad: Overlap with TimedMonad. **Article transcription**.
* TimedMonad: Implementacion de Timed Monad (para Refs). **Article transcription**.
* UnSafe. A bit of everything for messages. Types (aliases), unnecessary dependencies.

### Geometry

* Euclidean : Euclidean rhythms. Types and instances.
* Polygon : Irreducibly periodic and perfectly balanced rhythm. Types, instances and functions

There is overlap between both modules. Polygon is too comples.

### Internal

* List: Lists utility functions: List's utility functions
* Utils: More utils, mainly for Rationals (few functions).

### TopLevel

* HotSwap: Un implemented, using Midair library (niche library). Related to sequencer
* ReactivePattern: Yampa test module. Related to sequencer.
* TiledMusic: Refined tiled product using Euterpea. **Article transcription**.
* TiledStream:T-Calculus basen implementation for tiled product. **Article implementation**.
