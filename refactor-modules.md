# Module list

The current module structure with a summary of content for refactoring.

+ [ ] Flatten structure and simplify API definition.
+ [ ] Implement sequencer functionality
+ [ ] Implement global transformations
+ [ ] Remove/hide unnecesary modules.


## Rhythm

* Bjorklund: The Bjorklund algorithm.
* PerfectBalance: Calculation of balance and evenness.
* RatioDecons: Deprecated module (remove).
* RhythmicPattern: Main pattern interface. Very tangled: contains instances form external types.
Many functions to extract structure properties.
* TimePatterns: TimePattern type and collection of predefined time patterns.
* Zip: Alternative zip operations on lists (for Monoid operations).

## Time

* OscMessages: Types (alias), OSC messages and ports.
* Sequencer: Pattern sequencer. Global state and reference to existing patterns.
Decide on effect system strategy: IORefs o STA (TimedMonad).
* TemporalMonad: Overlap with TimedMonad. **Article transcription**.
* TimedMonad: Implementacion de Timed Monad (para Refs). **Article transcription**.
* UnSafe. A bit of everything for messages. Types (aliases), unnecessary dependencies.

Geometry

* Euclidean : Euclidean rhythms. Types and instances.
* Polygon : Irreducibly periodic and perfectly balanced rhythm. Types, instances and functions

Hay traslape entre los tipos de ambos modulos. Polygon es más complejo.

Internal

* List: Lists utility functions: List's utility functions
* Utils: More utils, mainly for Rationals (few functions).

TopLevel

* HotSwap: Un implemented, using Midair library (niche library). Related to sequencer
* ReactivePattern: Yampa test module. Related to sequencer.
* TiledMusic: Refined tiled product using Euterpea. **Article transcription**.
* TiledStream:T-Calculus basen implementation for tiled product. **Article implementation**. 

