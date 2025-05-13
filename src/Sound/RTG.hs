-- |
-- Module      : RTG
-- Description : A rhythmic pattern live coding interface
-- Copyright   : (c) Xavier Góngora, 2025
-- License     : GPL-3
-- Maintainer  : ixbalanque@protonmail.ch
-- Stability   : experimental
module Sound.RTG
   ( -- * Sequencer Operations

    -- ** Play
    p,
    a,
    start,
    startAll,
    stop,
    stopAll,
    solo,
    unsolo,
    hush,

    -- ** Configure
    reset,
    resume,
    setcps,
    setbpm,
    kill,
    clear,

    -- ** Querie
    status,
    querie,
    active,
    idle,

    -- ** Transformations
    actionT,
    actionB,
    handFanT,
    handFanB,

    -- * Rhythmic Types

    -- ** Time Patterns
    diatonic,
    diminished,
    wholeTone,
    japanese,
    gypsy,
    shiko,
    clave,
    soukous,
    rumba,
    bossa,
    gahu,
    fiveBalance,
    amiotScale,

    -- ** Euclidean Rhythms
    e,

    -- * The Rhythmic Interface
    Rhythmic (..),
    rhythm,
  )
where

import Sound.RTG.Euclidean (e)
import Sound.RTG.RhythmicPattern (Rhythmic (..), rhythm)
import Sound.RTG.Sequencer
import Sound.RTG.TimePatterns
