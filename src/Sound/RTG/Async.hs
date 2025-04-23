-- |

module Sound.RTG.Async (Async, async, wait, waitEither, tryWait) where

import Control.Concurrent.Async (Async, async, wait, waitEither, pollSTM)
import Control.Concurrent.STM (atomically)

-- | A non-blocking version of 'wait'.
tryWait = atomically . fmap forgetLeft . pollSTM

forgetLeft :: Maybe (Either a b) -> Maybe b
forgetLeft Nothing = Nothing
forgetLeft (Just (Left _)) = Nothing
forgetLeft (Just (Right b)) = Just b
