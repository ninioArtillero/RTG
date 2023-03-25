-- Ritmos euclideanos utilizando un redondeo modular
-- Esta aproximación es una adaptación del trabajo final con Mauricio Rodriguez

patronRegular :: Int -> Int -> [Int]
patronRegular pulses events = let p' = fromIntegral pulses :: Rational
                                  e' = fromIntegral events :: Rational
                                  step = e' / p'
                                  stepList = map round [0,step..]
                              in takeWhile (<= events) stepList


rotation :: Int -> Int -> [Int] -> [Int]
rotation amount events list = map ((`mod` events). (+ amount)) list
