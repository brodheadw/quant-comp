module Qubit (
    Qubit,
    normalize,
    toBloch,
    probabilities,
    showQubit
) where

import Data.Complex

-- Define a Qubit data type
data Qubit = Qubit (Complex Double) (Complex Double)

-- Normalize a qubit state
normalize :: Qubit -> Qubit
normalize (Qubit a0 a1) =
    let norm = sqrt ((magnitude a0 ** 2) + (magnitude a1 ** 2))
    in Qubit (a0 / (norm :+ 0)) (a1 / (norm :+ 0))

-- Convert to Bloch sphere coordinates
toBloch :: Qubit -> (Double, Double)
toBloch (Qubit a0 a1) =
    let theta = 2 * acos (magnitude a0)
        phi = phase a1 - phase a0
    in (theta, phi)

-- Calculate the probabilities of measuring 0 and 1
probabilities :: Qubit -> (Double, Double)
probabilities (Qubit a0 a1) = (magnitude a0 ** 2, magnitude a1 ** 2)

-- Show a qubit state
showQubit :: Qubit -> String
showQubit (Qubit a0 a1) =
    "Qubit |ψ⟩ = " ++ show a0 ++ " |0⟩ + " ++ show a1 ++ " |1⟩"