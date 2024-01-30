module Qubit (
    Qubit(..),
    normalize,
    toBloch,
    probabilities,
    showQubit,
    hadamard
) where

import Data.Complex

-- Define a Qubit data type
data Qubit = Qubit (Complex Double) (Complex Double)
    deriving (Eq, Show)

-- Normalize a qubit state
normalize :: Qubit -> Qubit
normalize (Qubit a0 a1) =
    let norm = sqrt ((magnitude a0 ** 2) + (magnitude a1 ** 2))
    in Qubit (a0 / (norm :+ 0)) (a1 / (norm :+ 0))

-- Convert to Bloch sphere coordinatesc
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


-- LOGIC GATES

-- Hadamard Gate
hadamard :: Qubit -> Qubit
hadamard (Qubit a0 a1) =
    let a0' = (a0 + a1) / (sqrt 2 :+ 0)
        a1' = (a0 - a1) / (sqrt 2 :+ 0)
    in normalize (Qubit a0' a1')

-- CNOT Gate
cnot :: Qubit -> Qubit -> (Qubit, Qubit)
cnot (Qubit a0 a1) targetQubit@(Qubit b0 b1) =
    if magnitude a1 > 0.5
        then (Qubit a0 a1, Qubit b1 b0)
        else (Qubit a0 a1, targetQubit)

-- Pauli-X Gate
pauliX :: Qubit -> Qubit
pauliX (Qubit a0 a1) = normalize (Qubit a1 a0)

-- Pauli-Y Gate
pauliY :: Qubit -> Qubit
pauliY (Qubit a0 a1) = normalize (Qubit ((-imagPart a1) :+ realPart a1) (imagPart a0 :+ (-realPart a0)))

-- Pauli-Z Gate
pauliZ :: Qubit -> Qubit
pauliZ (Qubit a0 a1) = normalize (Qubit a0 (-a1))