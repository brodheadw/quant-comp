module Qubit (
    Qubit(..),
    createQubit,
    createQubitFromMagPhase,
    ket0,
    ket1,
    normalize,
    toBloch,
    probabilities,
    showQubit,
    hadamard,
    fCNOT,
    pauliX,
    pauliY,
    pauliZ
) where

import Data.Complex

data Qubit = Qubit (Complex Double) (Complex Double)
    deriving (Eq, Show)

createQubit :: Double -> Double -> Double -> Double -> Qubit -- Create a qubit from real and imaginary parts
createQubit real0 imag0 real1 imag1 = normalize $ Qubit (real0 :+ imag0) (real1 :+ imag1)

ket0 :: Qubit
ket0 = createQubit 1 0 0 0

ket1 :: Qubit
ket1 = createQubit 0 0 1 0

normalize :: Qubit -> Qubit
normalize (Qubit a0 a1) =
    let norm = sqrt ((magnitude a0 ** 2) + (magnitude a1 ** 2))
    in Qubit (a0 / (norm :+ 0)) (a1 / (norm :+ 0))

toBloch :: Qubit -> (Double, Double)
toBloch (Qubit a0 a1) =
    let theta = 2 * acos (magnitude a0)
        phi = phase a1 - phase a0
    in (theta, phi)

probabilities :: Qubit -> (Double, Double)
probabilities (Qubit a0 a1) = (magnitude a0 ** 2, magnitude a1 ** 2)

showQubit :: Qubit -> String
showQubit (Qubit a0 a1) =
    "Qubit |ψ⟩ = " ++ showComplex a0 ++ " |0⟩ + " ++ showComplex a1 ++ " |1⟩"
    where
        showComplex :: Complex Double -> String
        showComplex z = let r = realPart z
                            i = imagPart z
                        in case (r, i) of
                            (_, 0) -> show r
                            (0, _) -> show i ++ "i"
                            (_, _) -> show r ++ (if i > 0 then " + " else " - ") ++ show (abs i) ++ "i"

-- Function to create a qubit from magnitude and phase
createQubitFromMagPhase :: (Double, Double) -> (Double, Double) -> Qubit
createQubitFromMagPhase (mag0, phase0) (mag1, phase1) =
    createQubit (mag0 * cos phase0) (mag0 * sin phase0)
                (mag1 * cos phase1) (mag1 * sin phase1)


{----- LOGIC GATES -----}

hadamard :: Qubit -> Qubit
hadamard (Qubit a0 a1) =
    let a0' = (a0 + a1) / (sqrt 2 :+ 0)
        a1' = (a0 - a1) / (sqrt 2 :+ 0)
    in normalize (Qubit a0' a1')

fCNOT :: Qubit -> Qubit -> (Qubit, Qubit)
fCNOT (Qubit a0 a1) targetQubit@(Qubit b0 b1) =
    if magnitude a1 > 0.5
        then (Qubit a0 a1, Qubit b1 b0)
        else (Qubit a0 a1, targetQubit)

pauliX :: Qubit -> Qubit
pauliX (Qubit a0 a1) = normalize (Qubit a1 a0)

pauliY :: Qubit -> Qubit
pauliY (Qubit a0 a1) = normalize (Qubit ((-imagPart a1) :+ realPart a1) (imagPart a0 :+ (-realPart a0)))

pauliZ :: Qubit -> Qubit
pauliZ (Qubit a0 a1) = normalize (Qubit a0 (-a1))