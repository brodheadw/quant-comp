module Main (main) where

import Qubit

main :: IO ()
main = do
    {-let qubits :: [Qubit]
        qubits = [ createQubitFromMagPhase (sqrt 2, 0) (sqrt 2, 0),  -- a)
                   createQubitFromMagPhase (1/2, 0) (sqrt 3 / 2, 0), -- b)
                   createQubitFromMagPhase (1/sqrt 2, 0) (1/sqrt 2, pi),  -- c) (note the phase of pi for the imaginary part)
                   createQubitFromMagPhase (1/4, 0) (sqrt 15 / 4, pi/2)  -- d) (phase of pi/2 for the imaginary part)
                 ]

    mapM_ processQubit qubits-}

    let qubit1 = ket0
    let qubit2 = pauliX ket1
    deutschAlgorithm qubit1 qubit2


{-processQubit :: Qubit -> IO ()
processQubit q = do
    let normalizedQubit = normalize q
    let (theta, phi) = toBloch normalizedQubit
    let (prob0, prob1) = probabilities normalizedQubit
    putStrLn $ showQubit normalizedQubit
    putStrLn $ "Bloch Sphere Coordinates: θ = " ++ show theta ++ ", φ = " ++ show phi
    putStrLn $ "Probability |0⟩: " ++ show prob0 ++ ", Probability |1⟩: " ++ show prob1
    putStrLn ""-}

{----- HOMEWORK 2 -----}

deutschAlgorithm :: Qubit -> Qubit -> IO ()
deutschAlgorithm q1 q2 = do
    let q1' = hadamard q1
    let q2' = hadamard q2
    let (q1'', q2'') = fCNOT q1' q2'
    let q1_final = hadamard q1''
    putStrLn $ "Final state of first qubit: " ++ showQubit q1_final