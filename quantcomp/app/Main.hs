module Main (main) where

import Qubit
import Data.Complex

main :: IO ()
main = do
    -- Define the qubit states with explicit type annotation
    let qubits :: [Qubit]  -- Explicit type annotation
        qubits = [ Qubit (sqrt 2 :+ 0) (sqrt 2 :+ 0),      -- a)
                   Qubit (0.5 :+ 0) ((sqrt 3 / 2) :+ 0),   -- b)
                   Qubit ((1 / sqrt 2) :+ 0) (0 :+ (-1 / sqrt 2)), -- c)
                   Qubit (0.25 :+ 0) (0 :+ (sqrt 15 / 4))  -- d)
                 ]

    -- Process each qubit
    mapM_ processQubit qubits

processQubit :: Qubit -> IO ()
processQubit q = do
    let normalizedQubit = normalize q
    let (theta, phi) = toBloch normalizedQubit
    let (prob0, prob1) = probabilities normalizedQubit
    putStrLn $ showQubit normalizedQubit
    putStrLn $ "Bloch Sphere Coordinates: θ = " ++ show theta ++ ", φ = " ++ show phi
    putStrLn $ "Probability |0⟩: " ++ show prob0 ++ ", Probability |1⟩: " ++ show prob1
    putStrLn ""