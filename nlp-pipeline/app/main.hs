module Main where

-- import Pipeline.Application.Run          (run)
import Pipeline.Application.RunBunch  (runPB)
-- import Pipeline.Application.WikiEL       (runEL)

main :: IO ()
main = runPB
