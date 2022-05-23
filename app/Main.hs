module Main where

import DFA
  ( DFA (DFA),
    regexify,
  )
import Regex
  ( Regex,
    stringify,
  )

autom1 :: DFA
autom1 =
  DFA
    [ (0, 1, 'a'),
      (1, 0, 'b'),
      (1, 2, 'a'),
      (2, 1, 'b'),
      (2, 0, 'a'),
      (0, 2, 'b')
    ]
    [0]
    0

autom2 :: DFA
autom2 =
  DFA
    [(0, 0, 'b'), (0, 1, 'a'), (1, 0, 'a'), (1, 2, 'b'), (2, 2, 'c')]
    [1, 2]
    1

autom3 :: DFA
autom3 =
  DFA [(0, 1, 'a'), (1, 1, 'b'), (1, 3, 'c'), (0, 2, 'c'), (2, 3, 'b')] [3] 0

main :: IO ()
main = do
  putStrLn . (++) "Automaton 1:\t" . stringify . regexify $ autom1
  putStrLn . (++) "Automaton 2:\t" . stringify . regexify $ autom2
  putStrLn . (++) "Automaton 3:\t" . stringify . regexify $ autom3
  pure ()
