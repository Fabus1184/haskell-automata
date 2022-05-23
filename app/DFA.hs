module DFA where

import Data.List (nub)
import Misc (takeFirstEqual)
import Regex
  ( Regex (RegexAny, RegexConcat, RegexEmpty, RegexNull, RegexOr, RegexString),
    simplify,
  )

type State = Int

type AlphabetElement = Char

data DFA = DFA
  { dfaTransitions :: [(State, State, AlphabetElement)],
    dfaFinalStates :: [State],
    dfaState :: State
  }
  deriving (Show, Eq)

evaluate :: DFA -> [AlphabetElement] -> (Maybe State, [AlphabetElement])
evaluate (DFA ts fs s) input
  | null l = (Nothing, input)
  | next `elem` fs && consumed == head input = (Just next, left)
  | otherwise = evaluate (DFA ts fs next) left
  where
    (current, next, consumed) = head l
    l = filter (\(x, y, z) -> x == s && z == head input) ts
    left = tail input

states :: DFA -> [State]
states (DFA ts _ _) = nub . concatMap (\(x, y, z) -> [x, y]) $ ts

dfaToMatrix :: DFA -> [[Regex]]
dfaToMatrix (DFA ts _ _) =
  [ [ ( \l -> case () of
          _
            | not $ null l -> RegexString [(\(a, b, c) -> c) (head l)]
            | x == y -> RegexEmpty
            | otherwise -> RegexNull
      )
        $ filter (\(f, t, c) -> f == x && t == y) ts
      | y <- [0 .. s]
    ]
    | x <- [0 .. s]
  ]
  where
    s = foldl max 0 $ states (DFA ts [] 0)

transitiveClosure :: DFA -> Int -> [[Regex]] -> [[Regex]]
transitiveClosure (DFA ts _ _) _ [] = []
transitiveClosure a 0 r = transitiveClosure a 1 . dfaToMatrix $ a
transitiveClosure (DFA ts _ _) k' r
  | k' == maximum (states a) + 1 = r'
  | otherwise = transitiveClosure a (k' + 1) r'
  where
    k = k' - 1
    r' =
      case () of
        _
          | k `elem` map (\(x, y, z) -> y) ts -> [[RegexOr (r !! i !! j) (inner i j) | j <- [0 .. s]] | i <- [0 .. s]]
          | otherwise -> r

    inner = \i j ->
      let a = r !! i !! k
          b = r !! k !! k
          c = r !! k !! j
       in RegexConcat (RegexConcat a (RegexAny b)) c

    s = maximum (states a)

    a = DFA ts [] 0

regexify :: DFA -> Regex
regexify (DFA ts fs c) =
  takeFirstEqual . iterate simplify . foldl RegexOr RegexNull $ k
  where
    k = map ((\x y -> s !! x !! y) c) fs
    s = transitiveClosure a 0 (dfaToMatrix a)
    a = DFA ts fs c
