module Main where

import Data.Functor ((<&>))
import Data.List (intercalate, intersperse, isPrefixOf, nub)
import Debug.Trace (traceShowId)
import GHC.IO (unsafePerformIO)
import Text.ParserCombinators.ReadP (string)
import Text.Printf (printf)

type State = Int

type AlphabetElement = Char

data DFA = DFA
    { dfaTransitions :: [(State, State, AlphabetElement)]
    , dfaFinalStates :: [State]
    , dfaState :: State
    }
    deriving (Show, Eq)

data Regex
    = RegexNull
    | RegexEmpty
    | RegexString String
    | RegexAny Regex
    | RegexSome Regex
    | RegexOptional Regex
    | RegexOr Regex Regex
    | RegexConcat Regex Regex
    deriving (Show, Eq)

stringify :: Regex -> String
stringify s = k
  where
    k = stringify' s
    stringify' RegexNull = "∅"
    stringify' RegexEmpty = "ε"
    stringify' (RegexString a) = a
    stringify' (RegexSome a) = "(" ++ stringify' a ++ ")" ++ "+"
    stringify' (RegexAny (RegexString c)) = stringify' (RegexString c) ++ "*"
    stringify' (RegexAny r) = "(" ++ stringify' r ++ ")" ++ "*"
    stringify' (RegexOptional r) = "(" ++ stringify' r ++ ")" ++ "?"
    stringify' (RegexOr a b) = "(" ++ stringify' a ++ "|" ++ stringify' b ++ ")"
    stringify' (RegexConcat a b) = stringify' a ++ stringify' b

simplify :: Regex -> Regex
simplify (RegexSome (RegexOptional r)) = RegexAny (simplify r)
simplify (RegexAny RegexEmpty) = RegexEmpty
simplify (RegexAny (RegexOptional a)) = RegexAny (simplify a)
simplify (RegexAny RegexNull) = RegexEmpty
simplify (RegexAny (RegexAny a)) = RegexAny (simplify a)
simplify (RegexAny r) = RegexAny (simplify r)
simplify (RegexOptional RegexNull) = RegexEmpty
simplify (RegexOptional (RegexOptional a)) = RegexOptional (simplify a)
simplify (RegexOptional RegexEmpty) = RegexEmpty
simplify (RegexOptional (RegexAny a)) = RegexAny (simplify a)
simplify (RegexOptional (RegexSome r)) = RegexAny (simplify r)
simplify (RegexOptional r) = RegexOptional (simplify r)
simplify (RegexOr a b)
    | a == b = simplify a
simplify (RegexOr a (RegexConcat b c))
    | a == b = RegexConcat (simplify a) (RegexOptional (simplify c))
simplify (RegexOr RegexNull a) = simplify a
simplify (RegexOr a RegexNull) = simplify a
simplify (RegexOr RegexEmpty (RegexAny a)) = RegexAny (simplify a)
simplify (RegexOr a RegexEmpty) = RegexOptional (simplify a)
simplify (RegexOr RegexEmpty a) = RegexOptional (simplify a)
simplify (RegexOr a b) = RegexOr (simplify a) (simplify b)
simplify (RegexConcat RegexEmpty a) = simplify a
simplify (RegexConcat a RegexEmpty) = simplify a
simplify (RegexConcat RegexNull a) = RegexNull
simplify (RegexConcat a RegexNull) = RegexNull
simplify (RegexConcat (RegexConcat a (RegexAny (RegexConcat c d))) b)
    | RegexConcat d c == RegexConcat a b =
        RegexSome
            (RegexConcat (simplify a) (simplify b))
simplify (RegexConcat a b) = RegexConcat (simplify a) (simplify b)
simplify x = x

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

printMatrix :: [[Regex]] -> IO ()
printMatrix = mapM_ (putStrLn . (++) "\n" . concatMap ((++ "\t") . show))

takeFirstEqual :: (Eq a) => [a] -> a
takeFirstEqual [] = undefined
takeFirstEqual (x : xs)
    | x == head xs = x
    | otherwise = takeFirstEqual xs

regexify :: DFA -> Regex
regexify (DFA ts fs c) =
    takeFirstEqual . iterate simplify . foldl RegexOr RegexNull $ k
  where
    k = map ((\x y -> s !! x !! y) c) fs
    s = transitiveClosure a 0 (dfaToMatrix a)
    a = DFA ts fs c

main :: IO ()
main = do
    -- let autom = DFA [(0, 1, "a"), (1, 1, "b"), (1, 3, "c"), (0, 2, "c"), (2, 3, "b")] [3] 0
    -- let autom = DFA [(1, 1, 'b'), (1, 2, 'a'), (2, 1, 'a'), (2, 3, 'b'), (3, 3, 'c')] [2, 3] 0
    -- mapM_ ((putStrLn . (++) "\n" . concatMap (++ "\t")) . map (stringify . takeFirstEqual . iterate simplify)) . transitiveClosure autom 0 1 . dfaToMatrix $ autom
    -- let x = (transitiveClosure autom 0 3 . dfaToMatrix $ autom) !! 3 !! 3
    -- let autom =
    --         DFA
    --             [ (0, 1, 'a')
    --             , (1, 0, 'b')
    --             , (1, 2, 'a')
    --             , (2, 1, 'b')
    --             , (2, 0, 'a')
    --             , (0, 2, 'b')
    --             ]
    --             [0]
    --             0

    let autom =
            DFA
                [ (0, 1, 'a')
                , (1, 1, 'b')
                , (1, 3, 'c')
                , (0, 2, 'c')
                , (2, 3, 'b')
                ]
                [3]
                0

    -- let autom =
    --         DFA
    --             [ (0, 0, 'b')
    --             , (0, 1, 'a')
    --             , (1, 0, 'a')
    --             , (1, 2, 'b')
    --             , (2, 2, 'c')
    --             ]
    --             [1, 2]
    --             1

    putStrLn . stringify . regexify $ autom
