# haskell-automata

Haskell library for working with automata, converting DFAs to and simplifying Regular Expressions

## Example
Consider the automaton A = (Q, Σ, δ, q₀, F) where

>Q = {0, 1, 2, 3} \
Σ = {'a', 'b', 'c'} \
δ (0, 1) = 'a' \
δ (1, 1) = 'b' \
δ (1, 2) = 'c' \
δ (0, 2) = 'c' \
δ (2, 3) = 'b' \
q₀ = 0 \
F = {3}

\
![automaton A](dfa.png)

With the DFA type being defined as

```haskell
data DFA = DFA
    { dfaTransitions :: [(State, State, AlphabetElement)]
    , dfaFinalStates :: [State]
    , dfaState :: State
    }
    deriving (Show, Eq)
```

automaton A can be represented as
```haskell
autom :: DFA
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
```

calling `regexify` on `autom` will result in the already simplified corresponding regular expression:

> `(ab*c|cb)`

\
This is achieved by applying the [transitive closure method](https://cs.stackexchange.com/a/2395) to convert DFAs to regular expressions and then simplifying the expression with defined rules. This will not always result in the shortest possible expression but it will definitely be equivalent.

Currently **only** DFAs are supported, which also means that δ has to be total, so there cannot be multiple transitions between two states.