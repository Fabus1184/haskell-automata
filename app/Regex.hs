module Regex where

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
