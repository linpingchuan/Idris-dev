module Prelude.Either

import Builtins

import Prelude.Functor
import Prelude.Fold
import Prelude.Applicative
import Prelude.Monad
import Prelude.Maybe
import Prelude.List

data Either a b
  = Left a
  | Right b

--------------------------------------------------------------------------------
-- Syntactic tests
--------------------------------------------------------------------------------

isLeft : Either a b -> Bool
isLeft (Left l)  = True
isLeft (Right r) = False

isRight : Either a b -> Bool
isRight (Left l)  = False
isRight (Right r) = True

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

choose : (b : Bool) -> Either (so b) (so (not b))
choose True  = Left oh
choose False = Right oh

either : Either a b -> (a -> c) -> (b -> c) -> c
either (Left x)  l r = l x
either (Right x) l r = r x

lefts : List (Either a b) -> List a
lefts []      = []
lefts (x::xs) =
  case x of
    Left  l => l :: lefts xs
    Right r => lefts xs

rights : List (Either a b) -> List b
rights []      = []
rights (x::xs) =
  case x of
    Left  l => rights xs
    Right r => r :: rights xs

partitionEithers : List (Either a b) -> (List a, List b)
partitionEithers l = (lefts l, rights l)
    
fromEither : Either a a -> a
fromEither (Left l)  = l
fromEither (Right r) = r

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

maybeToEither : e -> Maybe a -> Either e a
maybeToEither def (Just j) = Right j
maybeToEither def Nothing  = Left  def

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor (Either e) where
  map f (Left l) = Left l
  map f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure = Right
  (Left a) <$> _          = Left a
  (Right f) <$> (Right r) = Right (f r)
  (Right _) <$> (Left l)  = Left l

instance Monad (Either e) where
    (Left n) >>= _ = Left n
    (Right r) >>= f = f r

--------------------------------------------------------------------------------
-- Injectivity of constructors
--------------------------------------------------------------------------------

total leftInjective : {b : Type} -> {x : a} -> {y : a}
                    -> (Left {b = b} x = Left {b = b} y) -> (x = y)
leftInjective refl = refl

total rightInjective : {a : Type} -> {x : b} -> {y : b}
                     -> (Right {a = a} x = Right {a = a} y) -> (x = y)
rightInjective refl = refl
