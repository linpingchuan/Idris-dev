module Prelude.Maybe

import Builtins
import Prelude.Algebra
import Prelude.Cast
import Prelude.Fold
import Prelude.Traversal
import Prelude.Monad

%access public
%default total

data Maybe a
    = Nothing
    | Just a

--------------------------------------------------------------------------------
-- Syntactic tests
--------------------------------------------------------------------------------

isNothing : Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just j) = False

isJust : Maybe a -> Bool
isJust Nothing  = False
isJust (Just j) = True

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

maybe : |(def : b) -> (a -> b) -> Maybe a -> b
maybe n j Nothing  = n
maybe n j (Just x) = j x

fromMaybe : |(def: a) -> Maybe a -> a
fromMaybe def Nothing  = def
fromMaybe def (Just j) = j

toMaybe : Bool -> a -> Maybe a
toMaybe True  j = Just j
toMaybe False j = Nothing

justInjective : {x : t} -> {y : t} -> (Just x = Just y) -> x = y
justInjective refl = refl

raiseToMaybe : (Monoid a, Eq a) => a -> Maybe a
raiseToMaybe x = if x == neutral then Nothing else Just x

--------------------------------------------------------------------------------
-- Class instances
--------------------------------------------------------------------------------

instance (Eq a) => Eq (Maybe a) where
  Nothing  == Nothing  = True
  Nothing  == (Just _) = False
  (Just _) == Nothing  = False
  (Just a) == (Just b) = a == b

instance Functor Maybe where 
  map f (Just x) = Just (f x)
  map f Nothing  = Nothing

instance Foldable Maybe where
  foldMap _ Nothing = neutral
  foldMap f (Just x) = f x

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = map Just (f x)

instance Applicative Maybe where
  pure = Just
  (Just f) <$> (Just a) = Just (f a)
  _        <$> _        = Nothing

instance Alternative Maybe where
  empty = Nothing
  (Just x) <|> _ = Just x
  Nothing  <|> v = v

instance Monad Maybe where 
  Nothing  >>= k = Nothing
  (Just x) >>= k = k x

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: "Any semigroup S may be
-- turned into a monoid simply by adjoining an element i not in S
-- and defining i+i = i and i+s = s = s+i for all s in S."

instance (Semigroup a) => Semigroup (Maybe a) where
  Nothing <+> m = m
  m <+> Nothing = m
  (Just m1) <+> (Just m2) = Just (m1 <+> m2)

instance (Semigroup a) => Monoid (Maybe a) where
  neutral = Nothing

instance (Monoid a, Eq a) => Cast a (Maybe a) where
  cast = raiseToMaybe

instance (Monoid a) => Cast (Maybe a) a where
  cast = foldMap id
