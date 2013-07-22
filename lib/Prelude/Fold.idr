module Prelude.Fold

import Prelude.Algebra
import Prelude.Functor

class Functor t => Foldable (t : Type -> Type) where
  foldMap : Monoid m => (a -> m) -> t a -> m

fold : (Monoid m, Foldable t) => t m -> m
fold = foldMap id
