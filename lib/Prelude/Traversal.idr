module Prelude.Traversal

import Builtins
import Prelude.Applicative

%access public
%default total

class Traversable (s : Type -> Type) where
  traverse : Applicative f => (a -> f b) -> s a -> f (s b)
  traverse_ : Applicative f => (a -> f b) -> s a -> f ()
  traverse_ f t = traverse f t $> pure ()

sequence : (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence = traverse id

sequence_ : (Traversable t, Applicative f) => t (f a) -> f ()
sequence_ = traverse_ id
