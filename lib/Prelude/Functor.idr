module Prelude.Functor

class Functor (f : Type -> Type) where 
    map : (a -> b) -> f a -> f b

class Contravariant (f : Type -> Type) where
    contramap : (a -> b) -> f b -> f a

