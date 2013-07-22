module Control.Monad.Identity

import Prelude.Functor
import Prelude.Fold
import Prelude.Traversal
import Prelude.Applicative
import Prelude.Monad 

public record Identity : Type -> Type where
    Id : (runIdentity : a) -> Identity a

instance Functor Identity where
    map fn (Id a) = Id (fn a)

instance Foldable Identity where
    foldMap f (Id n) = f n

instance Traversable Identity where
    traverse f (Id n) = map Id (f n)

instance Applicative Identity where
    pure x = Id x
    
    (Id f) <$> (Id g) = Id (f g)

instance Monad Identity where
    (Id x) >>= k = k x
