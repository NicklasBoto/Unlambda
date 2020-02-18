import Prelude hiding (Monoid)
import Arity

class Monoid a where
        (<+>) :: Binary  a
        addId :: Nullary a

data SMon c = Con c
            | Add (SMon c) (SMon c)
            | AddId

instance Show a => Show (SMon a) where
        show (Con c) = show c
        show (Add a b) = show a ++ "+" ++ show b
        show (AddId)   = "0"

instance Monoid (SMon c) where
        (<+>) = Add
        addId = AddId

instance Monoid Int where
        (<+>) = (+)
        addId = 0

eval :: Monoid a => SMon a -> a
eval (Con c)   = c
eval AddId     = addId
eval (Add a b) = (<+>) (eval a) (eval b)


