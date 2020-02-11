----------- imports -------------------

import Prelude hiding ((<*>)) 
import Data.Maybe

---------- ring typeclass -------------

class Ring a where
        mulId :: Nullary a
        addId :: Nullary a
        (<*>) :: Binary  a
        (<+>) :: Binary  a
        neg   :: Unary   a

--------- RingExp datatype ---------

data RingExp c v = Con c
                 | Var v
                 | Add (RingExp c v) (RingExp c v)
                 | Mul (RingExp c v) (RingExp c v)
                 | Neg (RingExp c v)
                 | MulId
                 | AddId

instance Eq v => Ring (RingExp c v) where
        mulId = MulId
        addId = AddId
        (<*>) = Mul 
        (<+>) = Add
        neg   = Neg

------- Integers mod 2 datatype -------

data Mod2 = Zero | One deriving (Show, Eq)

instance Ring Mod2 where
        mulId = One
        addId = Zero
        (<*>) = modMul
        (<+>) = modAdd
        neg a = a

modAdd :: Binary Mod2
modAdd a Zero = a
modAdd Zero b = b
modAdd _    _ = Zero

modMul :: Binary Mod2
modMul One One = One
modMul _   _   = Zero

-- add one more ring instance datatype

----------- eval function ---------------

type Env var dom = [(var, dom)]

eval :: (Ring a, Eq v) => Env v a -> RingExp a v -> a 
eval _ (Con c)   = c
eval t (Var v)   = fromJust $ lookup v t
eval t (Add a b) = (eval t a) <+> (eval t b)
eval t (Mul a b) = (eval t a) <*> (eval t b)
eval t (Neg a)   = neg (eval t a)
eval _ MulId     = mulId
eval _ AddId     = addId

--------- testing ------------------

tabell :: Env Char Mod2
tabell = [('x', One), ('y', Zero), ('z', One)]

expr1 :: RingExp Mod2 Char
expr1 = Add (Var 'x') (Con Zero) -- returns One

expr2 :: RingExp Mod2 Char
expr2 = Mul (Var 'y') (MulId)    -- returns Zero 

expr3 :: RingExp Mod2 Char       
expr3 = Neg (Con One)            -- returns One

-- Both of always return true, good

testMul :: Mod2 -> Bool
testMul a = (eval tabell left) == (eval tabell right)
    where left  = (Mul (Con a) MulId)
          right = (Mul MulId (Con a))

testAdd :: Mod2 -> Bool
testAdd a = (eval tabell left) == (eval tabell right)
    where left  = (Add (Con a) AddId)
          right = (Add AddId (Con a))
