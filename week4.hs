----------- imports -------------------

import Prelude hiding ((<*>)) 
import Data.Maybe
import Arity

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

--------------- 2x2 Matrix datatype -----------

data Matrix a = Matrix a a a a -- 2x2

-- doesn't work... but almost
instance Show a => Show (Matrix a) where
        show (Matrix a b c d) = 
              "┌ " ++ top ++ " ┐\n" ++
              "└ " ++ bot ++ " ┘"
                where topChar = show a ++ " " ++ show b 
                      botChar = show c ++ " " ++ show d
                      topLen  = length topChar
                      botLen  = length botChar
                      top     = topChar ++ (unwords $ replicate (botLen-topLen) " ")
                      bot     = botChar ++ (unwords $ replicate (topLen-botLen) " ")
                 

instance Num a => Ring (Matrix a) where
        mulId = Matrix 1 0 0 1
        addId = Matrix 0 0 0 0
        (<*>) = matrixMul
        (<+>) = matrixAdd
        neg   = fmap negate

instance Functor Matrix where -- unnecessary, but nonetheless
        fmap f (Matrix a b c d) = Matrix (f a) (f b) (f c) (f d)

matrixAdd :: Num a => Binary (Matrix a)
matrixAdd (Matrix a b c d) (Matrix e f g h) = 
           Matrix (a+e) (b+f) (c+g) (d+h)

matrixMul :: Num a => Binary (Matrix a)
matrixMul (Matrix a b c d) (Matrix e f g h) =
           Matrix (a*e + b*g) (a*f + b*h)
                  (c*e + d*g) (c*f + d*h)

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

modTable :: Env Char Mod2
modTable = [('x', One), ('y', Zero), ('z', One)]

matrixTable :: Env Char (Matrix Int)
matrixTable = [('x', Matrix 1 0 0 1), ('y', Matrix 0 0 0 0), ('z', Matrix 1 2 3 4)]

expr1 :: Ring a => a -> RingExp a Char
expr1 r = Add (Var 'x') (Con r) -- returns One

expr2 :: RingExp Mod2 Char
expr2 = Mul (Var 'y') (MulId)    -- returns Zero 

expr3 :: RingExp Mod2 Char       
expr3 = Neg (Con One)            -- returns One
-- x + (neg x) (mod 2) = addId -> 1 + (neg 1) (mod 2) = 0 -> neg 1 = 1
-- x + (neg x) (mod 2) = addId -> 0 + (neg 0) (mod 2) = 0 -> neg 0 = 0

-- Both of always return true, good

testMul :: Mod2 -> Bool
testMul a = (eval modTable left) == (eval modTable right)
    where left  = (Mul (Con a) MulId)
          right = (Mul MulId (Con a))

testAdd :: Mod2 -> Bool
testAdd a = (eval modTable left) == (eval modTable right)
    where left  = (Add (Con a) AddId)
          right = (Add AddId (Con a))



