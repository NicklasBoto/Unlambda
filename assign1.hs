import Data.List

-------------------- type declarations ---------------

data SET v = Empty
           | Singleton (SET v) 
           | Union  (SET v) (SET v)
           | Insec  (SET v) (SET v)
           | Var v

data PRED v = Elem    (SET v) (SET v)
            | Subset  (SET v) (SET v)
            | And     (PRED v) (PRED v)
            | Or      (PRED v) (PRED v) 
            | Implies (PRED v) (PRED v)
            | Not     (PRED v)

newtype Set = S [Set] deriving Eq

instance Show Set where
    show (S a) = show $ length a

instance (Show v, Eq v) => Show (SET v) where
        show Empty = "Ø"
        show (Singleton s) = "{" ++ show s ++ "}"
        show (Union a b)   = show a ++ " ⋃ " ++ show b
        show (Insec a b)   = show a ++ " ⋂ " ++ show b
        show (Var v)       = show v

instance (Show v, Eq v) => Show (PRED v) where
        show (Elem a b)    = show a ++ " ∈ "  ++ show b
        show (Subset a b)  = show a ++ " ⊆ "  ++ show b
        show (And p q)     = show p ++ " ^ "  ++ show q
        show (Or p q)      = show p ++ " ∨ "  ++ show q
        show (Implies p q) = show p ++ " => " ++ show q
        show (Not p)       = "¬ (" ++ show p ++ ")"

instance Ord Set where -- this Ord only holds true for von Neumann ordinals
        compare (S a) (S b) = compare (length a) (length b)

instance Eq Set where
        (==) (S a) (S b) = (sort a) == (sort b)

instance Eq v => Eq (SET v) where
        (==) s1 s2 = check [] $ And (Subset s1 s2) (Subset s2 s1)

instance Eq v => Ord (SET v) where
        compare s1 s2
            | s1 == s2              = EQ
            | check [] $ Elem s1 s2 = LT
            | otherwise             = GT

type Env var dom = [(var, dom)]

------------------- eval functions -----------------------

eval :: Eq v => Env v Set -> SET v -> Set
eval table Empty         = S []
eval table (Singleton s) = S [eval table s]
eval table (Union a b)   = unionS (eval table a) (eval table b)
eval table (Insec a b)   = insecS (eval table a) (eval table b)
eval table (Var v)       = getVar table v

unionS :: Set -> Set -> Set
unionS (S a) (S b) = S (union a b)

insecS :: Set -> Set -> Set
insecS (S a) (S b) = S (intersect a b)

getVar :: Eq v => Env v Set -> v -> Set
getVar [] x = error "Nope" 
getVar ((var, dom) : ts) x
       | x == var  = dom
       | otherwise = getVar ts x

(~>) :: Ord a => a -> a -> Bool -- I'm just being fancy
(~>) = (<=)

check :: Eq v => Env v Set -> PRED v -> Bool
check table (Elem a b)    = elemS   (eval table a)  (eval table b)
check table (Subset a b)  = subsetS (eval table a)  (eval table b)
check table (And p q)     = (&&)   (check table p) (check table q)
check table (Or p q)      = (||)   (check table p) (check table q)
check table (Implies p q) = (~>)   (check table p) (check table q)
check table (Not p)       = not    (check table p) 

elemS :: Set -> Set -> Bool
elemS _ (S []) = False
elemS e (S (s:ss)) 
    | e == s    = True
    | otherwise = elemS e (S ss)

subsetS :: Set -> Set -> Bool
subsetS (S []) _ = True
subsetS _ (S []) = False
subsetS (S a) b  = all ((flip elemS) b) a -- våga vägra infix

--------------------- von Neumann ordinals ------------------------

vonNeu :: Eq v => Int -> SET v
vonNeu 0 = Empty
vonNeu n = Union (vonNeu (n-1)) (Singleton (vonNeu (n-1)))

vonNeuS :: Eq v => Env v Set -> Int -> Set
vonNeuS table n = eval table $ vonNeu n

---------------------- claims ---------------------------------------

claim1 :: Env Int Set -> Int -> Int -> Bool -- with bools 
claim1 table n1 n2 = (n1 <= n2) ~> (check table (Subset s1 s2))
    where s1  = vonNeu n1
          s2  = vonNeu n2

claim1' :: Env Int Set -> Int -> Int -> Bool -- with syntactic sets
claim1' table n1 n2 = check table $ Implies (s1 <=: s2) (Subset s1 s2)
    where s1 = vonNeu n1
          s2 = vonNeu n2

-- These hold true for von Neumann ordinal

(==:) :: Eq v => SET v -> SET v -> PRED v
(==:) s1 s2 = And (Subset s1 s2) (Subset s2 s1)

(<:) :: Eq v => SET v -> SET v -> PRED v
(<:) = Elem

(<=:) :: Eq v => SET v -> SET v -> PRED v
(<=:) s1 s2 = Or (s2 <: s2) (s1 ==: s2)

claim2 :: Eq v => Env v Set -> Int -> Bool -- with semantic sets
claim2 table n = (vonNeuS table n) == (S construct)
    where construct = map (vonNeuS table) [0..(n-1)]

claim2' :: Eq v => Env v Set -> Int -> Bool -- with syntactic sets
claim2' table n = check table $ And (Subset s1 s2) (Subset s2 s1) 
    where s1 = vonNeu n
          s2 = foldr1 (Union) $ map (vonNeu) [0..n]

