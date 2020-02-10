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

newtype Set = S [Set] 

instance Show Set where
        show (S a) = show a

instance (Show v) => Show (SET v) where
        show Empty = "Ø"
        show (Singleton s) = "{" ++ show s ++ "}"
        show (Union a b)   = "(" ++ show a ++ " ⋃ " ++ show b ++ ")"
        show (Insec a b)   = "(" ++ show a ++ " ⋂ " ++ show b ++ ")"
        show (Var v)       = show v

instance (Show v) => Show (PRED v) where
        show (Elem a b)    = "(" ++ show a ++ " ∈ "  ++ show b ++ ")"
        show (Subset a b)  = "(" ++ show a ++ " ⊆ "  ++ show b ++ ")"
        show (And p q)     = "(" ++ show p ++ " ^ "  ++ show q ++ ")"
        show (Or p q)      = "(" ++ show p ++ " ∨ "  ++ show q ++ ")"
        show (Implies p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
        show (Not p)       = "¬ (" ++ show p ++ ")"

instance Eq Set where -- we have decided on the arbitrary order of element lenghts
        (==) (S a) (S b) = (sort a) == (sort b)

instance Ord Set where 
        compare (S a) (S b) = compare (length a) (length b)

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

claim2 :: Eq v => Env v Set -> Int -> Bool -- with semantic sets
claim2 table n = (vonNeuS table n) == (S construct)
    where construct = map (vonNeuS table) [0..(n-1)]

claim2' :: Eq v => Env v Set -> Int -> Bool -- with syntactic sets
claim2' table n = check table $ And (Subset s1 s2) (Subset s2 s1) 
    where s1 = vonNeu n
          s2 = foldr1 Union $ map (Singleton . vonNeu) [0..(n-1)]

--------------- test cases ---------------------

s1S = S [S[], S[S[]]]
s2S = S [S[S[]], S[]]

s = Singleton; e = Empty

s1 = Union (s (s e)) (s e)
s2 = Union (s e) (s (s e))

