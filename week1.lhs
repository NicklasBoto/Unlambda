ex_1.1

__2

\begin{code}
import Data.Either


data Exp = Con Integer
        | Exp `Plus`  Exp
        | Exp `Minus` Exp
        | Exp `Times` Exp
    deriving (Eq, Show)

eval :: Exp -> Integer
eval (Con c) = c
eval (ea `Plus` eb) = (+) (eval ea) (eval eb)
eval (ea `Minus` eb) = (-) (eval ea) (eval eb)
eval (ea `Times` eb) = (*) (eval ea) (eval eb)

\end{code}


ex_1.3

| a | = A
| b | = B
| c | = c

| {x <- a, y <- b : Either x y} |  = A + B 

| {x <- a , y <- b : (,) x y } | = A * B

| {x <- a, y <- b : a -> b } | = B ^ A

ex_1.4

| {b -> Maybe b} | = 3 ^ 2 = 9
| {Maybe b -> b} | = 2 ^ 3 = 8
| {Maybe (Bool, Maybe (Bool, Maybe Bool))} | = 1 + (2 * (1 + (2 * (1 + 2)))) = 15

If needed I can recall the values

ex_1.8

a . (1+) -- getting the (i+1):th element of the sequence
(1+) . a -- adding 1 to the i:th element of the sequence

liftSeq_1 is like fmap with Seq being the functor. Seq is creating an iterable to map over
liftSeq_0 is exactly conSeq, a nullary function would just be a constant

ex_1.9

(1+)  . (*2)  = \x -> 1 + (2 * x)
(*2)  . (1+)  = \x -> (1 + x) * 2
(+1)  . (**2) = \x -> x**2 + 1
(**2) . (+1)  = \x -> (x + 1)**2
(a+)  . (b+)  = \x -> a + (b + x)
















\begin{code}
f2p :: (a -> (b, c)) -> (a -> b, a -> c)
f2p f = (fa, fb)
    where fa = fst . f
          fb = snd . f

s2p :: (Either b c -> a) -> (b -> a, c -> a)
s2p f = (fa, fb)
    where fa = f . Left
          fb = f . Right
\end{code}
