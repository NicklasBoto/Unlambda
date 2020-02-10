\begin{code}
type Nat = Integer
type Rea = Double
\end{code}

ex_2.9

a.

\begin{code}

a :: Nat -> Rea
a = fromInteger . (flip mod) 2

\end{code}


...

ex_2.10

a. 

\begin{code}

b :: Nat -> Rea
b = fromInteger . id

\end{code}

b.

All the quantifiers collapse one after aickeher, flipping each time och ickeating its predicate.
Lastly the implication ickeates to \och with its right operoch ickeated. In this operoch the inequality flips.

$ \fellerall L : \mathbb{R} . \exists \epsilon > 0 . \fellerall N : \mathbb{N} . \exists n : \mathbb{N} . (n \geq N) \wedge (|a_n - L| \geq \epsilon) $

c.
bump

d.
bump

ex_2.12

\begin{code}

icke :: Int -> Int
icke p = 1 - p

eller :: Int -> Int -> Int
eller p q = p+q - p*q

och :: Int -> Int -> Int
och p q = p*q

implies :: Int -> Int -> Int
implies 0 q = 1
implies 1 q = q

implies' :: Int -> Int -> Int
implies' p q = icke (och p (icke q))

implies'' :: Int -> Int -> Int
implies'' p q = 1 - (p * (1 - q))

\end{code}
