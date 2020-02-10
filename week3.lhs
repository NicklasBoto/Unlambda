\begin{code}
type REAL = Double -- REAL, because type shadowing
\end{code}

If we take the coordinates to be in their simplest form, in one dimension, the Hamiltonian will be of type

\begin{code}

type T = REAL
type Q = REAL
type P = REAL

fH :: (Q, P, T) -> REAL
fH (q, p, t) = undefined

\end{code}

Assuming that the Hamiltonian is an endofunction it is implied that the derivative of H is a function of the same type.

\begin{code}

delHdelq :: (Q, P, T) -> REAL
delHdelq (q, p, t) = undefined

delHdelp :: (Q, P, T) -> REAL
delHdelp (q, p, t) = undefined

\end{code}

Since the left hand side of the equations seem be of a single variable, it is assumed that delHdelq (and delHdelp) somehow evaluate to single variable functions.

Also, the coordinate functions p and q are assumed to be function of time.

\begin{code}

q :: T -> Q
p :: T -> P
p = undefined; q = undefined

-- And since T and Q are both REALs

dpdt :: T -> P
dqdt :: T -> Q
dpdt = undefined; dqdt = undefined

\end{code}

The problem now becomes equating functions that are of different arity, p and q, and H.
We should now want some function

\begin{code}

expand :: T -> (Q, P, T)

\end{code}

What would this function mean?
Well, since we have p and q as functions of time that return the position and momentum of the system, we can simply get the whole system state from the time.

\begin{code}

expand t = (q t, p t, t)

\end{code}

With this, we can transform the tertiary functions to unary functions

\begin{code}

fH_but_unary_this_time :: T -> REAL
fH_but_unary_this_time  = fH . expand 

-- and also

delHdelp_unary :: T -> REAL
delHdelp_unary = delHdelp . expand

delHdelq_unary :: T -> REAL
delHdelq_unary = delHdelq . expand

\end{code}

The types of these functions pose a problem though. They return REAL, when dpdt and dqdt return P and Q, respectively.

This is not an issue for the haskell compiler, since all of them are REAL, but it might be an issue either way.

Resolving this would require me to think about the actual physical quantities that the equations represent, which I'm not sure I'm supposed to be doing...

\begin{code}

firstEq :: T -> Bool
firstEq t =  (dpdt t) == - (delHdelq_unary t)
--            dpdt    ==    delHdelq . expand

secondEq :: T -> Bool
secondEq t = (dqdt t) ==   (delHdelp_unary t)
--            dqdt    ==    delHdelp . expand

\end{code}

The meaning of these equations then is inputing the coordinate pair (p, q) of a system, and then requiring that both of them hold true. Practically, the functions are evaluated over time, explaining why they require it as input.

