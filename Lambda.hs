module Name where

--- do some lambda dambda stuff
-- datatypes for all types of things in λCal,
-- Application
-- Abstraction
-- Variable
-- Like A, V and P......
-- get it to typecheck

type VarType = String
data Eλ = V VarType
        | A Eλ Eλ
        | Λ VarType Eλ

instance Show Eλ where
        show (V idt) = id idt
        show (A l r) = show l ++ "(" ++ show r ++ ")"
        show (Λ i e) = "λ" ++ id i ++ "." ++ show e

