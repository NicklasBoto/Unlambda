{-# LANGUAGE UnicodeSyntax #-}

module Unλαmβdα where

import System.IO
import Control.Monad

data Aλ = A Aλ Aλ | E Eλ
data Eλ = K 
        | Kf Eλ
        | S
        | Sf Eλ
        | Sff Eλ Eλ
        | I
        | D String
        | R

instance Show Aλ where
        show (A a b)   = "`" ++ show a ++ show b
        show (E e)     = case e of
                           K -> "k"
                           S -> "s"
                           I -> "i"
                           (D a) -> "." ++ id a
                           R     -> "r"

instance Show Eλ where
        show K         = "k"
        show (Kf a)    = "k" ++ show a
        show S         = "s"
        show (Sf a)    = show (Sff a (I))
        show (Sff a b) = "s" ++ show a ++ show b
        show I         = ""
        show (D a)     = "." ++ id a 
        show R         = "\n"

-- A (A (E $ D "h") (E $ D "m")) (E I)
-- `  ` [E $ D "h" , E $ D "m" ,  E I]
-- ` (`     (.  h)      (.  m))    (i)
-- `  `      .  h        .  m       i
-- ``.h.mi

parse :: String -> Aλ
parse = parseA . reverse . parseE 

parseA :: [Aλ] -> Aλ
parseA []     = error "Invalid program"
parseA (e:[]) = e
parseA (e:es) = (flip A) e $ parseA es 

parseE :: String -> [Aλ]
parseE []         = []
parseE (a:[])     = case a of
                      'i' -> [E I]
                      'r' -> [E R]
parseE (a:b:c:ds) = case a of
                     '`' -> parseE (b:c:ds)
                     '.' -> [E $ D [b]] ++ parseE (c:ds)
                     'i' -> [E I] ++ parseE (b:c:ds)
                     'r' -> [E R] ++ parseE (b:c:ds)
                     'k' -> [E K] ++ parseE (b:c:ds)
                     's' -> [E S] ++ parseE (b:c:ds)

collapse :: Eλ -> Eλ -> IO (Eλ)
collapse (D a) b     = (putStr a)    >> return b
collapse (R) a       = (putStr "\n") >> return a
collapse (I) a       = return a
collapse (Kf a) b    = return a  
collapse (K) a       = return $ Kf a
collapse (S) a       = return $ Sf a 
collapse (Sf a) b    = return $ Sff a b
collapse (Sff a b) c = collapse <$> fun <*> val >>= id
    where fun = collapse a c
          val = collapse b c

showEλ :: Aλ -> IO (Eλ)
showEλ (E e) = return e  
showEλ (A l r) = collapse <$> showEλ l <*> showEλ r >>= id

run :: String -> IO (Eλ)
run = showEλ . parse

