{-# LANGUAGE UnicodeSyntax #-}

module Unλαmβdα
        ( parseNaive
        , run 
        ) where

import Text.Parsec
import System.IO
import Control.Monad
import Control.Applicative hiding ((<|>))

-----------------------------------------------------------------------
----------------------- Datatypes -------------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*--- 
            λ is beautiful
            Shut up
        ---*-_-*-_-*-_-*---}

data Aλ = A Aλ Aλ | E Eλ
data Eλ = K 
        | Kf Eλ
        | S
        | Sf Eλ
        | Sff Eλ Eλ
        | I
        | D String
        | R

-----------------------------------------------------------------------
----------------------- Instances -------------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*--- 
            Show me love
            And pretty strings
        ---*-_-*-_-*-_-*---}

instance Show Aλ where
        show (A a b)   = "`(" ++ show a ++ ")" ++ show b
        show (E e)     = case e of
                           K         -> "k"
                           (Kf a)    -> "kf" ++ show (E a)
                           S         -> "s"
                           (Sf a)    -> "sf" ++ show (E a)
                           (Sff a b) -> "sff" ++ show (E a) ++ show (E b)
                           I         -> "i"
                           (D a)     -> "." ++ id a
                           R         -> "r"

instance Show Eλ where
        show K         = "k"
        show (Kf a)    = "k" ++ show a
        show S         = "s"
        show (Sf a)    = show (Sff a (I))
        show (Sff a b) = "s" ++ show a ++ show b
        show I         = ""
        show (D a)     = "." ++ id a 
        show R         = "\n"

-----------------------------------------------------------------------
-------------------------- Parsec Parse  ------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*--- 
            What giving up
            This is
            Looks Like
        ---*-_-*-_-*-_-*---}

parseLazy :: String -> Aλ
parseLazy = either (error . show) id . parse tryEλ "unlambda" 

-- No type because infer?
tryEλ = try (char '`' *> (A <$> tryEλ <*> tryEλ))        <|>
        try (char '.' *> (E . D <$> fmap (:[]) anyChar)) <|>
        try (char 's' *> return (E S))                   <|>
        try (char 'k' *> return (E K))                   <|>
        try (char 'i' *> return (E I))                   <|>
        try (char 'r' *> return (E R))                -- <|>

-----------------------------------------------------------------------
--------------------------- Naive Parse  ------------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*--- 
            Parsing is hell
            This is dumb
            So am I
        ---*-_-*-_-*-_-*---}

-- A (A (E $ D "h") (E $ D "m")) (E I)
-- `  ` [E $ D "h" , E $ D "m" ,  E I]
-- ` (`     (.  h)      (.  m))    (i)
-- `  `      .  h        .  m       i
-- ``.h.mi

parseNaive :: String -> Aλ
parseNaive = parseA' . reverse . parseE 

parseA' :: [Aλ] -> Aλ
parseA' []     = error "parseA (1): Invalid program"
parseA' (e:[]) = e
parseA' (e:es) = (flip A) e $ parseA' es 

test :: String -> Aλ
test s = (flip parseA $ s) . reverse . parseE $ s

parseA :: [Aλ] -> String -> Aλ
parseA [] _     = error "(1) free expression"
parseA _ []     = error "(2) non application"
parseA _ (a:[]) = error "(3) free application"
parseA (e:[]) _ = e 
parseA (e:es) (a:as) = case a of
                         '`' -> (flip A) e $ parseA es as
                         _   -> parseA (e:es) (as)

parseE :: String -> [Aλ]
parseE []         = []
parseE (a:[])     = case a of
                      'i' -> [E I]
                      'r' -> [E R]
                      _   -> error "parseE (1): Invalid program"
parseE (a:b:[])   = case a of
                     '`' -> parseE [b] 
                     '.' -> [E $ D [b]]
                     'i' -> [E I] ++ parseE [b]
                     'r' -> [E R] ++ parseE [b]
                     'k' -> [E K] ++ parseE [b]
                     's' -> [E S] ++ parseE [b]
                     _   -> error "parseE (2): Invalid program"
parseE (a:b:cs) = case a of
                     '`' -> parseE (b:cs)
                     '.' -> [E $ D [b]] ++ parseE (cs)
                     'i' -> [E I] ++ parseE (b:cs)
                     'r' -> [E R] ++ parseE (b:cs)
                     'k' -> [E K] ++ parseE (b:cs)
                     's' -> [E S] ++ parseE (b:cs)
                     _   -> error "parseE (3): Invalid program"

parseSK :: String -> [Aλ]
parseSK [] = []
parseSK (a:[])   = case a of
                       's' -> [E S]
                       'k' -> [E K]
parseSK (a:b:[]) = case a of
                       's' -> [E $ Sf (getE b)]
                       'k' -> [E $ Kf (getE b)]
parseSK (a:b:c:[]) = case a of
                       's' -> [E $ Sff (getE b) (getE c)] 
parseSK (a:b:c:ds) = case a of
                       's' -> [E $ Sff (getE b) (getE c)] ++ parseSK ds
                       'k' -> [E $ Kf (getE b)] ++ parseSK (c:ds)

getE :: Char -> Eλ
getE a = case a of
           's' -> S
           'k' -> K
           'i' -> I

-----------------------------------------------------------------------
------------------------ Interpreter Logic ----------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*--- 
            ooga to the booga     
        ---*-_-*-_-*-_-*---}

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
showEλ (E e)   = return e  
showEλ (A l r) = collapse <$> showEλ l <*> showEλ r >>= id

run :: String -> IO (Eλ)
run = showEλ . parseLazy

