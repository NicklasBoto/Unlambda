{-# LANGUAGE UnicodeSyntax #-}

module Interpreter
        ( collapse
        , showEλ
        ) where

import Control.Monad
import AST

-----------------------------------------------------------------------
------------------------ Interpreter Logic ----------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            `````.P.=.N.P.?i
        ---*-_-*-_-*-_-*---}

collapse :: Eλ -> Eλ -> IO Eλ
collapse (D a) b   = putStr a    >> return b
collapse R a       = putStr "\n" >> return a
collapse V a       = return V
collapse I a       = return a
collapse (Kf a) b  = return a
collapse K a       = return $ Kf a
collapse S a       = return $ Sf a
collapse (Sf a) b    = return $ Sff a b
collapse (Sff a b) c = join $ collapse <$> fun <*> val
    where fun = collapse a c
          val = collapse b c

showEλ :: Aλ -> IO (Eλ)
showEλ (E e)   = return e
showEλ (A l r) = join $ collapse <$> showEλ l <*> showEλ r

