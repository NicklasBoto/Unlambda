{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Run
import AST
import Turtle
import Control.Monad
import Data.List
import System.IO
import System.Directory

getFile :: Parser (Bool, Text)
getFile = (,) <$> switch "string" 's' "Interpret as string instead of filepath"
              <*> argText "source" "Input file of interpreter"

interpretFile :: Text -> IO Eλ
interpretFile = runFile . show

interpretString :: Text -> IO Eλ
interpretString = run . show

main :: IO Eλ
main = do
        (flag, text) <- options "Unλαmβdα - The Gothenburg Unlambda Interpreter\nversion 1.1" getFile
        if flag then
                interpretString text
        else
                interpretFile text
