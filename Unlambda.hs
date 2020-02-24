{-# LANGUAGE UnicodeSyntax #-}

module Unλαmβdα
        ( runFile
        , run    -- run/runFile call parseLazy
        , formatParse
        , formatParseFile
        ) where

import Unlambda.AST
import Unlambda.UserInputHandler
import Unlambda.Parser
import Unlambda.Interpreter

run :: String -> IO Eλ
run = showEλ . formatParse

runFile :: String -> IO Eλ
runFile s = run =<< readFile s

formatParse :: String -> Aλ
formatParse = parseLazy . format

formatParseFile :: String -> IO Aλ
formatParseFile s = formatParse <$> readFile s

