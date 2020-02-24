{-# LANGUAGE UnicodeSyntax #-}

module Unλambda
        ( runFile
        , run    -- run/runFile call parseLazy
        , formatParse
        , formatParseFile
        ) where

import AST
import UserInputHandler
import Parser
import Interpreter

run :: String -> IO Eλ
run = showEλ . formatParse

runFile :: String -> IO Eλ
runFile s = run =<< readFile s

formatParse :: String -> Aλ
formatParse = parseLazy . format

formatParseFile :: String -> IO Aλ
formatParseFile s = formatParse <$> readFile s

