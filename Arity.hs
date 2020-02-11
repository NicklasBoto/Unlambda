module Arity (
        Nullary,
        Unary,
        Binary,
        Tertiary
        ) where

type Nullary  a = a
type Unary    a = a -> a
type Binary   a = a -> a -> a
type Tertiary a = a -> a -> a -> a

