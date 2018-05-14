-- Before using monad class, these two problem have their own `return` ans `>>=`,
-- which may be different from standard Haskell libraries and be conflict with each other.
-- Therefore, we put them into two sub-modules as isolate namespace.

module Tree.BeforeMonad(
       N.number, Z.zipTree
    ) where

import Tree.BeforeMonad.Number as N
import Tree.BeforeMonad.ZipTree as Z