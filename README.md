# Monad and all that ...
It is lectures by John Hugues. I implement the examples shown in his presentation.
There are some codes he didn't give audiences, which are written by me to make project work.

## Example 1: Tree

Preliminaries in file `src/Tree.hs`. Here we want solve two problems:
- number the leaves on a given tree
- zip two trees when trees have the same shape, otherwise return an "failed" result.

### 1.1 Naive

It is shown in file `src/Tree/Naive.hs`, which is tedious and mistakable, because Haskell is pure and impossible to store variables during computions so that we need pass paramemters everywhere.

### 1.2 Code Reusability Improved

It is in file `src/Tree/BeforeMonad.hs` and directory `src/Tree/BeforeMonad`.
Here we get some common features in solutions of two problems. That is why we need monad, which defined by our discovered features.

### 1.3 Monad

It is in file `src/Tree/Monad.hs`. All implementation is based on self defined Monad.

### 1.4 Advanced Monad Methods

It file `src/Tree/AfterMonad.hs`, some common operations for monad are defined, which makes our solution shorter and more readable.