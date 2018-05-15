# Monad and all that ...
It is lectures by John Hugues. I implement the examples shown in his presentation.
There are some codes he didn't give audiences, which are written by me to make project work.

- [Presentation Slides](http://www.cse.chalmers.se/~rjmh/OPLSS/)
- [Videos](https://www.youtube.com/playlist?list=PLGCr8P_YncjVeZTcfHT1Cb1OfVnNahek5)

## Lecture 1: Monad
### Example 1: State Monad & Maybe Monad

Preliminaries are in file `src/Tree.hs`, where a full binary tree are defined. We want solve two problems:
- number the leaves on a given tree
- zip two trees when trees have the same shape, otherwise return an "failed" result.

#### 1.1 Naive

It is shown in file `src/Tree/Naive.hs`, which is tedious and mistakable, because Haskell is pure and impossible to store variables during computions so that we need pass paramemters everywhere.

#### 1.2 Code Reusability Improved

It is in file `src/Tree/BeforeMonad.hs` and directory `src/Tree/BeforeMonad`.
Here we get some common features in solutions of two problems. That is why we need monad, which defined by our discovered features.

#### 1.3 Monad

In file `src/Tree/Monad.hs`, we define `Monad` class. The solutions to two problems are refactored using `Monad`.

#### 1.4 Advanced Monad Methods

It file `src/Tree/AfterMonad.hs`, some common operations for monad such as `liftM2` and `liftM` are defined, which makes our solutions shorter and more readable.

### Example 2: Random Monad

To generate a list of numbers, the `seed` should be modified (`next` or `split`) and transmitted repeatedly.

### Example 3: IO Monad

Monad is boxed computions. IO is computions that interact with real world. In `src/IO.hs`, the code shows that IO is something like `RealWorld -> (RealWorld, feedback)`. It takes a "world" to get a new "world".

## Lecture 2: Monad Transformer

### Example 1: List Monad / Backtracking (8 queens)

Nested lists isomorphic with trees. Lazy evaluation on nested lists is equivalent to deep first search on solution space tree.

### Example 2: State Monad Transformer

Add features of state monad to any other monads.

### Example 3: Parser

A monad with features from both state and list.

### Example 4: BackT - List Monad Transformer

### Example 5: Prolog List

A Haskell function `appendL` behaves in the same way as Prolog function:
```prolog
append([], Ys, Ys).
append([X|Xs], Ys, [Z|Zs]) :-
    append(Xs, Ys, Zs)
```