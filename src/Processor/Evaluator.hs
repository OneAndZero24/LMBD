module Processor.Evaluator(reduce) where

import Structure.Term

-- | Core of whole program.
-- | Reduces given λ term tree to normal-form using simple
-- normal order substitution. If there exists a normal form for a given term,
-- this strategy is guaranteed to produce it.
-- | Detecting terms which don't have normal form is undecidable.
-- | This evaluator will stop once it loops to starting term, but in general
-- IT CAN PERFORM INFINITE CALCULATIONS. 

-- | One beta reduction step.
reduce :: Term -> Term
reduce (Var a) = Var a
reduce (Abs t) = Abs (reduce t)
reduce (App (Abs t1) t2) = subst 1 t1 t2
reduce (App t1 t2) = App (reduce t1) (reduce t2)

-- | Application of term to abstraction. Substitutes parameters.
subst :: Int -> Term -> Term -> Term
subst i (Var a) nt
    | a == (i-1) = shiftUp 0 (i-1) nt 
    | a >= i = Var (a-1)
    | otherwise = Var a
subst i (Abs t) nt = Abs (subst (i+1) t nt)
subst i (App t1 t2) nt = App (subst i t1 nt) (subst i t2 nt)

shiftUp :: Int -> Int -> Term -> Term
shiftUp i n (Var a)
    | a >= i = Var(a+n)
    | otherwise = Var a
shiftUp i n (Abs t) = Abs (shiftUp (i+1) n t)
shiftUp i n (App t1 t2) = App (shiftUp i n t1) (shiftUp i n t2)

--TODO eval, loop detection, reduce untill, reducible