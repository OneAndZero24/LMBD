module Processor.Evaluator(reduce) where

import Structure.Term

-- | Core of whole program.
-- | Reduces given λ term tree to normal-form using simple
-- normal order substitution. If there exists a normal form for a given term,
-- this strategy is guaranteed to produce it.

-- | Greedy evaluator, can loop infinitely.
eval :: Term -> Term
eval = until (not.reducible) reduce

-- | Checks if term can be reduced.
reducible :: Term -> Bool
reducible (Var _) = False
reducible (App (Abs _) _) = True
reducible (Abs a) = reducible a
reducible (App a b) = reducible a || reducible b

-- | Reduces to normal form, 
-- stops once terms reduct to themselves.
reduce2Norm :: Term -> Term
reduce2Norm t
    | nt == t = t
    | otherwise = reduce2Norm nt
    where nt = reduce t

-- | One beta reduction step.
reduce :: Term -> Term
reduce (Var a) = Var a
reduce (Abs t) = Abs (reduce t)
reduce (App (Abs t1) t2) = subst 1 t1 t2 -- substitution 
reduce (App t1 t2) = App (reduce t1) (reduce t2)

-- | Application of term to abstraction.
-- | Performs necessary substitutions and upadtes De Bruijn idices.
-- | When term is applied to lambda it's substituted for bound variable,
-- free variable indices get decreased as they get out of lambda.
-- | Parameters: Lambda level (as in term), abstraction, term to substitute
subst :: Int -> Term -> Term -> Term
subst i (Var a) nt
    | a == (i-1) = shiftUp 0 (i-1) nt   -- bound to currently reduced
    | a >= i = Var (a-1)                -- free
    | otherwise = Var a                 -- bound to lower
subst i (Abs t) nt = Abs (subst (i+1) t nt)
subst i (App t1 t2) nt = App (subst i t1 nt) (subst i t2 nt)

-- | Increases indexes of term substituted to abstraction,
-- Only free terms need to be updated.
-- Parameters: Lambda level (as in term), how deep inserted, term to be updated
shiftUp :: Int -> Int -> Term -> Term
shiftUp i n (Var a)
    | a >= i = Var(a+n)  -- free
    | otherwise = Var a  -- bound
shiftUp i n (Abs t) = Abs (shiftUp (i+1) n t)
shiftUp i n (App t1 t2) = App (shiftUp i n t1) (shiftUp i n t2)