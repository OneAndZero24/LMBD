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
reduce (App (Abs t1) t2) = 
reduce (Abs t) = Abs (reduce t)
reduce (App t1 t2) = reduce(App (reduce t1) (reduce t2))

-- TODO differentiate reindexing and substitution - putting arguments in
--    (this is worng)

-- | Reindexes variables.
subst :: Int -> Term -> Term
subst i (Var a) = (Var (a+i))
subst i (Abs t) = Abs (subst i t)
subst i (App t1 t2) = App (subst i t1) (subst i t2)

-- TODO Detect loops