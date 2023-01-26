module Evaluator(eval) where

-- | Core of whole program.
-- | Reduces given λ term tree to normal-form using simple
-- call-by-name substitution. If there exists a normal form for a given term,
-- this strategy is guaranteed to produce it.
-- | As detecting terms which don't have a normal form is undecidable,
-- and using partials solutions is out of scope of this project this evaluation strategy
-- CAN LEAD TO INFINITE CALCULATIONS.
eval :: Term -> Term
