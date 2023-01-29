# **L M B D**
## Simple λ calculus evaluator/reducer 
---  
## Info
Jan Miksa z1173329  
Done as Functional Programming course final project.  
> "Write a program, that takes in λ term from stdio and returns normal form, if such exists. Suppose that λ terms can contain: brackets, dot, slash (used as λ) and variables represented by letters a-z."  
> "Use only: Prelude, System IO, System Environment"

## How it works?
This program uses De Bruijn indexing to omit need for alpha reduction. Beta reduction is done in normal order.  
Such evaluation should result in normal form whenever one exists. Although in `normal` mode it will stop reducing
once terms reduce to themselves, but in general it can LOOP INFINITELY.
First `Lexer` processes input into a series of tokens. All of missing brackets omitted due to operator associativity are added.
Then `Parser` parses these tokens into `Term` tree which is evaluated.
Parsing in `Parser` and `Lexer` is loosely based on normal order parsing algorithm.

## Compiling
`ghc --make Main.hs`

## Running
`./lmbd *args*`  
LMBD can run in two modes: REPL or batch.
In batch mode only argument should be filename to parse. This file can include comments - lines starting with `>`.  
In REPL mode (activated by `-i` flag) program will query for next terms no reduce. `!` to quit.  

Evaluation can run in 3 modes: `normal`, `greedy`, `one step`.
- `normal` - default, will reduce until term reduces to itself
- `greedy` - (activated by `-g` flag), will reduce whenever possible
- `one step` - (activated by `-s` flag), performs one beta reduction step at a time
## Examples
*(aside from ex1.lmbd)*  
*Bx - means bound variable*
```
\>\x.xz \y.xy
\Ba.((Ba z) (\Bb.(Ba Bb)))

\>(\x.(x y))(\z.z)
y

\>((\x. \y.(x y))(\y.y)) w
w

\>(\x.x x) (\y.y x) z
x x z
```