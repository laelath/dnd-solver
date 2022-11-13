# DND-Solver

A solver for the Dungeons & Diagrams minigame in Last Call BBS.

Why be a human SMT solver when the SMT solver is already in the computer.

## Dependencies
- `racket`
- `z3`

## Usage

Right now the only way to use it is through the repl.
Open up a repl with `DNDSolver.rkt` and call `do-solve` with lists of the block counts, monster positions, and treasure positions.
Positions are pairs of zero-indexed integers in row-column order.

The solver right now does not check that solutions satisfy the contiguous section requirement,
but all checked puzzles produce a solution that satisfies this requirement anyways.
Please let me know if you find one where this assumption breaks.

### Example
```
> (do-solve
   '(1 5 3 3 3 5 2 2)
   '(5 1 5 2 3 2 6 0)
   '()
   '((3 . 3)))
XXXXX   
      X 
 XXXXX  
   t X X
 X   X X
 X   X  
 XXXXXX 
```
