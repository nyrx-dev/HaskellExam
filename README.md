# Haskell Snake

A two-player Snake game built for the DM580 functional programming exam at SDU. The game uses Scratchy, an embedded Haskell language for graphical programs, and Gloss for rendering.

The project builds on the Scratchy starter framework supplied for the course. I implemented the game and worked on the language interpreter as part of the assignment.

## Play

- Green snake: W, A, S, D
- Blue snake: I, J, K, L
- The background turns red on a wall collision and black on a sprite collision.

## Run

Install GHC 9.12 and Cabal, then run:

```sh
cabal run
```

## Tests

```sh
cabal test
```

The tests cover direction checks, key bindings, and the number of sprites in the game.
