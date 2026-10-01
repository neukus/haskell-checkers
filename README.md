# Haskell Checkers

Checkers in Haskell: play against a minimax engine in the terminal.

```
  0 1 2 3 4 5 6 7
0 * b * b * b * b
1 b * b * b * b *
2 * b * b * b * b
3 * * * * * * * *
4 * * * * * * * *
5 w * w * w * w *
6 * w * w * w * w
7 w * w * w * w *
```

## Run

Requires [GHC](https://www.haskell.org/ghcup/), no other dependencies.

```bash
ghc -O2 checkers.hs -o checkers
./checkers
```

Or without compiling: `runghc checkers.hs`

## How to play

You play White (`w`) and move first; the computer plays Black (`b`). Kings are shown as `W` / `B`.

Enter moves as `x,y` squares (column,row), e.g. `2,5 3,4`. For multi-jumps, list every landing square. `q` quits.

## Rules

- Men move one square diagonally forward and capture in all four directions.
- Kings move and capture one square in all four directions.
- Capturing is mandatory and multi-jumps must be completed.
- A man reaching the far row becomes a king.
- A player without a legal move loses.

## Engine

Minimax search (depth 4, adjustable in `main`) with an evaluation based on material, board position, capture opportunities and risks, and the opponent's promotion threats.
