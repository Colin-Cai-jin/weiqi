## Weiqi(Go)

Weiqi(Go) is an ancient game of Chinese. More than 30,000,000 peoples play it. This program is written by Scheme, and you can play it on the linux terminal.

## Run

### Racket
- **./run.sh Racket**

### Chez Scheme
- **./run.sh Chez**

## Play

Two players can play Weiqi(Go) in turn.
The moving of the cursor:`w' for upward;`s' for downward;`a' for left;`d' for right.
Directly input the coordinate of the cursor: Two capital letters between `A' and `S'.
For example, if the two letters input are `C' and `H'. `A' is 65 in ASCII, `C' is 67 in ASCII, and `H' is 72 in ASCII, so the input coodinate is (67-65, 72-65).
`Enter': To put one stone on the position of the cursor.

## Ending the game

Type `1' if the game is over.
Then, pick up the dead stones by moving the cursor to the position of the dead stone and type `Enter'
At last, type `2' to determin the winner.
