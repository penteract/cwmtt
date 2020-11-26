##Overview:
This is pretty similar to Hexicube's notation, but uses symbols more like Shad's. See https://github.com/adri326/5dchess-notation for more info about those.

The biggest innovation is to use lowercase T and L for relative offsets (may be helpful when transcribing games). It is also flexible in various ways, allowing for newlines between moves, omitting turn numbers, etc . It is always permissible to add extra information such as the T or L-index of the starting point of a move¹.

rough guide (misses spacing, check indicators, comments):
```
actionSequence ::= (action ('/' action)*)?
action ::= turnNumber? move (';' move)* check?
turnNumber ::= n? ('b'|'w')? ('T' n)? '.'
move ::= source piece? ('>>'|'>')? dest | '-' | '_' | '<'
source ::= (n'L')?('T'n)? file? n?
piece ::= ('K'|'N'|'B'|'R'|'Q'|'P'|'U'|'D')
dest ::= (n'L'|'l')?('T'|'t'n)? file n
file ::= 'a'|'b'|...|'h'
n ::= '-'? digit digit*
check ::= '+' | '#'
```

Within an action(moveset), unless the L-index of the source of a move is indicated, it is assumed that they are listed in descending order of L-index, starting from the active timeline most recently created by white.



##Recommendations:
These are suggestions to make it easier to read, but they aren't enforced by the parser.
- If the turn (T-index) of the present is not 1 greater than(for white) or equal to(for black) that of the previous action, indicate it in the turn number.
- If multiple timelines are created in an action, give the L-index of all moves for that action.
- If no L-indices are given, give placeholders ('-', '_' or '<' meaning didn't move, can't move and hopped to from another board) for all timelines.
- Begin each action pair on a new line and give turn number
  - If it is clearer, black's and white's actions may be put on separate lines. If so, give each the same turn number and make use of w/b.



sample:

```
1. e3 / Nf6
2. Bb5 / e6
3. c3 / Ne4
4. Qb3 / Qf6
5w. Q>>xt-4f7+ /    ? 5w. Q>>xT1f7+ /
5bT1. Kxf7;-
6. Nf3;_ / e6;-
7. N>>t-1f5;_ / Qh4;-
8. e3;_ / -;Q>>t-4f2#
```

Note: if more than 1 timeline is created in a single turn, then order matters, so you must specify something


sample2:
```
1.    c3 / c5
2.    Qb3 / N>>T1g6
3T2.  -;Qb3 / _;c6
4.    Qc4;Qc4 / B>>T1c6;Nh6
5wT4. 0LT4Q>>xf7 /
5bT1. Kxf7;-;_;_
6.    e3;_;-;d4 / Qe8;-;_;Nf6
7.    Qh5;-;_;Qd2#
```
##Technical details (incomplete):
- If the source timeline of a move is not indicated, it is assumed to be 1 lower than that of the previous move (or the greatest active timeline for the first move in a moveset).

- All whitespace except newlines is ignored. A sequence of consecutive newlines are treated as a single newline. Newlines followed or preceded by ';' are ignored. Other newlines act as '/',  but may be duplicated.
- `-` indicates choosing to pass on a board, empty space before/after ; `_` indicates being unable to move on a timeline because it is not that player's turn there
- Lowercase letters indicate relative offsets for T and L of destination.
- Comments go from '?' to newline (and don't remove the newline)
- Jumps creating a new timeline use `>>`. Jumps between timelines(hops) use `>` and the destination timeline may be marked with '<'
- When multiple timelines are created in a different order, the starting timelines of pieces must be given.


¹You may not specify the L coordinate of the destination of a same board jump.
For a same board jump, if L or T is given for the source, then rank must also be given for the source.
