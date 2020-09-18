All whitespace except newlines is ignored. A sequence of consecutive newlines are treated as a single newline. Newlines followed or preceded by ';' are ignored.
Other newlines act as '/',  but may
without further specification, assume moves are all in present
- indicates choosing to pass on a board, empty space before/after ; _ indicates being unable to move on a timeline because it is not that player's turn there
Lowercase letters indicate relative offsets for T and L of destination.
Comments go from '?' to newline (and don't remove the newline)


sample:

```
1. e3 / Nf6
2. Bb5 / e6
3. c3 / Ne4
4. Qb3 / Qf6
5w. Q>>xt-4f7+ /    ? 5w. Q>>xT1f7+ /
5bT1. -;Kxf7
6. _;Nf3 / -;e6
7. _;N>>t-1f5 / -;Qh4
8. _;e3 / Qt-4f2;-#
```

Note: if more than 1 timeline is created in a single turn, then order matters, so you must specify something

You may not specify the L coordinate of the destination of a same board jump.
For a same board jump, if L or T is given for the source, then rank must also be given for the source.
