# Tree manipulations

Given a triangle tree, for example:
```
7
6 3
3 8 5
11 2 10 9
```

Application will return the shortest path from the top to the bottom node.

To run - `sbt run` and provide input. Application will read everything till the end of file, so EOF must be provided.

Implementation is based on a simplified Dijkstra's algorithm