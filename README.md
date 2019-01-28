In this script, I develop an algorithm to solve sudoku grids that uses four very simple principles. Despite its simplicity, the algorithm can solve most sudoku grids, even the most sparse ones!

- Principle 1: A cell can only have one value.

- Principle 2: A row/column/box contains each value exactly once.

- Principle 3: If there is only one value available to fill a cell, fill the cell with it.

- Principle 4: If a value has only one location available in a row/column/box, write it there.

Yet, for difficult grids, the algorithm can get stuck. When the algorithm is stuck, we need to guess the value of a cell. I updated the original algorithm so that, when it is stuck, it guesses the value of a cell and subsequently checks the validity of the obtained grid. This version of the algorithm solves even the most difficult grid after a few iterations.

This shows that, for most sudoku grids, applying these four simple principles in a systematic way leads to a solution. When these principles are not sufficient, we just have to make one or two guesses. Who knew sudoku was so simple?
