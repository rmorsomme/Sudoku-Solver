The RMD file presents an algorithm that solves sudoku following two very simple principles:

- if there is only one value left to fill a cell, fill the cell with it

- if a value has only one possible location left in a row, column or box, write it there.

Surprisingly, by just following these principles, we can solve most sudoku grids.

When these principles are not sufficient anymore to further complete a grid, then guessing the value of a cell is sufficient to continue filling in the grid.

I have not encountered grids that required the algorithm to effectuate more than one guess.
