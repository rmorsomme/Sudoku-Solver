Sudoku
================
Raphaël Morsomme
2019-02-22

-   [Introduction](#introduction)
-   [Representations](#representations)
    -   [The object `grid`](#the-object-grid)
    -   [The object `poss`](#the-object-poss)
-   [Solving a sudoku](#solving-a-sudoku)
    -   [Pseudo-code](#pseudo-code)
    -   [Updating the object `poss`](#updating-the-object-poss)
        -   [Cell by cell](#cell-by-cell)
        -   [Row by row, column by column and box by box](#row-by-row-column-by-column-and-box-by-box)
        -   [The function `update_poss()`](#the-function-update_poss)
    -   [Updating `grid`](#updating-grid)
        -   [Cell by cell](#cell-by-cell-1)
        -   [Row by row, column by column and box by box](#row-by-row-column-by-column-and-box-by-box-1)
        -   [The function `update_grid()`](#the-function-update_grid)
    -   [Solving our first sudoku grid](#solving-our-first-sudoku-grid)
    -   [More complex cases](#more-complex-cases)
-   [Algorithm stuck? Take a guess!](#algorithm-stuck-take-a-guess)
    -   [Taking a guess](#taking-a-guess)
    -   [Checking a grid](#checking-a-grid)
    -   [Solving a sudoku grid with guess](#solving-a-sudoku-grid-with-guess)
    -   [Testing the algorithm: extreme cases](#testing-the-algorithm-extreme-cases)
-   [Conclusion](#conclusion)

``` r
library(tidyverse)
```

Introduction
============

This is a short script presenting a simple algorithm to solve sudoku grids in a very fast manner. The algorithm follows four very simple principles:

-   Principle 1: A cell has exactly one value.

-   Principle 2: A row/column/box contains each value exactly once.

-   Principle 3: If there is only one value available to fill a cell, fill the cell with it.

-   Principle 4: If a value has only one location available in a row/column/box, write it there.

It turns out that these four principles are sufficient to solve most sudoku grid. When this is not the case, we just need to guess the value of an empty cell *once* to be able to solve the grid.

Representations
===============

The matrix `grid` and the three-dimensional array `poss` are the building blocks of the algorithm.

The object `grid`
-----------------

The numerical matrix `grid` represents the sudoku grid. Its rows and columns correspond to the rows and columns of the sudoku grid, and its entries indicate the value of the corresponding cell in the sudoku grid. Empty cells are represented with a `0`. For convenience, we create the function `create_grid()`, which, given a vector of `81` values, returns the corresponding `grid`.

``` r
# Function to create grid
create_grid <- function(grid)  matrix(grid, ncol = 9, nrow = 9, byrow = TRUE, dimnames = list(1:9, 1:9))

# Sudoku grid from english Wikipedia page on sudoku (a 0 indicates an empty cell)
grid_wiki <- create_grid(c(5,3,0,0,7,0,0,0,0,
                           6,0,0,1,9,5,0,0,0,
                           0,9,8,0,0,0,0,6,0,
                           8,0,0,0,6,0,0,0,3,
                           4,0,0,8,0,3,0,0,1,
                           7,0,0,0,2,0,0,0,6,
                           0,6,0,0,0,0,2,8,0,
                           0,0,0,4,1,9,0,0,5,
                           0,0,0,0,8,0,0,7,9))
print(grid_wiki)
```

    ##   1 2 3 4 5 6 7 8 9
    ## 1 5 3 0 0 7 0 0 0 0
    ## 2 6 0 0 1 9 5 0 0 0
    ## 3 0 9 8 0 0 0 0 6 0
    ## 4 8 0 0 0 6 0 0 0 3
    ## 5 4 0 0 8 0 3 0 0 1
    ## 6 7 0 0 0 2 0 0 0 6
    ## 7 0 6 0 0 0 0 2 8 0
    ## 8 0 0 0 4 1 9 0 0 5
    ## 9 0 0 0 0 8 0 0 7 9

The object `poss`
-----------------

The three-dimensional logical array `poss` indicates which values are available to fill the cells of the sudoku grid. Its rows (first dimension) and columns (second dimension) correspond to the rows and column of the sudoku grid, and its layers (third dimension) to the nine values (1, 2, 3, 4, 5, 6, 7, 8, 9) available to fill the grid (excluding `0`). The logical entry *poss<sub>i,j,n</sub>* of `poss` indicates whether the number *n* could be used to fill the cell on row *i* and column *j* of the sudoku grid.

The function `create_poss()` generates a `9 x 9 x 9` array of `TRUEs`.

``` r
create_poss <- function() array(TRUE, dim = c(9, 9, 9), dimnames = list(1:9, 1:9, 1:9))

poss <- create_poss()

print(poss[ , , 1])
```

    ##      1    2    3    4    5    6    7    8    9
    ## 1 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 2 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 3 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 4 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 5 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 6 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 7 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 8 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## 9 TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

Solving a sudoku
================

Pseudo-code
-----------

To solve a sudoku grid, we iteratively update the objects `grid` and `poss` until `grid` has no empty cell.

``` r
#   Pseudo-code for solving a sudoku
#
#   while(any(grid == 0)){
#
#     # Use principle 1 and principle 2 to update `poss`
#     poss <- update_poss(grid, poss)
#
#     # Use principle 3 and principle 4 to update `grid`
#     grid <- update_grid(grid, poss)
#
#   }
```

Updating the object `poss`
--------------------------

We start with the object `poss` which we update in four different ways: cell-wise, row-wise, column-wise and box-wise (boxes are the three-by-three subgrids of the main sudoku grid), following principle 1 and principle 2:

-   Principle 1: A cell has exactly one value.

-   Principle 2: A row/column/box contains each value exactly once.

### Cell by cell

Following principle 1, if a cell *cell<sub>i,j</sub>* is filled with a value, then no other value can be used to fill the cell in question. Consequently, we set the entries of `poss` *poss<sub>i,j,n</sub>* corresponding to *cell<sub>i,j</sub>* to `FALSE` for all *n*. The function `update_poss_cell()` accomplishes this. It first finds the locations of filled cells and assigns them to `location_filled` before updating `poss` and returning `poss`. (We use `rep()` on `location_filled` to match the dimensions of `poss`).

``` r
update_poss_cell <- function(poss, grid){
  
  # Location of filled cells
  location_filled <- grid != 0
  location_filled <- array(rep(location_filled, 9), dim = c(9, 9, 9))
  
  
  # Updating poss
  location_empty <- !location_filled
  poss           <- poss & location_empty
  
}


#
# Illustration

# Creating the array poss
poss <- create_poss()

# Updating poss cell-wise
poss <- update_poss_cell(poss, grid_wiki)

# The entries corresponding to filled cells of the sudoku grid are `FALSE`. E.g. cells (1, 3), (1, 7), (1, 8), etc
print(poss[ , , 1])
```

    ##       1     2     3     4     5     6     7     8     9
    ## 1 FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 2 FALSE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 3  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE
    ## 4 FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
    ## 5 FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE
    ## 6 FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE
    ## 7  TRUE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE
    ## 8  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE
    ## 9  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE FALSE

### Row by row, column by column and box by box

Following principle 2, if a cell *cell<sub>i,j</sub>* is filled with a value *n*, then *n* cannot be used to fill any other cell of *cell<sub>i,j</sub>*'s row, column and box. Consequently, we set the entries of poss corresponding to *cell<sub>i,j</sub>*'s row, column and box to `FALSE`. The functions `update_poss_row()`, `update_poss_col()` and `update_poss_box()` accomplish this by looping through each row/column/box of the grid. In each loop, they assign the row/column/box under investigation to `row_grid`, `col_grid` and `box_grid`. They then loop through the values 1, 2, 3, 4, 5, 6, 7, 8, 9. In this second loop, they check if the value is present in the row/column/box under investigation. If this is the case, we update the corresponding entries of `poss` to `FALSE`.

``` r
# Row by row
update_poss_row <- function(poss, grid){
  
  for(row in 1 : 9){
    
    row_grid <- grid[row, ] # row under investigation
    
    for(n in 1 : 9){
      
      # If n present in row, then set corresponding entries of poss to FALSE
      if(any(row_grid == n)){
        
        poss[row, , n] <- FALSE
        
      } 
      
    }
    
  }
  
  return(poss)
  
}


#
# Illustration

# Creating the array poss
poss <- create_poss()

# Updating poss row-wise
poss <- update_poss_row(poss, grid_wiki)

# First layer of poss: rows containing a `1` on the sudoku grid (row 2, row 5 and row 8) are `FALSE`.
print(poss[ , , 1]) 
```

    ##       1     2     3     4     5     6     7     8     9
    ## 1  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 2 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 4  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 5 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 7  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 8 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 9  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

``` r
# Column by column
update_poss_col <- function(poss, grid){
  
  for(col in 1 : 9){
  
    col_grid <- grid[ , col]
    
    for(n in 1 : 9)
      if(any(col_grid == n))  poss[ , col, n] <- FALSE
    
  }
  
  return(poss)
  
}
```

``` r
# Box by box
update_poss_box <- function(poss, grid){
  
  # three column of (three) boxes
  for(col_box in 1 : 3){
    
    # three rows of (three) boxes
    for(row_box in 1 : 3){
      
      # identifying the (3 x 3) subgrid corresponding to the box
      rows <- 1 : 3 + 3 * (row_box - 1)
      cols <- 1 : 3 + 3 * (col_box - 1)
      box_grid <- grid[rows, cols]
      
      for(n in 1 : 9)
        if(any(box_grid == n)) poss[rows, cols, n] <- FALSE
      
    }
    
  }
  
  return(poss)
  
}
```

### The function `update_poss()`

We encapsulate these four functions in `update_poss()` for convenience.

``` r
update_poss <- function(poss, grid){
  
  poss %>%
    update_poss_cell(grid) %>%
    update_poss_row(grid) %>%
    update_poss_col(grid) %>%
    update_poss_box(grid)
  
}


#
# Illustration

# Creating poss
poss <- create_poss()

# Updating poss
poss <- update_poss(poss, grid_wiki)

# filled cells, and rows, columns and boxes containing a `1` are `FALSE` on layer `1`
print(poss[ , , 1]) 
```

    ##       1     2     3     4     5     6     7     8     9
    ## 1 FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE
    ## 2 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## 4 FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
    ## 5 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6 FALSE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE
    ## 7  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 8 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 9  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE FALSE FALSE

Updating `grid`
---------------

Now that we have updated `poss`, we can use it to update `grid` following principle 3 and principle 4:

-   Principle 3: If there is only one value available to fill a cell, fill the cell with it.

-   Principle 4: If a value has only one location available in a row/column/box, write it there.

Similarly to how we udpate `poss`, we update `grid` cell-wise, row-wise, column-wise and box-wise. Following principle 3, the function `update_grid_cell()` checks for each cell (loop) how many values are still available to fill it. If there is only one value available, then we fill the cell with it. Following principle 4, the functions `update_grid_row()`, `update_grid_col()` and `update_grid_box()` identify for each value 1, 2, 3, 4, 5, 6, 7, 8, 9 (loop) their possible locations in each row/column/box. If a value has only one location available in a row/column/box, then we write it there.

### Cell by cell

``` r
update_grid_cell <- function(grid, poss){
  
  for(row in 1 : 9){
    
    for(col in 1 : 9){
      
      # Check if cell is empty
      if(grid[row, col] == 0){
        
        # Identify available values
        values_available   <- poss[row, col, ]
        n_values_available <- sum(values_available)
        
        # if only one value available, write it there
        if(n_values_available == 1){
          
          grid[row, col] <- c(1:9)[values_available]
          
        }
        
      }
      
    }
    
  }
  
  return(grid)
  
}


#
# Illustration

# Updating the sudoku grid from wiki cell-wise
grid_update <- update_grid_cell(grid_wiki, poss)

# grid before update
print(grid_wiki)
```

    ##   1 2 3 4 5 6 7 8 9
    ## 1 5 3 0 0 7 0 0 0 0
    ## 2 6 0 0 1 9 5 0 0 0
    ## 3 0 9 8 0 0 0 0 6 0
    ## 4 8 0 0 0 6 0 0 0 3
    ## 5 4 0 0 8 0 3 0 0 1
    ## 6 7 0 0 0 2 0 0 0 6
    ## 7 0 6 0 0 0 0 2 8 0
    ## 8 0 0 0 4 1 9 0 0 5
    ## 9 0 0 0 0 8 0 0 7 9

``` r
# grid after update
print(grid_update)
```

    ##   1 2 3 4 5 6 7 8 9
    ## 1 5 3 0 0 7 0 0 0 0
    ## 2 6 0 0 1 9 5 0 0 0
    ## 3 0 9 8 0 0 0 0 6 0
    ## 4 8 0 0 0 6 0 0 0 3
    ## 5 4 0 0 8 5 3 0 0 1
    ## 6 7 0 0 0 2 0 0 0 6
    ## 7 0 6 0 0 0 7 2 8 4
    ## 8 0 0 0 4 1 9 0 3 5
    ## 9 0 0 0 0 8 0 0 7 9

``` r
# 4 cells have been filled {(5,5), (7,6), (7,9), (8,8)}
sum(grid_update != grid_wiki)
```

    ## [1] 4

### Row by row, column by column and box by box

``` r
update_grid_row <- function(grid, poss){
  
  for(row in 1 : 9){
    
    for(n in 1 : 9){
      
      locations   <- poss[row, , n]
      n_locations <- sum(locations)
      
      if(n_locations == 1) grid[row, locations] <- n
      
    }
    
  }
  
  return(grid)
  
}
```

``` r
update_grid_col <- function(grid, poss){
  
  for(col in 1 : 9){
    
    for(n in 1 : 9){
      
      locations   <- poss[ , col, n]
      n_locations <- sum(locations)
      
      if(n_locations == 1) grid[locations, col] <- n
      
    }
    
  }
  
  return(grid)
  
}
```

``` r
update_grid_box <- function(grid, poss){
  
  for(col_box in 1 : 3){
    
    for(row_box in 1 : 3){
      
      rows <- 1 : 3 + 3 * (row_box-1)
      cols <- 1 : 3 + 3 * (col_box-1)
      box  <- grid[rows, cols]
      
      for(n in 1 : 9){
        
        locations   <- poss[rows, cols, n]
        n_locations <- sum(locations)
        
        if(n_locations == 1) grid[rows, cols][locations] <- n
        
      }
      
    }
    
  }
  
  return(grid)
  
}
```

### The function `update_grid()`

We encapsulate these four updating functions in `update_grid` for convenience.

``` r
update_grid <- function(grid, poss){
  
   grid %>%
     update_grid_cell(poss) %>%
     update_grid_row(poss) %>%
     update_grid_col(poss) %>%
     update_grid_box(poss)
  
}


#
# Illustration

# Updating grid
grid_update <- update_grid(grid_wiki, poss)

# 16 cells have been filled
sum(grid_wiki != grid_update)
```

    ## [1] 16

``` r
# There are still 35 empty cells...
sum(grid_update == 0)
```

    ## [1] 35

Solving our first sudoku grid
-----------------------------

Now that we are equipped with `update_poss()` and `update_grid()`, we can write a function that iteratively updates `poss` and `grid` until the sudoku grid is complete. We add a safeguard in our function to avoid the loop to continue for ever in case `update_grid()` fails to find a cell to update (which can happen for difficult grids, in which case we need to *guess* the values of a cell, see following section)

``` r
solve_sudoku <- function(grid){
  
  
  #
  # Setup
  poss <- update_poss(create_poss(), grid)
  
  
  #
  # Loop
  while(any(grid == 0)){
    
    # For convenience
    print(paste(sum(grid == 0), "empty cells left."))
    
    # Update grid
    grid_update <- update_grid(grid, poss)
    
    # Check if algorithm is stuck (safeguard)
    if(all(grid == grid_update)){ # if no cell updated, stop algorithm
      
      return(print("Algorithm stuck"))

    }else{ # otherwise, continue with algorithm 
      
      grid <- grid_update
      poss <- update_poss(poss, grid)
      
    }
    
  } # close while-loop
  
  
  #
  # Output
  print("Grid solved")
  return(grid)
  
}
```

Let us solve the sudoku grid from the Wikipedia page with our algorithm.

``` r
# Solving our first grid
grid <- create_grid(c(5,3,0,0,7,0,0,0,0,
                      6,0,0,1,9,5,0,0,0,
                      0,9,8,0,0,0,0,6,0,
                      8,0,0,0,6,0,0,0,3,
                      4,0,0,8,0,3,0,0,1,
                      7,0,0,0,2,0,0,0,6,
                      0,6,0,0,0,0,2,8,0,
                      0,0,0,4,1,9,0,0,5,
                      0,0,0,0,8,0,0,7,9))

solve_sudoku(grid)
```

    ## [1] "51 empty cells left."
    ## [1] "35 empty cells left."
    ## [1] "14 empty cells left."
    ## [1] "5 empty cells left."
    ## [1] "1 empty cells left."
    ## [1] "Grid solved"

    ##   1 2 3 4 5 6 7 8 9
    ## 1 5 3 4 6 7 8 9 1 2
    ## 2 6 7 2 1 9 5 3 4 8
    ## 3 1 9 8 3 4 2 5 6 7
    ## 4 8 5 9 7 6 1 4 2 3
    ## 5 4 2 6 8 5 3 7 9 1
    ## 6 7 1 3 9 2 4 8 5 6
    ## 7 9 6 1 5 3 7 2 8 4
    ## 8 2 8 7 4 1 9 6 3 5
    ## 9 3 4 5 2 8 6 1 7 9

More complex cases
------------------

Let us try our algorithm on a more difficult sudoku grid: a grid with only 17 starting values (minimum number of starting values necessary to have a unique solution).

``` r
# Sparsest sudoku grid possible
x <- c(0,0,0,0,0,0,0,1,0,
       0,0,0,0,0,2,0,0,3,
       0,0,0,4,0,0,0,0,0,
       0,0,0,0,0,0,5,0,0,
       4,0,1,6,0,0,0,0,0,
       0,0,7,1,0,0,0,0,0,
       0,5,0,0,0,0,2,0,0,
       0,0,0,0,8,0,0,4,0,
       0,3,0,9,1,0,0,0,0)

# Only 17 starting values...
sum(x != 0)
```

    ## [1] 17

``` r
grid_sparse <- create_grid(x)

solve_sudoku(grid_sparse)
```

    ## [1] "64 empty cells left."
    ## [1] "59 empty cells left."
    ## [1] "55 empty cells left."
    ## [1] "51 empty cells left."
    ## [1] "46 empty cells left."
    ## [1] "43 empty cells left."
    ## [1] "42 empty cells left."
    ## [1] "39 empty cells left."
    ## [1] "33 empty cells left."
    ## [1] "27 empty cells left."
    ## [1] "23 empty cells left."
    ## [1] "18 empty cells left."
    ## [1] "14 empty cells left."
    ## [1] "8 empty cells left."
    ## [1] "3 empty cells left."
    ## [1] "Grid solved"

    ##   1 2 3 4 5 6 7 8 9
    ## 1 7 4 5 3 6 8 9 1 2
    ## 2 8 1 9 5 7 2 4 6 3
    ## 3 3 6 2 4 9 1 8 5 7
    ## 4 6 9 3 8 2 4 5 7 1
    ## 5 4 2 1 6 5 7 3 9 8
    ## 6 5 8 7 1 3 9 6 2 4
    ## 7 1 5 8 7 4 6 2 3 9
    ## 8 9 7 6 2 8 3 1 4 5
    ## 9 2 3 4 9 1 5 7 8 6

Our algorithm takes more iterations, but it still solves the grid nonetheless. Impressive!

Yet, for difficult grids, our algorithm may fail to find a cell to fill. In such cases, the algorithm is stuck and fails to solve the sudoku grid.

``` r
# Grid with "evil" level (from https://www.websudoku.com/?level=4&set_id=4360842130)
grid_evil <- create_grid(c(0,1,0,0,4,5,0,0,0,
                           0,0,0,0,0,0,7,0,6,
                           0,0,5,2,0,0,0,0,4,
                           0,9,0,0,7,0,0,8,2,
                           0,0,0,6,0,1,0,0,0,
                           4,8,0,0,3,0,0,7,0,
                           8,0,0,0,0,9,4,0,0,
                           7,0,9,0,0,0,0,0,0,
                           0,0,0,3,5,0,0,6,0))

# Algorithm fails to find a cell to fill on the 5th iteration...
solve_sudoku(grid_evil)
```

    ## [1] "55 empty cells left."
    ## [1] "50 empty cells left."
    ## [1] "47 empty cells left."
    ## [1] "46 empty cells left."
    ## [1] "Algorithm stuck"

Algorithm stuck? Take a guess!
==============================

When the algorithm is stuck, we need to *guess* the value of a cell.

Taking a guess
--------------

The function `guess()` guesses the value of an empty cell. We use `guess()` when our algorithm is stuck. To minimze the risk of making a wrong guess, we identify a cell with the smallest number of values available (ideally only two values available so tath we guess the right number half of the time). The function `guess()` first assigns the number of values available per cell to `n_possible`. Next, it loops through each cell to find one that is both empty and has the minimum number of values available. We fill the first cell meeting these two conditions with one of its possible values (randomly chosen), and immediatly return the grid to minimize the risk of making a wrong guess.

``` r
guess <- function(grid, poss){
  
  # Number of values available per cell
  n_available       <- rowSums(poss, dims = 2)
  
  # find minimum number among empty cells 
  n_available_empty <- n_available[grid == 0]
  n_min             <- min(n_available_empty)
  
  for(row in 1 : 9){
    
    for(col in 1 : 9){
      
      # find a cell that is empty and has the minimum number of values available
      if(grid[row, col] == 0  &  sum(poss[row, col, ]) == n_min){
        
        # randomly choose one of the values available and fill the cell with it.
        values_available <- c(1:9)[poss[row, col, ]]
        guess            <- sample(values_available, size = 1)
        grid[row, col]   <- guess
        
        # return grid after first guess to minimize risk of making an error
        return(grid)
      }
    }
  }
}
```

Checking a grid
---------------

Guessing the value of a cell opens the door to errors. After a guess, we must check that the grid is still valid. Let us design a function `check_grid()` that checks if a sudoku grid is valid, using two criteria:

Criteria 1: each empty cell must have at least one value availble.

Criteria 2: each value that is not present in a row/column/box must have at least one location available in the row/column/box.

The code of the function `check_grid()` is very similar to code of the functions `update_poss()` and `update_grid()`. If the grid satisfies the two criteria, then the function returns a `TRUE`, otherwise, it returns a `FALSE`.

``` r
check_grid <- function(grid, poss){

  
  #
  # Criteria 1: each empty cell must have at least one value availble.
  
  for(row in 1 : 9){
    for(col in 1 : 9){
      # If cell is empty and no value available, then grid contains an error.
      if(grid[row, col] == 0  &  all(poss[row, col, ] == FALSE))  return(FALSE)
    }
  }
  
  
  #
  # Criteria 2: each value not present in a row/column/box must have at least one location available in the row/column/box
  
  # check each row
  for(row in 1:9){
    row_grid <- grid[row, ]
    for(n in 1:9){
      # If value absent from row and no location available, then grid contains an error.
      if(all(row_grid != n)  &  all(poss[row, , n] == FALSE))  return(FALSE)
    }
  }

  # check each column
  for(col in 1:9){
    col_grid <- grid[ , col]
    for(n in 1:9){
      if(all(col_grid != n)  &  all(poss[ , col, n] == FALSE))  return(FALSE)
    }
  }

  # check each box
  for(col_box in 1 : 3){
    for(row_box in 1 : 3){
      rows     <- 1 : 3 + 3 * (row_box-1)
      cols     <- 1 : 3 + 3 * (col_box-1)
      box_grid <- grid[rows, cols]
      for(n in 1:9){
        if(all(box_grid != n)  &  all(poss[rows, cols, n] == FALSE))  return(FALSE)
      }
    }
  }
  
  
  #
  # if criteria 1 and criteria 2 are satisfied, then grid is ok
  return(TRUE)
  
}
```

Solving a sudoku grid with guess
--------------------------------

Let us include the functions `guess()` and `check_grid()` to our algorithm, and try to solve the "evil" grid.

``` r
solve_sudoku <- function(grid){
  
  
  #
  # Setup
  poss          <- update_poss(poss = create_poss(), grid)
  grid_original <- grid # save grid to be abe to start over in case we make a wrong guess
  n_wrong_guess <- 0    # keep track of number of wrong guesses
  has_guessed   <- FALSE
  
  
  #
  # Loop
  while(any(grid == 0)){
    
    # Update grid
    grid_update <- update_grid(grid, poss)
    
    if(all(grid == grid_update)){ # If algorithm stuck, make a guess
      
      grid_update <- guess(grid, poss)
      has_guessed <- TRUE
      
    }
    
    # Update poss
    poss <- update_poss(poss, grid_update)

    # check validity of grid
    if(check_grid(grid_update, poss)){ # If grid valid, continue with algorithm
      
      grid <- grid_update
      
    }else{ # else, start over
      
      grid <- grid_original
      poss <- update_poss(poss = create_poss(), grid)
      n_wrong_guess <- n_wrong_guess + 1
      
    }
    
    
    #
    # Safeguard
    if(n_wrong_guess >= 100) return(print("Too many guesses: impossible grid"))

  } # end while-loop
  
  
  #
  # Output
  if(has_guessed) print(paste("Grid solved after", n_wrong_guess, "wrong guesses."))
  else            print("Grid solved without guess")
  
  return(grid)
  
}
```

Now, our algorithm solves the "evil" sudoku grid seamlessly. In fact, I have tried to solve numerous sudoku grids with the algorithm and always succeeded. In the worst case, the algorithm makes a few wrong guesses (usually only one or two) before solving the grid.

``` r
set.seed(123)
x <- replicate(n = 10, solve_sudoku(grid_evil))
```

    ## [1] "Grid solved after 1 wrong guesses."
    ## [1] "Grid solved after 1 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 1 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 1 wrong guesses."
    ## [1] "Grid solved after 1 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 1 wrong guesses."

Testing the algorithm: extreme cases
------------------------------------

Surprisingly, even if the sudoku grid is empty, our algorithm manages to generate a valid solution very quickly. This indicates that solving sparse grids is not an issue for the algorithm.

``` r
# Empty grid
grid_empty <- create_grid(0)
set.seed(123)
x <- replicate(n = 10, solve_sudoku(grid_empty))
```

    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."
    ## [1] "Grid solved after 0 wrong guesses."

Furthermore, if we give an impossible sudoku grid to the algorithm, the safeguard prevents the algorithm from looping forever.

``` r
# Impossible grid
grid_impossible <- create_grid(c(1,1, rep(0,79)))
print(grid_impossible)
```

    ##   1 2 3 4 5 6 7 8 9
    ## 1 1 1 0 0 0 0 0 0 0
    ## 2 0 0 0 0 0 0 0 0 0
    ## 3 0 0 0 0 0 0 0 0 0
    ## 4 0 0 0 0 0 0 0 0 0
    ## 5 0 0 0 0 0 0 0 0 0
    ## 6 0 0 0 0 0 0 0 0 0
    ## 7 0 0 0 0 0 0 0 0 0
    ## 8 0 0 0 0 0 0 0 0 0
    ## 9 0 0 0 0 0 0 0 0 0

``` r
set.seed(123)
solve_sudoku(grid_impossible)
```

    ## [1] "Too many guesses: impossible grid"

Conclusion
==========

In this script, I develop a simple algorithm to solve sudoku grids in a very fast manner. The algorithm follows four very simple principles:

-   Principle 1: A cell has exactly one value.

-   Principle 2: A row/column/box contains each value exactly once.

-   Principle 3: If there is only one value available to fill a cell, fill the cell with it.

-   Principle 4: If a value has only one location available in a row/column/box, write it there.

Despite its simplicity, the algorithm can solve most sudoku grids, even the most sparse ones!

Yet, for difficult grids, the algorithm can get stuck. When the algorithm is stuck, we need to guess the value of a cell. I updated the original algorithm so that, when it is stuck, it guesses the value of a cell and subsequently checks the validity of the obtained grid. This version of the algorithm solves even the most difficult grid after a few iterations.

This shows that, for most sudoku grids, applying these four simple (almost trivial) principles in a *systematic* way lead to a solution. When these principles are not sufficient, we just have to make one or two guesses. Who knew sudoku was so simple?
