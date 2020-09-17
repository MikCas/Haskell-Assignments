--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where
import Data.List
--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can
-- have on a row or column total.
data Action
    = Add Int
    | Sub Int
    deriving (Eq, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = Cell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = Row Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = Grid [Int] [Row]
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | `eval` @action total@ applies @action@ to the running @total@.
eval :: Action -> Int -> Int
--Using pattern matching the eval function adds or subtracts the corresponding inputs
eval (Add x) y = x + y
eval (Sub x) y = y - x

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@
-- if @cell@ is enabled
apply :: Cell -> Int -> Int
--By checking the if the cell is enabled or not we can evaluate the action in the cell
apply (Cell bool action) x | bool == True = eval (action) x
                           | otherwise = x

-- | `result` @cells@ calculates the total produced by the actions of all
-- enabled cells in @cells@ starting from 0.
--Using foldr we can go add up each element of cells using apply
--Hence when an element is not enabled it is not added to the result with the apply function
result :: [Cell] -> Int
result cells = foldr apply 0 cells

-- | `solveRow` @row@ finds solutions for @row@.
--The row solutions which were obtained using correctCellsSolution function are now changed to type Row
solveRow :: Row -> [Row]
--Change initial rows to have false cells
--The list containing all solved rows is formed by using the defined rowSolutions function
solveRow (Row x cells) = [Row x cellsSolutions| cellsSolutions <- rowSolutions (correctCells (Row x falseCells)) (Row x falseCells)]
                        where falseCells = map (\(Cell _ action) -> Cell False action) cells

--All the possible row combinations are obtained and the combinations which add up to the row value
correctCells :: Row -> [[(Int, Cell)]]
--Need to fix result here as well
--cellResult is a function which acts like the result function however accounts for the fact that Cell objects are now tuples
--cellsZip used to uniquely identify cells within the list, thus removing the problem of having to deal with duplicate values
--allRowSubsequences contains all the possible subsequences of the row where each cell is mapped onto an integer which determines the position in the row
--and all the cells are enabled
correctCells (Row x cells) = filter (cellResult) (allRowSubsequences)
                            where cellResult cellsZip = if result cellsNoZip == x then True else False
                                                        where cellsNoZip = map (\(_, c) -> c) cellsZip
                                  allRowSubsequences = (subsequences (map (\(y, Cell _ action) -> (y, Cell True action)) (zip [0..] cells)))

--This function is used to enable row solutions
--If a cell is enabled in correctCells then the cell is enabled when obtaining the row solutions contianing all the disabled cells as well
replaceFalseCell :: (Int, Cell) -> [Cell] -> [Cell]
replaceFalseCell (x, cell) cells = cell1 ++ [cell] ++ cell2
                                  where (cell1, _:cell2) = splitAt (x) cells

--Function used to obtain all the cells which are solutions of the current row
--The function uses replaseFalseCell to set all the cells which are a solution to True
--Hence we are going through each enabled cell and setting the corresponding cell in the initial cell list to True
--This is repeated for all the solutions of the row
rowSolutions :: [[(Int, Cell)]] -> Row -> [[Cell]]
rowSolutions allCellsZip (Row _ initialCells) = [ foldr replaceFalseCell initialCells cellsZip | cellsZip <- allCellsZip]

-- | `solve` @grid@ finds all solutions for @grid@.
solve :: Grid -> [Grid]
--By filtering the row soltions which give a valid column solution we can obtain solutions for the grid
--The sequence function here acts the same as a permutation function for the given solved rows
solve (Grid xs rows) = map rowsToGrid (filter (validate) (sequence (solvedRows)))
                       where solvedRows = map (\x -> solveRow x) rows
                             validate rowsParam = validColumns rowsParam xs
                             rowsToGrid rowsParam = Grid xs rowsParam

--This function will be used to filter the permutations and see if the columns of the current grid are also valid
validColumns :: [Row] -> [Int] -> Bool
--Check if the solutions of the column are non-empty (If one is empty then this means that the column has no solutions)
--Only columns which are all true admit a valid grid, hence those columns will outut a true boolean value
validColumns rows xs = and (checkSolution (swapColsRows rows xs))
                      --Checks if the current row has a solution or not, and outputs a corresponding boolean value
                      where checkSolution columns = map (\column -> not (null (solveRow column))) columns

--By using this function, one is able to transpose a list of rows/columns to a list of columns,rows
--This function works by obtaining the nth element of each row and adding them to a list which hence creates a row
--This row is then zipped with the row value and so a list of rows is created which can be formed into a grid
swapColsRows :: [Row] -> [Int] -> [Row]
swapColsRows rows xs = zipWith cellsToRows xs [map (!! pos) (rowsToCells rows) | pos <- [0 .. ((length xs) - 1)]]
                       where rowsToCells rowsParam = map (\(Row _ cells) -> cells) rowsParam
                             cellsToRows x cells = Row x cells

-- | `rotations` @grid@ returns a list of grids containing all possible ways
-- to rotate @grid@. This means the resulting list should normally have
-- rows + columns many elements.
rotations :: Grid -> [Grid]
rotations (Grid xs initialRows) = gridRowRotation (grid) ++ gridColumnRotation (grid)
                                  where grid = Grid xs initialRows

--This function rotates a row by 1 rotation from the left to the right
rotateRow :: Row -> Row
rotateRow (Row x cells) = (\cs -> Row x cs) (drop 1 (take incrementedLength (cycle cells)))
                                    where incrementedLength = length (cells) + 1

--This function will output a list of all the row rotations of the given grid
--These rows are mapped to an index for easier computation
allGridRotations :: [Row] -> [(Row, Int)]
allGridRotations rows = zip [rotateRow row | row <- rows] [0..]

--Function which swaps grid row with an indexed row
gridReplaceRow :: Grid -> (Row, Int)-> Grid
--Split up the the rows of the grid and remove the row in the nth position
--Add the row to be replaced instead, and output the new list of rows as a grid
gridReplaceRow (Grid xs initialRows) (row, rowPosition) = Grid xs (rows1 ++ [row] ++ rows2)
                                                          where (rows1, _:rows2) = splitAt rowPosition initialRows

--Rotate the grid and obtain rotations for the rows
gridRowRotation :: Grid -> [Grid]
--By going through all the rotations of the given grid rows, replace all the rows in the initial grid to that of the rotated grid
--Hence the list will end up having a grid with each rotation
--Need to chaneg the way ot rotate the input to only give 1 rotation
gridRowRotation (Grid xs initialRows) = [gridReplaceRow (Grid xs initialRows) rowRotations| rowRotations <- allRotations]
                                        where allRotations = allGridRotations initialRows

gridColumnRotation :: Grid -> [Grid]
--This is basically the same as gridRowRotation but for columns
--First we change the grid into the columns and then we obtain all the grid rotations for the columns
--Then we swap back all the column rotations back into rows, and so these are the grid rotations for when a column is rotated
gridColumnRotation (Grid xs rows) = map (\x -> Grid xs x) ( map (swapGridColumns) (gridRowRotation (Grid rowValues (swapColsRows rows xs))))
                                    where rowValues = map (\(Row x _) -> x) rows
                                          swapGridColumns (Grid _ rowsParam) = swapColsRows rowsParam rowValues

-- | `steps` @grid@ finds the sequence of rotations that lead to a solution
-- for @grid@ in the fewest number of rotations. The resulting list includes
-- the solution as the last element.
steps :: Grid -> [Grid]
steps grid = undefined
--The functions below are partially implemented but the reasonsing behind the implementation shoudl give a valid solution to a grid
--Basically this function works by obtaining all the rotations of a given grid, then checks all the grids and sees if any of them are solvable
--If a grid is solvable then the the function stops and outputs the grid solution, all while outputting as well the previous steps needed to obtain the current solution
--If none of the grids are solvable in a current rotation, go through each rotation and perform an new rotation on them and check if any lof the new rotations are solvable, then repeat

newSteps :: [Grid] -> [[Grid]]
--If a solution exists then find it
newSteps grids = if (or (map checkSolves (concatenatedGrids))) then (stepsSolution (concatRotations grids)) else concatMap newSteps concatenatedGrids
               where concatenatedGrids = concatRotations grids

--Function which outputs the list of grids which are a solution
stepsSolution :: [[Grid]] -> [[Grid]]
stepsSolution gridss = filter checkSolves gridss

--Function which concatenates the current grid list with the rotations of the last grid
--Hence we obtain a list of grids which all have a new rotation of the previous grid at the end
concatRotations :: [Grid] -> [[Grid]]
concatRotations gridList = sequence ([gridList] ++ [rotations lastGrid])
                               where lastGrid = last gridList
--concatMap (\x -> gridList ++ x) (rotations grids)
--where gridList = grids:grid

--Given a grid list we can check if the last element of the grid can be solved
--If there is a solution for the last row then output True else False
checkSolves :: [Grid] -> Bool
checkSolves grids = not (null (solve (lastGrid)))
                    where lastGrid = last grids


--------------------------------------------------------------------------------
