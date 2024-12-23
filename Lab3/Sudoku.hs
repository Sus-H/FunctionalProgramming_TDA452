module Sudoku where

import Test.QuickCheck
import Data.Maybe(isJust, isNothing, fromJust, listToMaybe, catMaybes)
import Data.Char
import Data.List

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
  Sudoku
    [ [j 5, j 3, j 4, j 6, j 7, j 8, j 9, j 1, j 2]
    , [j 6, j 7, j 2, j 1, j 9, j 5, j 3, j 4, j 8]
    , [j 1, j 9, j 8, j 3, j 4, j 2, j 5, j 6, j 7]
    , [j 8, j 5, j 9, j 7, j 6, j 1, j 4, j 2, j 3]
    , [j 4, j 2, j 6, j 8, j 5, j 3, j 7, j 9, j 1]
    , [j 7, j 1, j 3, j 9, j 2, j 4, j 8, j 5, j 6]
    , [j 9, j 6, j 1, j 5, j 3, j 7, j 2, j 8,  n ]
    , [j 2, j 8, j 7, j 4, j 1, j 9, j 6, j 3, j 5]
    , [j 3, j 4, j 5, j 2, j 8, j 6, j 1, j 7, j 9]
    ]
    where
      n = Nothing
      j = Just

-- A sample sudoku where all entries are a 1
tester :: Sudoku
tester = Sudoku [[Just 1 | x <- [1..9]] | y <- [1..9]]

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x <- [1..9]] | y <- [1..9]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = boardRows sud && rowLength sud
      where  
        -- checks if number of rows is 9
        boardRows sud = length (rows sud) == 9
        
        -- check if number of elements in rows are 9
        rowLength sud = 
          and [length sudRows == 9 
                        && and (cellType sudRows) 
                        | sudRows <- rows sud]

        -- check if cells contains "Nothing" or a number between 1 and 9
        cellType sudRows = [isNothing c || 
                           (getC c >= 1 && getC c <= 9) | c <- sudRows]
        getC = fromJust

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = checkboard
  where checkboard = and [and (checkRow r) | r <- rows sud]
        checkRow row = [isJust c | c <- row]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = do
  putStrLn (concat (concat board))
  where
    board = [cells row ++ ["\n"] | row <- rows sud]
    cells row = [if isJust c then show(fromJust c) else ['.'] | c <- row ]


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
-- readSudoku = undefined
readSudoku path = do
  sud <- readFile path
  let 
    rowsOfStrings = lines sud
    board = Sudoku [sudRows rowString | rowString <- rowsOfStrings]
    sudRows rowString = [if c == '.' then Nothing 
                         else Just (digitToInt c) | c <- rowString ]
    result | isSudoku board = board
           | otherwise = error "Not a Sudoku!"
    in return result
    -- in printSudoku $ result

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(1, cellNumber),(9, return Nothing)]
    where
      cellNumber = do
        n <- choose(1,9)
        return $ Just n

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    s <- vectorOf 9 (vectorOf 9 cell)
    return (Sudoku s)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku 
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = length newBlock == length (nub newBlock)
  where
    newBlock = [c | c <- block, isJust c]
    
-- * D2

blocks :: Sudoku -> [Block]
blocks sud = blockRows ++ blockColumns ++ squareBlocks
  where
    blockRows    = rows sud
    blockColumns = transpose $ rows sud
    squareBlocks = map concat (concat [block topRows, 
                                       block middleRows, 
                                       block bottomRows])

    (topRows, restRows) = splitAt 3 $ rows sud
    (middleRows, bottomRows) = splitAt 3 restRows

block :: [Row] -> [[Block]]
block rowOfBlocks = [left, middle, right]
  where
    (left, restRight) = splitAt 3 (transpose rowOfBlocks)
    (middle, right)  = splitAt 3 restRight

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length allBlocks == 27 && eachBlock
  where
    eachBlock = and [length b == 9 | b <- allBlocks]
    allBlocks = blocks sud

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock b | b <- blocks sud]

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom right corner
type Pos = (Int,Int)

-- * E1

posMatrix :: [(Int,Int)]
posMatrix = [(a,b) | a <- [0..8], b <- [0..8]]

blanks :: Sudoku -> [Pos]
blanks sud = positions
  where
    cells = concat $ rows sud
    pairs = zip posMatrix cells
    positions = [fst pair | pair <- pairs, isNothing (snd pair)]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == posMatrix

-- * E2

-- Writes a cell to a position
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _     = error "Empty List"
xs !!= (i,y) | i >= length xs || i < 0 = error "Index out of bounds B)"
             | otherwise = take i xs ++ [y] ++ drop (i+1) xs


prop_bangBangEquals_correct :: [Int] -> (Int,Int) -> Bool
prop_bangBangEquals_correct [] _      = True
prop_bangBangEquals_correct l (i,y) | i' < length l = 
  updated !! i' == y && length updated == length l
                                    | otherwise       = False                            
  where
    i' = abs i `mod` length l
    updated = l !!= (i',y)


-- * E3

-- Updates a specific cell in a sudoku
update :: Sudoku -> Pos -> Cell -> Sudoku
update sud (a,b) c | a < 9 && b < 9 && a >= 0 && b >= 0 = 
  Sudoku (take a sudoku ++ updatedRow ++ drop (a+1) sudoku)
                   | otherwise = error "Invalid input"
  where
    -- sudoku is a [[rows]]
    sudoku = rows sud
    -- row = get the row we want to update => [Row] = [[Cell]]
    row = sudoku !! a
    -- use our function to change 1 cell => [[Cell]]
    updatedRow = [row !!= (b, c)]

sudIndex :: Sudoku -> Pos -> Cell
sudIndex sud (a,b) = c
  where
    sudoku = rows sud
    row = sudoku !! a
    c = row !! b

-- Check that the sudoku is updated properly when using the function update
prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated sud (a,b) c = newCell == c
  where
    a' = abs a `mod` 9
    b' = abs b `mod` 9
    updated = update sud (a', b') c
    sudoku = rows updated
    rowUpdated = sudoku !! a'
    newCell = rowUpdated !! b'
  

------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve unsolvedSud | not (isSudoku unsolvedSud) = Nothing
                  | otherwise = solved $ solve' unsolvedSud unsolvedPos
  where
    unsolvedPos = blanks unsolvedSud
    solved []     = Nothing
    solved (s:ss) = s

solve' :: Sudoku -> [Pos] -> [Maybe Sudoku]
solve' sud [] | isOkay sud =  [Just sud]
              | otherwise = []
solve' sud (x:xs) | isOkay sud = maybeOkaySuds
                  | otherwise = []
  where
    maybeOkaySuds = concat [solve' (update sud x (Just n)) xs | n <- [1..9]]


-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve filePath = do
  sud <- readSudoku filePath
  let solution = fromJust $ solve sud
  printSudoku solution

-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sud1 sud2 | isSudoku sud1 && isSudoku sud2 
                        && isOkay sud1 && null (blanks sud1) = isTrue
                       | otherwise = False
                       
  where
    -- get a list of all positions in a sudoku
    allEmpty = posMatrix 
    -- get all blank pos in the sudoku
    blankPositions = blanks sud2
    -- keep all the non-empty pos form the sudoku
    nonBlankPos = [filledPos | filledPos <- allEmpty, 
                               filledPos `notElem` blankPositions ]
    isTrue = and [sudIndex sud1 t == sudIndex sud2 t | t <- nonBlankPos]

-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = (isSudoku sud && isOkay sud) 
                        ==> case solved of 
                          Nothing -> True
                          (Just s) -> isSolutionOf s sud 
  where
    solved = solve sud

fewerChecks :: Testable prop => prop -> IO ()
fewerChecks =
  quickCheckWith stdArgs{maxSuccess=30 }