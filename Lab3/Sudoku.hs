module Sudoku where

import Test.QuickCheck
import Data.Maybe(isJust, isNothing, fromJust)
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
isSudoku sud = all (==True) [boardRows sud, rowLength sud]
      where  
        -- checks if number of rows is 9
        boardRows sud = length (rows sud) == 9
        
        -- check if number of elements in rows are 9
        rowLength sud = 
          all (==True) [length sudRows == 9 
                        && all (==True) (cellType sudRows) 
                        | sudRows <- rows sud]

        -- check if cells contains "Nothing" or "just _"
        cellType sudRows = [isNothing(c) || isJust(c) | c <- sudRows]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = checkboard
  where checkboard = all (==True) [all (==True) (checkRow r) | r <- rows sud]
        checkRow row = [isJust(c) | c <- row]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = do
  putStrLn (concat (concat board))
  where
    board = [cell row ++ ["\n"] | row <- rows sud]
    cell row = [if isJust(c) then show(fromJust(c)) else ['.'] | c <- row ]


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku = undefined
-- readSudoku path = do
--   sud <- readFile path
--   let rowsOfStrings = lines sud
--   return result
--   where
--       rowStrings rowsOfStrings = rowsOfStrings
--       board = Sudoku [sudRows rowString | rowString <- rowStrings]
--       sudRows rowString = [if c == '.' then Nothing else Just (digitToInt c) | c <- rowString ]
--       result | isSudoku board == True = board
--              | otherwise = error "Not a Sudoku!"


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
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
prop_Sudoku sud = isSudoku sud == True
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
blocks sud = blockRows ++ blockColumns ++ blocks
  where
    blockRows    = rows sud
    blockColumns = transpose $ rows sud
    blocks = map concat [topLeft,    topMiddle,    topRight,
                         middleLeft, middle,       middleRight,
                         bottomLeft, bottomMiddle, bottomRight]

    (topRows, restRows) = split' $ rows sud
    (topLeft, restTopRight) = split' (transpose topRows)
    (topMiddle, topRight)  = split' restTopRight
    
    (middleRows, bottomRows) = split' restRows
    
    (middleLeft, restMiddleRight) = split' (transpose middleRows)
    (middle, middleRight)  = split' restMiddleRight
    
    (bottomLeft, restBottomRight) = split' (transpose bottomRows)
    (bottomMiddle, bottomRight)  = split' restBottomRight

    split' :: [Row] -> ([Row],[Row])
    split' = splitAt 3

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length allBlocks == 27 && eachBlock
  where
    eachBlock = all (==True) [length b == 9 | b <- allBlocks]
    allBlocks = blocks sud

-- isOkayBlock :: Block -> Bool
-- isOkayBlock block = length newBlock == length (nub newBlock)
--   where
--     newBlock = [c | c <- block, isJust c]

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud = all (==True) [isOkayBlock b | b <- blocks sud]

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
