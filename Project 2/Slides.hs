
-- CPSC 312 - Project 2

{-
   Byron Duenas
   34095117
   v5e8

   Tongli Li
   15688112
   w6d8
-}

-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--       W is a piece of the White player
--       B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements
-- representing what a point is occupied by
-- where the first element represents a piece
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
--
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--       newBoard is the next board to add to the tree
--       seenBoards is the updated history to avoid possible future trouble boards
--       cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
--       board is the game state at that node
--       nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move over
--       the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
--       the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--       is that a jump can be reduced to a move as in effect
--       nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing
--
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
-- jumps0 = generateLeaps grid0 3
board0 = strToBoard "WWW-WW-------BB-BBB"

--
-- strToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
--       [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

strToBoard :: String  -> Board
strToBoard s = map (\ x -> check x) s
    where
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--       to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where
        check W = 'W'
        check B = 'B'
        check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid
--         initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--       [(0,0),(1,0),(2,0)
--     (0,1),(1,1),(2,1),(3,1)
--  (0,2),(1,2),(2,2),(3,2),(4,2)
--     (0,3),(1,3),(2,3),(3,3)
--        (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

-- hexagonGrid :: Int -> Grid
-- hexagonGrid sideLength = generateGrid (sideLength) (sideLength - 1) (2 * (sideLength - 1))

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
--       it is a part of the internal representation of the game, this
--       list of all possible slides is only generated once; and when
--       generating next moves, the program decides which slides out of
--       all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--


generateSlides :: Grid -> Int -> [Slide]
generateSlides b n -- To Be Completed
    | null b        = []
    | otherwise     = slideLeft b (head b) n ++ generateSlides (tail b) n
    | otherwise = []

-- x - 1, y
slideLeft :: Grid -> Point -> Int -> [Slide]
slideLeft b p n
    | elem (fst p - 1, snd p) b = (p, (fst p - 1, snd p)) : slideRight b p n
    | otherwise                 = slideRight b p n

-- x + 1, y
slideRight :: Grid -> Point -> Int -> [Slide]
slideRight b p n
    | elem (fst p + 1, snd p) b = (p, (fst p + 1, snd p)) : slideDown b p n
    | otherwise                 = slideDown b p n

-- x, y + 1
slideDown :: Grid -> Point -> Int -> [Slide]
slideDown b p n
    | elem (fst p, snd p + 1) b = (p, (fst p, snd p + 1)) : slideUp b p n
    | otherwise                 = slideUp b p n

-- x, y - 1
slideUp :: Grid -> Point -> Int -> [Slide]
slideUp b p n
    | elem (fst p, snd p - 1) b = (p, (fst p, snd p - 1)) : slideUpLeft b p n
    | otherwise                 = slideUpLeft b p n

-- x - 1, y - 1
slideUpLeft :: Grid -> Point -> Int -> [Slide]
slideUpLeft b p n
    | (snd p) < n && elem (fst p - 1, snd p - 1) b  = (p, (fst p - 1, snd p - 1)) : slideDownRight b p n
    | otherwise                         = slideDownRight b p n

-- x + 1, y + 1
slideDownRight :: Grid -> Point -> Int -> [Slide]
slideDownRight b p n
    | (snd p) < (n - 1) && elem (fst p + 1, snd p + 1) b  = (p, (fst p + 1, snd p + 1)) : slideUpRight b p n
    | otherwise                         = slideUpRight b p n

-- x + 1, y - 1
slideUpRight :: Grid -> Point -> Int -> [Slide]
slideUpRight b p n
    | (snd p) > (n - 1) && elem (fst p + 1, snd p - 1) b  = (p, (fst p + 1, snd p - 1)) : slideDownLeft b p n
    | otherwise                         = slideDownLeft b p n

-- x - 1, y + 1
slideDownLeft :: Grid -> Point -> Int -> [Slide]
slideDownLeft b p n
    | (snd p) >= (n - 1) && elem (fst p - 1, snd p + 1) b  = [(p, (fst p - 1, snd p + 1))]
    | otherwise                         = []

--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
--       it is a part of the internal representation of the game, this
--       list of all possible leaps is only generated once; and when
--       generating next moves, the program decides which leaps out of
--       all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

-- generateLeaps :: Grid -> Int -> [Jump]
-- generateLeaps b n = -- To Be Completed
