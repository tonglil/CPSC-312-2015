import Data.List

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
{-
   run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
   grid0 = generateGrid 3 2 4 []
   slides0 = generateSlides grid0 3
   jumps0 = generateLeaps grid0 3
   board0 = strToBoard "WWW-WW-------BB-BBB"
   newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
   tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
   heuristic0 = boardEvaluator W [] 3
-}

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of
-- search tree, the size of the provide boards, and produces the
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--


crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n
    | p == 'W' = boardToStr (minimax (generateTree (strToBoard current) (map strToBoard old) (generateHexagonGrid n) (generateSlides (generateHexagonGrid n) n) (generateLeaps (generateHexagonGrid n) n) W d n) (getWhiteScore)) : (current:old)
    | otherwise = boardToStr (minimax (generateTree (strToBoard current) (map strToBoard old) (generateHexagonGrid n) (generateSlides (generateHexagonGrid n) n) (generateLeaps (generateHexagonGrid n) n) B d n) (getBlackScore)) : (current:old)

generateHexagonGrid :: Int -> Grid
generateHexagonGrid n = generateGrid n (n - 1) (2 * (n - 1)) []

--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n
    | board `elem` history              = True  -- Look for the current board inside the history of boards
    | length (filter (== W) board) < n  = True  -- Check if W has enough pieces
    | length (filter (== B) board) < n  = True  -- Check if B has enough pieces
    | otherwise                         = False -- Game continues

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

--       [(0,0),(1,0),(2,0)
--     (0,1),(1,1),(2,1),(3,1)
--  (0,2),(1,2),(2,2),(3,2),(4,2)
--     (0,3),(1,3),(2,3),(3,3)
--        (0,4),(1,4),(2,4)()]


--         [(0,0),(1,0),(2,0),(3,0),
--       (0,1),(1,1),(2,1),(3,1),(4,1),
--    (0,2),(1,2),(2,2),(3,2),(4,2),(5,2),
-- (0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3),
--    (0,4),(1,4),(2,4),(3,4),(4,4),(5,4),
--      (0,5),(1,5),(2,5),(3,5),(4,5),
--         (0,6),(1,6),(2,6),(3,6)]

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

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
generateSlides b n = generateSlidesHelper b b n

generateSlidesHelper :: Grid -> Grid -> Int -> [Slide]
generateSlidesHelper originalGrid currentGrid n -- To Be Completed
    | null currentGrid        = []
    | otherwise     = slideLeft originalGrid (head currentGrid) n ++ generateSlidesHelper originalGrid (tail currentGrid) n

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

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = generateLeapsHelper b b n

generateLeapsHelper :: Grid -> Grid -> Int -> [Jump]
generateLeapsHelper originalGrid currentGrid n -- To Be Completed
    | null currentGrid        = []
    | otherwise     = jumpLeft originalGrid (head currentGrid) n ++ generateLeapsHelper originalGrid (tail currentGrid) n

-- x - 2, y
jumpLeft :: Grid -> Point -> Int -> [Jump]
jumpLeft b p n
    | elem (fst p - 2, snd p) b = (p, (fst p - 1, snd p), (fst p - 2, snd p)) : jumpRight b p n
    | otherwise                 = jumpRight b p n

-- x + 2, y
jumpRight :: Grid -> Point -> Int -> [Jump]
jumpRight b p n
    | elem (fst p + 2, snd p) b = (p, (fst p + 1, snd p), (fst p + 2, snd p)) : jumpDown b p n
    | otherwise                 = jumpDown b p n

-- x, y + 2
jumpDown :: Grid -> Point -> Int -> [Jump]
jumpDown b p n
    | elem (fst p, snd p + 2) b = (p, (fst p, snd p + 1), (fst p, snd p + 2)) : jumpUp b p n
    | otherwise                 = jumpUp b p n

-- x, y - 2
jumpUp :: Grid -> Point -> Int -> [Jump]
jumpUp b p n
    | elem (fst p, snd p - 2) b = (p, (fst p, snd p - 1), (fst p, snd p - 2)) : jumpUp2Left2 b p n
    | otherwise                 = jumpUp2Left2 b p n

-- x - 2, y - 2
jumpUp2Left2 :: Grid -> Point -> Int -> [Jump]
jumpUp2Left2 b p n
    | snd p < n && elem (fst p - 2, snd p - 2) b = (p, (fst p - 1, snd p - 1), (fst p - 2, snd p - 2)) : jumpDown2Right2 b p n
    | otherwise                 = jumpDown2Right2 b p n

-- x + 2, y + 2
jumpDown2Right2 :: Grid -> Point -> Int -> [Jump]
jumpDown2Right2 b p n
    | (snd p + 2) < n && elem (fst p + 2, snd p + 2) b = (p, (fst p + 1, snd p + 1), (fst p + 2, snd p + 2)) : jumpUp2Left1 b p n
    | otherwise                 = jumpUp2Left1 b p n

-- x - 1, y - 2
jumpUp2Left1 :: Grid -> Point -> Int -> [Jump]
jumpUp2Left1 b p n
    | snd p == n && elem (fst p - 1, snd p - 2) b = (p, (fst p, snd p - 1), (fst p - 1, snd p - 2)) : jumpDown2Right1 b p n
    | otherwise                 = jumpDown2Right1 b p n

-- x + 1, y + 2
jumpDown2Right1 :: Grid -> Point -> Int -> [Jump]
jumpDown2Right1 b p n
    | snd p == (n - 2) && elem (fst p + 1, snd p + 2) b = (p, (fst p + 1, snd p + 1), (fst p + 1, snd p + 2)) : jumpDown2Left2 b p n
    | otherwise                 = jumpDown2Left2 b p n

-- x - 2, y + 2
jumpDown2Left2 :: Grid -> Point -> Int -> [Jump]
jumpDown2Left2 b p n
    | snd p > n - 2 && elem (fst p - 2, snd p + 2) b = (p, (fst p - 1, snd p + 1), (fst p - 2, snd p + 2)) : jumpUp2Right2 b p n
    | otherwise                                 = jumpUp2Right2 b p n

-- x + 2, y - 2
jumpUp2Right2 :: Grid -> Point -> Int -> [Jump]
jumpUp2Right2 b p n
    | snd p > n && elem (fst p + 2, snd p - 2) b = (p, (fst p + 1, snd p - 1), (fst p + 2, snd p - 2)) : jumpUp2Right1 b p n
    | otherwise                                      = jumpUp2Right1 b p n

-- x + 1, y - 2
jumpUp2Right1 :: Grid -> Point -> Int -> [Jump]
jumpUp2Right1 b p n
    | snd p == n && elem (fst p + 1, snd p - 2) b = (p, (fst p + 1, snd p - 1), (fst p + 1, snd p - 2)) : jumpDown2Left1 b p n
    | otherwise                                      = jumpDown2Left1 b p n

-- x - 1, y + 2
jumpDown2Left1 :: Grid -> Point -> Int -> [Jump]
jumpDown2Left1 b p n
    | snd p == (n - 2) && elem (fst p - 1, snd p + 2) b = [(p, (fst p, snd p + 1), (fst p - 1, snd p + 2))]
    | otherwise                                      = []

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the
-- board, else generate a search tree till the specified depth and apply
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over,
--          otherwise produces the next best board
--

{-
   stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
   stateSearch board history grid slides jumps player depth num = -- To Be Completed
-}

--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

{-
   generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
   generateTree board history grid slides jumps player depth n = -- To Be Completed
-}

--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate
-- a list of next boards, and then checks whether or not that move would
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
    map stateToBoard (applyMoves state moves player) \\ history -- set complement of history
        where
            state = boardToState board grid
            moves = moveGenerator state slides jumps player

-- Generate all possible states by applying each possible moves to the current state for a player
applyMoves :: State -> [Move] -> Piece -> [State]
applyMoves state moves player
    | null moves    = []
    | otherwise     = applyMove state (head moves) player : applyMoves state (tail moves) player

-- Apply a move to the state to get a possible future state
-- For each tile in state, check if move will change it
applyMove :: State -> Move -> Piece -> State
applyMove state move player
    | null state    = [] -- This case doesn't really make sense
    | otherwise     = processTile (head state) move player : applyMove (tail state) move player

-- Convert the tile if it is part of the move
processTile :: Tile -> Move -> Piece -> Tile
processTile tile move player
    -- The src tile -> becomes blank tile
    | fst tile == player && snd tile == fst move    = (D, snd tile)
    -- The dest tile -> becomes new tile
    | snd tile == snd move                          = (player, snd tile)
    -- This tile is not a part of the move
    | otherwise                                     = tile


-- For each move
--      Apply to state to create a new state
-- For each new state
--      Convert to board
--      Filter out ones that have been played before (in history)




{-
   let string = "WWW-WW-------BB-BBB"
   let board = strToBoard string
   let grid = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)]
   let past = ["-WW-WW---W---BB-BBB", "WWW-WW-------BB-BBB"]
   let history = map strToBoard past
   let state = boardToState board grid
   let slides = generateSlides grid 3
   let jumps = generateLeaps grid 3
   let tile = (W,(0,0))
   let movesW = moveGenerator state slides jumps W
   let playerW = W
   length movesW
-}

-- Generate current state from board
-- Generate all moves from current state (moveGenerator)
-- Generate states based on moves
-- Convert states into board
-- Filter out boards that have been played

-- State = [Tile]
-- Tile = (Piece, Point) = (W, (0,0))
-- Piece = D | W | B
-- Point = (Int, Int)
-- Move = (Point, Point)





--
-- boardToState
--
-- This function consumes a Board and a Grid to generate the State of the game
--
-- Arguments:
-- -- board: a list of Pieces representing the Board
-- -- grid: a list of Points representing the Grid
--
-- Returns: a list of Tiles representing the State of the game
--
-- Example:
--          let board = [W,W,W,D,W,W,D,D,D,D,D,D,B,B,B,D,B,B,D]
--          let grid = [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1),(3,1),(0,2),(1,2),(2,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3),(0,4),(1,4),(2,4)]
--          boardToState board grid
--

boardToState :: Board -> Grid -> State
boardToState board grid
    | null board    = []
    | otherwise     = (head board, head grid) : boardToState (tail board) (tail grid)

--
-- stateToBoard
--
-- This function consumes a State to generate the Board of the game
--
-- Arguments:
-- -- state: a list of Tiles representing the State of the game
--
-- Returns: a list of Pieces representing the Board
--
-- Example:
--          let state = [(W,(0,0)),(W,(1,0)),(W,(2,0)),(D,(0,1)),(W,(1,1)),(W,(2,1)),(D,(3,1)),(D,(0,2)),(D,(1,2)),(D,(2,2)),(D,(3,2)),(D,(4,2)),(B,(0,3)),(B,(1,3)),(B,(2,3)),(D,(3,3)),(B,(0,4)),(B,(1,4)),(D,(2,4))]
--          stateToBoard state
--

stateToBoard :: State -> Board
stateToBoard state
    | null state    = []
    | otherwise     = fst (head state) : stateToBoard (tail state)

--
-- tripleFst, tripleMid, tripleLst
--
-- These functions consume a triple-tuple and returns the first, middle, or last element
--
-- Arguments
-- -- a: anything
-- -- b: anything
-- -- c: anything
--
-- Returns: the selected value in the triple-tuple

tripleFst :: (a, b, c) -> a
tripleFst (x, _, _) = x

tripleMid :: (a, b, c) -> b
tripleMid (_, x, _) = x

tripleLst :: (a, b, c) -> c
tripleLst (_, _, x) = x

--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps,
-- a list of possible slides and a player from whose perspective
-- to generate moves, to check which of these jumps and slides
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: The bulk of the work is deferred to moveGeneratorHelper
--
-- Note: This is the only instance where the program makes use of the
--       type State, for our purposes it is zipping the board and the
--       grid together for making it easier to make moves
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--

moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player = moveGeneratorHelper state state slides jumps player

--
-- moveGeneratorHelper
--
-- This function generates moves for a state and tracks of the entire game state
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- remaining: the remaining states to generate moves for
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of all valid moves that the player could make
--

moveGeneratorHelper :: State -> State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGeneratorHelper state remaining slides jumps player
    -- No state to generate moves for
    | null remaining        = []
    -- Look for moves for the current Tile if it belongs to the player and add to the list of moves
    | fst tile == player    = moveGeneratorTile state tile slides jumps player ++ next
    -- Recurse and generate moves for the remaining Tiles
    | otherwise             = next
        where
            tile = head remaining
            next = moveGeneratorHelper state (tail remaining) slides jumps player

--
-- moveGeneratorTile
--
-- This function consumes a state, a tile, a list of possible jumps,
-- a list of possible slides and a player from whose perspective
-- to generate moves, to check which of these jumps and slides
-- the player could actually make from a tile, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- tile: the Tile to generate moves for
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of all valid moves that the player could make from a tile
--

moveGeneratorTile :: State -> Tile -> [Slide] -> [Jump] -> Piece -> [Move]
moveGeneratorTile state tile slides jumps player =
    -- Look for all slide moves possible from the current tile
    solveSlides state tile slides ++
    -- Look for all leap moves possible from the current tile
    solveLeaps state tile jumps player

--
-- solveSlides
--
-- This function consumes a state, a tile, and a list of possible slides
-- to generate a list of valid slide moves from a tile
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- tile: the Tile to generate moves for
-- -- slides: the list of all Slides possible for the given grid
--
-- Returns: the list of valid slide moves that the player could make from a tile
--

solveSlides :: State -> Tile -> [Slide] -> [Move]
solveSlides state tile slides =
    -- Slide is valid when the end point is not occupied
    [x | x <- allSlides, elem (snd x) freeStates]
    where
        -- Get slides that start at the tile's point
        allSlides = [x | x <- slides, fst x == snd tile]
        -- All empty tiles
        freeStates = [snd x | x <- state, fst x == D]

--
-- solveLeaps
--
-- This function consumes a state, a tile, a list of possible jumps, and a piece
-- to generate a list of valid jump moves from a tile
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- tile: the Tile to generate moves for
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of valid jump moves that the player could make from a tile
--

solveLeaps :: State -> Tile -> [Jump] -> Piece -> [Move]
solveLeaps state tile jumps player =
    -- Jump is valid when the end point is not occupied by the player; move is the start and end points
    [(tripleFst x, tripleLst x) | x <- legalLeaps, elem (tripleLst x) freeStates]
    where
        -- Get jumps that start at the tile's point
        allLeaps = [x | x <- jumps, tripleFst x == snd tile]
        -- Of those jumps, check that the piece lept over is the player's color
        legalLeaps = [x | x <- allLeaps, elem (player, tripleMid x) state]
        -- All tiles not occupied by the player
        freeStates = [snd x | x <- state, fst x /= player]

--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by
-- taking into account whose perspective the program is playing from, the list
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and
-- accordingly produce a goodness value of the given board
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

{-
   boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
   boardEvaluator player history n board myTurn = -- To Be Completed
-}

--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree,
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

{-
   minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
   minimax (Node _ b children) heuristic = -- To Be Completed
-}

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes
-- a search tree, an appropriate heuristic to apply to the leaf nodes of
-- the tree, and based on whether it would have been the maximizing
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
--               or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--

{-
   minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
   minimax' boardTree heuristic maxPlayer = -- To Be Completed
-}

type BoardTreeScore = (BoardTree, Int)

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n = generateTreeHelper history grid slides jumps player depth 0 n board

generateTreeHelper :: [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> Board -> BoardTree
generateTreeHelper history grid slides jumps player depth currentDepth n board
    | currentDepth == depth = (Node depth board [])
    | otherwise = (Node currentDepth board (map (generateTreeHelper history grid slides jumps player depth (currentDepth + 1) n) (generateNewStates board history grid slides jumps player)))


minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic
    | null children = b
    | otherwise = board (fst (head (sortBy compareBoardTreeScores (map (minimaxTuple' heuristic True) children))))

compareBoardTreeScores :: BoardTreeScore -> BoardTreeScore -> Ordering
compareBoardTreeScores (a1,b1) (a2,b2)
     | b1 < b2      = GT  
     | b1 == b2     = EQ  
     | otherwise    = LT

getWhiteScore :: Board -> Bool -> Int
getWhiteScore board _ = countWhite board - countBlack board

getBlackScore :: Board -> Bool -> Int
getBlackScore board _ = countBlack board - countWhite board

countBlack :: Board -> Int
countBlack board
    | null board = 0
    | head board == B = 1 + countBlack (tail board)
    | otherwise = countBlack (tail board)

countWhite :: Board -> Int
countWhite board
    | null board = 0
    | head board == W = 1 + countWhite (tail board)
    | otherwise = countWhite (tail board)

minimax' :: (Board -> Bool -> Int) -> Bool -> BoardTree -> Int
minimax' heuristic isMax (Node depth board nextBoards)
    | null nextBoards = heuristic board False
    | isMax == False = minimum (map (minimax' heuristic True) nextBoards)
    | otherwise = maximum (map (minimax' heuristic False) nextBoards)

minimaxTuple' :: (Board -> Bool -> Int) -> Bool -> BoardTree -> (BoardTree, Int)
minimaxTuple' heuristic isWhite boardTree = (boardTree, (minimax' heuristic isWhite boardTree))
