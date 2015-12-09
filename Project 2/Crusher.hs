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

--
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

type Tile = (Piece, Point)

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
-- BoardTreeScore is a tuple of a BoardTree and its corresponding goodness value
--
type BoardTreeScore = (BoardTree, Int)

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a jump
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
crusher (current:old) p d n =
    boardToStr (stateSearch (strToBoard current) (map strToBoard old) (generateHexagonGrid n) (generateSlides (generateHexagonGrid n) n) (generateJumps (generateHexagonGrid n) n) piece d n) : (current : old)
        where piece = if p == 'W' then W else B

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

strToBoard :: String -> Board
strToBoard s = map (\ x -> check x) s
    where
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B or D and
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
-- generateHexagonGrid
--
-- This function consumes one integer and generates a
-- regular hexagon of side length n
--
-- Arguments:
-- -- n: side length
--
-- Returns: the corresponding hexagon grid
--

generateHexagonGrid :: Int -> Grid
generateHexagonGrid n = generateGrid n (n - 1) (2 * (n - 1)) []

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
-- Note: This function on being passed 4 3 6 [] would produce
--
--         [(0,0),(1,0),(2,0),(3,0),
--       (0,1),(1,1),(2,1),(3,1),(4,1),
--    (0,2),(1,2),(2,2),(3,2),(4,2),(5,2),
-- (0,3),(1,3),(2,3),(3,3),(4,3),(5,3),(6,3),
--    (0,4),(1,4),(2,4),(3,4),(4,4),(5,4),
--      (0,5),(1,5),(2,5),(3,5),(4,5),
--         (0,6),(1,6),(2,6),(3,6)]
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

--
-- Helper to generateSlides
-- Calls slideLeft, which starts to check all valid moves
-- The x and y translation that is being checked are documented above the slideLeft, slideRight, etc functions
-- and are only added to the list of valid slides if they are a possible move
--

generateSlidesHelper :: Grid -> Grid -> Int -> [Slide]
generateSlidesHelper originalGrid currentGrid n
    | null currentGrid  = []
    | otherwise         = slideLeft originalGrid (head currentGrid) n ++ generateSlidesHelper originalGrid (tail currentGrid) n

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
    | otherwise                                     = slideDownRight b p n

-- x + 1, y + 1
slideDownRight :: Grid -> Point -> Int -> [Slide]
slideDownRight b p n
    | (snd p) < (n - 1) && elem (fst p + 1, snd p + 1) b    = (p, (fst p + 1, snd p + 1)) : slideUpRight b p n
    | otherwise                                             = slideUpRight b p n

-- x + 1, y - 1
slideUpRight :: Grid -> Point -> Int -> [Slide]
slideUpRight b p n
    | (snd p) > (n - 1) && elem (fst p + 1, snd p - 1) b    = (p, (fst p + 1, snd p - 1)) : slideDownLeft b p n
    | otherwise                                             = slideDownLeft b p n

-- x - 1, y + 1
slideDownLeft :: Grid -> Point -> Int -> [Slide]
slideDownLeft b p n
    | (snd p) >= (n - 1) && elem (fst p - 1, snd p + 1) b   = [(p, (fst p - 1, snd p + 1))]
    | otherwise                                             = []

--
-- generateJumps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible jumps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate jumps for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
--       it is a part of the internal representation of the game, this
--       list of all possible jumps is only generated once; and when
--       generating next moves, the program decides which jumps out of
--       all these possible jumps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

generateJumps :: Grid -> Int -> [Jump]
generateJumps b n = generateJumpsHelper b b n

--
-- Helper to generateJumps
-- Calls jumpLeft, which starts to check all valid moves
-- The x and y translation that is being checked are documented above the jumpLeft, jumpRight, etc functions
-- and are only added to the list of valid jumps if they are a possible move
--

generateJumpsHelper :: Grid -> Grid -> Int -> [Jump]
generateJumpsHelper originalGrid currentGrid n
    | null currentGrid  = []
    | otherwise         = jumpLeft originalGrid (head currentGrid) n ++ generateJumpsHelper originalGrid (tail currentGrid) n

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
    | snd p < n && elem (fst p - 2, snd p - 2) b    = (p, (fst p - 1, snd p - 1), (fst p - 2, snd p - 2)) : jumpDown2Right2 b p n
    | otherwise                                     = jumpDown2Right2 b p n

-- x + 2, y + 2
jumpDown2Right2 :: Grid -> Point -> Int -> [Jump]
jumpDown2Right2 b p n
    | (snd p + 2) < n && elem (fst p + 2, snd p + 2) b  = (p, (fst p + 1, snd p + 1), (fst p + 2, snd p + 2)) : jumpUp2Left1 b p n
    | otherwise                                         = jumpUp2Left1 b p n

-- x - 1, y - 2
jumpUp2Left1 :: Grid -> Point -> Int -> [Jump]
jumpUp2Left1 b p n
    | snd p == n && elem (fst p - 1, snd p - 2) b       = (p, (fst p, snd p - 1), (fst p - 1, snd p - 2)) : jumpDown2Right1 b p n
    | otherwise                                         = jumpDown2Right1 b p n

-- x + 1, y + 2
jumpDown2Right1 :: Grid -> Point -> Int -> [Jump]
jumpDown2Right1 b p n
    | snd p == (n - 2) && elem (fst p + 1, snd p + 2) b = (p, (fst p + 1, snd p + 1), (fst p + 1, snd p + 2)) : jumpDown2Left2 b p n
    | otherwise                                         = jumpDown2Left2 b p n

-- x - 2, y + 2
jumpDown2Left2 :: Grid -> Point -> Int -> [Jump]
jumpDown2Left2 b p n
    | snd p > n - 2 && elem (fst p - 2, snd p + 2) b    = (p, (fst p - 1, snd p + 1), (fst p - 2, snd p + 2)) : jumpUp2Right2 b p n
    | otherwise                                         = jumpUp2Right2 b p n

-- x + 2, y - 2
jumpUp2Right2 :: Grid -> Point -> Int -> [Jump]
jumpUp2Right2 b p n
    | snd p > n && elem (fst p + 2, snd p - 2) b    = (p, (fst p + 1, snd p - 1), (fst p + 2, snd p - 2)) : jumpUp2Right1 b p n
    | otherwise                                     = jumpUp2Right1 b p n

-- x + 1, y - 2
jumpUp2Right1 :: Grid -> Point -> Int -> [Jump]
jumpUp2Right1 b p n
    | snd p == n && elem (fst p + 1, snd p - 2) b   = (p, (fst p + 1, snd p - 1), (fst p + 1, snd p - 2)) : jumpDown2Left1 b p n
    | otherwise                                     = jumpDown2Left1 b p n

-- x - 1, y + 2
jumpDown2Left1 :: Grid -> Point -> Int -> [Jump]
jumpDown2Left1 b p n
    | snd p == (n - 2) && elem (fst p - 1, snd p + 2) b = [(p, (fst p, snd p + 1), (fst p - 1, snd p + 2))]
    | otherwise                                         = []

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

stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth num
    | gameOver board history num    = board
    | player == W                   = minimax (generateTree board history grid slides jumps W depth num) getWhiteScore
    | otherwise                     = minimax (generateTree board history grid slides jumps B depth num) getBlackScore

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

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n = generateTreeHelper history grid slides jumps player depth 0 n board

--
-- generateTreeHelper
--
-- This function is a helper to the actual generateTree function. This
-- function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTreeHelper :: [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> Board -> BoardTree
generateTreeHelper history grid slides jumps player depth currentDepth n board
    | currentDepth == depth = (Node depth board [])
    | player == W           = (Node currentDepth board (map (generateTreeHelper history grid slides jumps B depth (currentDepth + 1) n) (generateNewStates board history grid slides jumps W)))
    | otherwise             = (Node currentDepth board (map (generateTreeHelper history grid slides jumps W depth (currentDepth + 1) n) (generateNewStates board history grid slides jumps B)))

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
    map stateToBoard (applyMoves state moves player) \\ history -- Find the relative set complement of the history in all possible boards
        where
            state = boardToState board grid
            moves = moveGenerator state slides jumps player

--
-- applyMoves
--
-- This function applys each possible move to the current state for a player to generate all possible states
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- moves: the list of all valid Moves that the player could make
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next states
--

applyMoves :: State -> [Move] -> Piece -> [State]
applyMoves state moves player
    | null moves    = []
    | otherwise     = applyMove state (head moves) player : applyMoves state (tail moves) player

--
-- applyMove
--
-- This function applys a move to the state to get a possible future state
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- move: a valid Move the player could make
-- -- player: W or B representing the player the program is
--
-- Returns: the different state as a result of applying a move
--

applyMove :: State -> Move -> Piece -> State
applyMove state move player
    -- No more tiles in the current state to check
    | null state    = []
    -- Recursively check each tile in state if the move will change it
    | otherwise     = processTile (head state) move player : applyMove (tail state) move player

--
-- processTile
--
-- This function converts the tile if it is part of the move
--
-- Arguments:
-- -- tile: the Tile in the current state checked to see if it is part of the move
-- -- move: a valid Move the player could make
-- -- player: W or B representing the player the program is
--
-- Returns: the tile as a result of applying a move
--

processTile :: Tile -> Move -> Piece -> Tile
processTile tile move player
    -- The source tile becomes a blank tile
    | fst tile == player && snd tile == fst move    = (D, snd tile)
    -- The destination tile becomes the new tile
    | snd tile == snd move                          = (player, snd tile)
    -- This tile is not a part of the move
    | otherwise                                     = tile

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
--

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
    -- Look for all jump moves possible from the current tile
    solveJumps state tile jumps player

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
-- solveJumps
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

solveJumps :: State -> Tile -> [Jump] -> Piece -> [Move]
solveJumps state tile jumps player =
    -- Jump is valid when the end point is not occupied by the player; move is the start and end points
    [(tripleFst x, tripleLst x) | x <- legalJumps, elem (tripleLst x) freeStates]
    where
        -- Get jumps that start at the tile's point
        allJumps = [x | x <- jumps, tripleFst x == snd tile]
        -- Of those jumps, check that the piece lept over is the player's color
        legalJumps = [x | x <- allJumps, elem (player, tripleMid x) state]
        -- All tiles not occupied by the player
        freeStates = [snd x | x <- state, fst x /= player]


--
-- minimaxTuple
--
-- This function pairs a BoardTree with its corresponding goodnessValue
--
-- Arguments:
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
-- -- isMax: a Boolean indicating whether the function should be maximizing
--           or miniziming the goodness values of its children
-- -- boardTree: a BoardTree
--
-- Returns: a minimax tuple
--

minimaxTuple' :: (Board -> Bool -> Int) -> Bool -> BoardTree -> BoardTreeScore
minimaxTuple' heuristic isMax boardTree = (boardTree, (minimax' heuristic isMax boardTree))

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

minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic
    | null children = b
    | otherwise     = board (fst (head (sortBy compareBoardTreeScores (map (minimaxTuple' heuristic True) children))))

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
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--               appropriate heuristic to apply based on the size of the board,
--               who the program is playing as, and all the boards already seen
-- -- isMax: a Boolean indicating whether the function should be maximizing
--           or miniziming the goodness values of its children
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
--
-- Returns: the minimax value at the top of the tree
--

minimax' :: (Board -> Bool -> Int) -> Bool -> BoardTree -> Int
minimax' heuristic isMax (Node depth board nextBoards)
    | null nextBoards   = heuristic board False
    | isMax == False    = minimum (map (minimax' heuristic True) nextBoards)
    | otherwise         = maximum (map (minimax' heuristic False) nextBoards)

--
-- compareBoardTreeScores
--
-- This function is used to sort objects of type BoardTreeScore
--
-- Arguments:
-- -- (a1,b1): a BoardTreeScore
-- -- (a2,b2): a BoardTreeScore
--
-- Returns: the descending order of BoardTreeScore
--

compareBoardTreeScores :: BoardTreeScore -> BoardTreeScore -> Ordering
compareBoardTreeScores (a1,b1) (a2,b2)
    | b1 < b2      = GT
    | b1 == b2     = EQ
    | otherwise    = LT

--
-- getWhiteScore
--
-- This function is used to the goodness value of the board in perspective of W
--
-- Arguments:
-- -- board: a Board
-- -- _: a boolean that might be implemented later for a more accurate goodness value
--
-- Returns: the goodness value of the board
--

getWhiteScore :: Board -> Bool -> Int
getWhiteScore board _ = square (countWhite board) - square (countBlack board)

--
-- getBlackScore
--
-- This function is used to the goodness value of the board in perspective of B
--
-- Arguments:
-- -- board: a Board
-- -- _: a boolean that might be implemented later for a more accurate goodness value
--
-- Returns: the goodness value of the board
--

getBlackScore :: Board -> Bool -> Int
getBlackScore board _ = square (countBlack board) - square (countWhite board)

--
-- square
--
-- This function squares an integer
--
-- Arguments:
-- -- x: the integer being squared
--
-- Returns: the squared value fo the integer
--

square :: Int -> Int
square x = x * x

--
-- countBlack
--
-- This function counts the number of B pieces on the board
--
-- Arguments:
-- -- board: a Board
--
-- Returns: the number of B pieces on the board
--

countBlack :: Board -> Int
countBlack board = countBoard board B

--
-- countWhite
--
-- This function counts the number of W pieces on the board
--
-- Arguments:
-- -- board: a Board
--
-- Returns: the number of W pieces on the board
--

countWhite :: Board -> Int
countWhite board = countBoard board W

--
-- countBoard
--
-- This function counts the number of a type of pieces on the board
--
-- Arguments:
-- -- board: a Board
-- -- piece: the Piece to count
--
-- Returns: the number of pieces on the board
--

countBoard :: Board -> Piece -> Int
countBoard board piece
    | null board            = 0
    | head board == piece   = 1 + countBoard (tail board) piece
    | otherwise             = countBoard (tail board) piece
