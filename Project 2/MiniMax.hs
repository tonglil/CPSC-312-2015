import Data.List (sortBy)

data Piece = D | W | B deriving (Eq, Show)
type Board = [Piece]
data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
type BoardTree = Tree Board
type BoardTreeScore = (BoardTree, Int)

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
    | otherwise = board (fst (head (sortBy compareBoardTreeScores (map (minimaxTuple' heuristic True) children))))

compareBoardTreeScores :: BoardTreeScore -> BoardTreeScore -> Ordering
compareBoardTreeScores (a1,b1) (a2,b2)
     | b1 < b2      = GT  
     | b1 == b2     = EQ  
     | otherwise    = LT

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

-- let board1 = (Node 5  [B,B,B,B,B,W,W,W,W] [(Node 5  [B,B,B,B,B] [(Node 5  [W,W,W] [])])])
-- let board1a = (Node 5  [B,B,B,B,B,W,W,W,W] [(Node 5  [B,B,B,B,B] [(Node 5  [W,W,W] [])])])
-- let board2 = (Node 5  [B,B,B,B,W,W,W,W,W] [(Node 5  [W,W] [(Node 5  [B,B,B,B,B] [])])])
-- let board3 = (Node 5  [B,B,B,B,B,W,W,W,W,W] [(Node 5  [B,B,B,W,W,W,W] [(Node 5  [B,B,B,B,B] [])])])


-- let boardTree = (Node 5  [B,B,B,B,B,W,W,W,W,W] [board1, board2, board3])



