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
generateTree board history grid slides jumps player depth n = generateTreeHelper history grid slides jumpts player depth 0 n board

generateTreeHelper :: [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Int -> Board -> BoardTree
generateTreeHelper history grid slides jumps player depth currentDepth n board
    | currentDepth == depth = (Node depth board [])
    | otherwise = (Node currentDepth board (map (generateTreeHelper history grid slides jumps player depth (currentDepth + 1) n) (generateNewStates board history grid slides jumps player)))


