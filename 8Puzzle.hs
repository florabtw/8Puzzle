module EightPuzzle
( Step
, Node(..)
, goal
, nodeElem
, nodeEq
) where

type Puzzle = String
type Score  = Int

data Step = MoveLeft
          | MoveUp
          | MoveRight
          | MoveDown
          deriving (Show)

data Node = Node { puzzle :: Puzzle,
                   gScore :: Score,
                   fScore :: Score,
                   prev   :: Puzzle
                 }
            deriving (Show)

goal :: Puzzle
goal = "12345678_"

nodeElem :: Node -> [Node] -> Bool
nodeElem node [] = False
nodeElem node (n:ns)
    | nodeEq node n = True
    | otherwise     = nodeElem node ns

nodeEq :: Node -> Node -> Bool
nodeEq m n = puzzle m == puzzle n
