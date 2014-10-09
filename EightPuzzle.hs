module EightPuzzle
( Step
, Node(..)
, goal
, nodeElem
, nodeEq
, stepsTaken
) where

import Data.List (elemIndex)

type Puzzle = String
type Score  = Int

data Step = StepLeft
          | StepUp
          | StepRight
          | StepDown
          deriving (Show)

data Node = Root
          | Node { puzzle :: Puzzle,
                   gScore :: Score,
                   fScore :: Score,
                   prev   :: Node
                 }
          deriving (Show, Eq)

goal :: Puzzle
goal = "12345678_"

nodeElem :: Node -> [Node] -> Bool
nodeElem node [] = False
nodeElem node (n:ns)
    | nodeEq node n = True
    | otherwise     = nodeElem node ns

nodeEq :: Node -> Node -> Bool
nodeEq m n = puzzle m == puzzle n

stepsTaken :: Node -> [Step]
stepsTaken node = stepsTaken' node []

stepsTaken' :: Node -> [Step] -> [Step]
stepsTaken' node steps
    | prev node == Root = steps
    | otherwise         = stepsTaken' (prev node) steps'
    where
        prevPuzzle = puzzle $ prev node
        nodePuzzle = puzzle node
        steps'     = stepTaken prevPuzzle nodePuzzle : steps

stepTaken :: Puzzle -> Puzzle -> Step
stepTaken from to
    | indexDiff == 3 = if isIncreasingIndex then StepDown else StepUp
    | indexDiff == 1 = if isIncreasingIndex then StepRight else StepLeft
    | otherwise      = error "Not a valid, single step."
    where
        indexDiff         = abs (blankIndex from - blankIndex to)
        isIncreasingIndex = blankIndex from < blankIndex to

blankIndex :: Puzzle -> Int
blankIndex puzzle =
    case elemIndex '_' puzzle of
        Just index -> index
        Nothing    -> error "Invalid puzzle."
