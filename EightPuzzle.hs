module EightPuzzle
( Step
, Node(..)
, goal
, nodeElem
, nodeEq
, stepsTaken
, moveLeft
, moveUp
, moveRight
, moveDown
, initPuzzle
) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

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
blankIndex puzzle = fromJust $ elemIndex '_' puzzle

distancesToGoal ::  Puzzle -> Int
distancesToGoal puzzle' = sum $ map (distanceToGoal puzzle') [0..8]

-- Manhattan distance to goal position from current index
distanceToGoal :: Puzzle -> Int -> Int
distanceToGoal puzzle i =
    let goalI   = fromJust $ elemIndex (puzzle !! i) goal
        goalRow = goalI `div` 3
        goalCol = goalI `mod` 3
        currRow = i `div` 3
        currCol = i `mod` 3
    in  abs (goalRow - currRow) + abs (goalCol - currCol)

swap :: Eq a => a -> a -> [a] -> [a]
swap _ _ [] = []
swap a b (x:xs)
    | x == a    = b : swap a b xs
    | x == b    = a : swap a b xs
    | otherwise = x : swap a b xs

moveLeft ::  Node -> Maybe Node
moveLeft = move movePuzzleLeft

moveUp ::  Node -> Maybe Node
moveUp = move movePuzzleUp

moveRight ::  Node -> Maybe Node
moveRight = move movePuzzleRight

moveDown ::  Node -> Maybe Node
moveDown = move movePuzzleDown

move ::  (Puzzle -> Maybe Puzzle) -> Node -> Maybe Node
move f node = do
    puzzle' <- f (puzzle node)
    let gScore' = gScore node + 1
        hScore  = distancesToGoal puzzle'
        fScore' = gScore' +  hScore
    return $ Node puzzle' gScore' fScore' node

movePuzzleLeft ::  Puzzle -> Maybe Puzzle
movePuzzleLeft = movePuzzle mod 0 (-) 1

movePuzzleUp ::  Puzzle -> Maybe Puzzle
movePuzzleUp = movePuzzle div 0 (-) 3

movePuzzleRight ::  Puzzle -> Maybe Puzzle
movePuzzleRight = movePuzzle mod 2 (+) 1

movePuzzleDown ::  Puzzle -> Maybe Puzzle
movePuzzleDown = movePuzzle div 2 (+) 3

movePuzzle :: (Int -> Int -> Int) -> Int -> (Int -> Int -> Int) -> Int -> Puzzle -> Maybe Puzzle
movePuzzle checkOp checkBound applyOp applyBound puzzle
    | canMoveRight = Just $ swap '_' neighbor puzzle
    | otherwise    = Nothing
    where
        canMoveRight = blankIndex puzzle `checkOp` 3 /= checkBound
        neighbor     = puzzle !! (blankIndex puzzle `applyOp` applyBound)

initPuzzle :: Puzzle -> Node
initPuzzle puzzle' =
    let gScore' = 0
        hScore  = distancesToGoal puzzle'
        fScore' = gScore' + hScore
    in  Node puzzle' gScore' fScore' Root
