import qualified Data.Map as M

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

goal :: Puzzle
goal = "12345678_"

aStar :: [Node] -> [Node] -> [Step]
aStar open closed
    | isGoal    = buildPath open closed current
    | otherwise = aStar open'' closed'
    where
        isGoal    = puzzle current == goal
        current   = cheapest open
        open'     = remove current open
        neighbors = expand current closed
        open''    = visit neighbors open'
        closed'   = append current closed

buildPath :: [Node] -> [Node] -> Node -> [Step]
buildPath = undefined

cheapest :: [Node] -> Node
cheapest nodes = undefined

remove :: Node -> [Node] -> [Node]
remove item ns = undefined

expand :: Node -> [Node] -> [Node]
expand node closed = undefined

visit :: [Node] -> [Node] -> [Node]
visit new old = undefined

append :: Node -> [Node] -> [Node]
append node nodes = undefined
