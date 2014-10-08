import Data.List (find)

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
visit [] open = open
visit (n:ns) open
    | isNewNeighbor     = visit ns (n:open)
    | isCheaperNeighbor = visit ns (replace n open)
    | otherwise         = visit ns open
    where
        isNewNeighbor     = not (n `nodeElem` open)
        isCheaperNeighbor = gScore n < gScore dupN
        Just dupN         = find (nodeEq n) open

replace :: Node -> [Node] -> [Node]
replace new (x:xs)
    | new `nodeEq` x = new : xs
    | otherwise      = x : replace new xs

nodeElem :: Node -> [Node] -> Bool
nodeElem node [] = False
nodeElem node (n:ns)
    | nodeEq node n = True
    | otherwise     = nodeElem node ns

nodeEq :: Node -> Node -> Bool
nodeEq m n = puzzle m == puzzle n

append :: Node -> [Node] -> [Node]
append node nodes = undefined
