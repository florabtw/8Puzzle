import EightPuzzle (Node(..), Step, goal, nodeElem, nodeEq, stepsTaken)
import Data.List (find)

aStar :: [Node] -> [Node] -> [Step]
aStar open closed
    | isGoal    = stepsTaken current
    | otherwise = aStar open'' closed'
    where
        isGoal    = puzzle current == goal
        current   = cheapest open
        open'     = remove current open
        neighbors = expand current closed
        open''    = visit neighbors open'
        closed'   = append current closed

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
    | otherwise      = x   : replace new xs

append :: Node -> [Node] -> [Node]
append node nodes = undefined
