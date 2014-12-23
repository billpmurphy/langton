import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

-- Ant

type Point = (Int, Int)

data Direction = North | West | South | East
    deriving (Eq, Show, Read, Enum)

move :: Ant -> Ant
move (Ant (x, y) d) = case d of
    North -> Ant (x, y+1) d
    South -> Ant (x, y-1) d
    East  -> Ant (x+1, y) d
    West  -> Ant (x-1, y) d

data Rotation = L | R | B
    deriving (Eq, Show, Read)

data Ant = Ant { loc :: Point, facing :: Direction }
    deriving (Eq, Show, Read)

defaultAnt :: [Ant]
defaultAnt = [Ant (0, 0) North]

rotate :: Ant -> Rotation -> Ant
rotate (Ant p f) r = case r of
    L -> Ant p $ next f
    R -> Ant p $ prev f
    B -> Ant p . next $ next f
  where next d = if d == East  then North else succ d
        prev d = if d == North then East  else pred d

-- Board

type Grid = Map Point [Rotation]

data Board = Board { ants :: [Ant], board :: Grid }
    deriving (Show, Eq)

defaultBoard :: [Ant] -> Board
defaultBoard as = Board as M.empty

data Rule = Rule { rule :: [Rotation] -> [Rotation], def :: [Rotation] }

nextBoard :: Rule -> Board -> Board
nextBoard _       bd@(Board [] g) = bd
nextBoard (Rule r d) (Board as g) = case as of
    []      -> Board as g
    (an:[]) -> step an (Board [] g)
    (an:_)  -> foldr step (Board [] g) (reverse as)
  where slide a@(Ant c f)   = move . rotate a . last . M.findWithDefault d c
        alter a@(Ant c f)   =  M.alter (Just . r . fromMaybe d) c
        step  a (Board x b) = Board (slide a b : x) $ alter a b

advanceBoard :: Int -> Rule -> Board -> Board
advanceBoard n r b
    | n == 0    = b
    | otherwise = advanceBoard (n-1) r (nextBoard r b)

renderBoard :: (Int, Int) -> Board -> String
renderBoard (x, y) b = concatMap ((++ "\n") . row) (reverse [(-1)*y..y])
  where row r = map (flip (curry render) r) [(-1)*x..x]
        render (u, v)
            | (u, v) `elem` map loc (ants b)                     = 'A'
            | length (M.findWithDefault [] (u, v) (board b)) > 1 = 'x'
            | otherwise                                          = ' '

-- Rulesets

sequenceRule :: [Rotation] -> Rule
sequenceRule []     = error "Invalid rule."
sequenceRule (r:rs) = Rule next [r]
  where next fs
            | length fs == length (r:rs) = [r]
            | otherwise                  = take (length fs + 1) (r:rs)

basicRL = sequenceRule [R,L]

-- Example

main = do
    let b = defaultBoard defaultAnt
    let r = advanceBoard 1000000 (sequenceRule $ map (read . (:"")) "RLR") b
    putStrLn $ renderBoard (80, 80) r
