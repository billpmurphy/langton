import Control.Applicative ((<$>))
import Control.Monad (foldM)
import qualified Data.HashTable.IO as H
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

type HashTable k v = H.BasicHashTable k v
type Grid = HashTable Point [Rotation]

data Board = Board { ants :: [Ant], board :: Grid }

defaultBoard :: [Ant] -> IO Board
defaultBoard as = Board as <$> H.new

data Rule = Rule { rule :: [Rotation] -> [Rotation], def :: [Rotation] }

nextBoard :: Rule -> Board -> IO Board
nextBoard _       bd@(Board [] g) = return bd
nextBoard (Rule r d) (Board as g) = case as of
    []      -> return $ Board as g
    (an:[]) -> step (Board [] g) an
    (an:_)  -> foldM step (Board [] g) (reverse as)
  where step bd@(Board x g) a@(Ant c f) = do
            look <- fromMaybe d <$> H.lookup g c
            let newant = move . rotate a $ last look
            H.insert g c $ r look
            return $ Board (newant : x) (board bd)

advanceBoard :: Int -> Rule -> Board -> IO Board
advanceBoard n r b
    | n == 0    = return b
    | otherwise = nextBoard r b >>= advanceBoard (n-1) r

renderBoard :: (Int, Int) -> Board -> IO String
renderBoard (x, y) b = concat <$> mapM (fmap (++"\n") . renderRow) [(-1)*y..y]
  where renderRow r = mapM (\w -> renderCell (w, r)) [(-1)*x..x]
        renderCell coords
            | coords `elem` map loc (ants b) = return 'A'
            | otherwise = do
                v <- H.lookup (board b) coords
                return (if length (fromMaybe [] v) > 1 then 'x' else ' ')

-- Rules

sequenceRule :: [Rotation] -> Rule
sequenceRule []     = error "Invalid rule."
sequenceRule (r:rs) = Rule next [r]
  where next fs
            | length fs == length (r:rs) = [r]
            | otherwise                  = take (length fs + 1) (r:rs)

basicRL = sequenceRule [R,L]

stringToRule :: String -> Rule
stringToRule = sequenceRule . map (read . (:"")) . filter (`elem` "BLR")

-- Example

main = do
    b <- defaultBoard defaultAnt
    r <- advanceBoard (10^5) (stringToRule "LRRRRRLLR") b
    s <- renderBoard (90, 90) r
    putStrLn s
