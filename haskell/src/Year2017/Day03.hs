module Year2017.Day03 ( day03 ) where

import           Data.List     (find, sort)
import           Data.Maybe    (fromJust)
import           Control.Monad (join)

import           Types


type Grid = ([Cell], Size)
type Cell = (Coords, Address, Val)

type Coords = (Int, Int)
type Address = Int
type Val = Int
type Size = Int


day03 :: Mode -> AB -> String -> IO [String]
day03 mode ab input = return [ show . fromJust $ result ]
  where
    addr = read input
    size = ceiling . sqrt $ (fromIntegral addr :: Double)

    result =
      case (mode, ab) of
        (Test, A) -> getDistanceBetween 1 addr . buildGrid $ size
        (Run,  A) -> getDistanceBetween 1 addr . buildGrid $ size
        (Test, B) -> fmap getVal . findAddrInPopulatedGrid addr . buildGrid $ size+2
        (Run,  B) -> findFirstGreaterValInPopulatedGrid addr . buildGrid $ size `div` 6


getDistanceBetween :: Address -> Address -> Grid -> Maybe Int
getDistanceBetween a1 a2 g = do
  (oneX,oneY) <- getCoordsForAddress a1 g
  (locX,locY) <- getCoordsForAddress a2 g
  return $ abs (oneX - locX) + (abs (oneY - locY))

findAddrInPopulatedGrid :: Address -> Grid -> Maybe Cell
findAddrInPopulatedGrid addr =
  find (hasAddr addr) . fst . populateGrid

findFirstGreaterValInPopulatedGrid :: Val -> Grid -> Maybe Val
findFirstGreaterValInPopulatedGrid val =
  find (flip (>) val) . sort . map getVal . fst . populateGrid


getCoords :: Cell -> Coords
getCoords (xy,_,_) = xy
getAddr :: Cell -> Address
getAddr (_,a,_) = a
getVal :: Cell -> Val
getVal (_,_,v) = v
hasAddr :: Address -> Cell -> Bool
hasAddr addr = ((==) addr) . getAddr

buildGrid :: Int -> Grid
buildGrid size =
  (grid . until (\g -> gridSize g >= size) incPreGrid $ [[1]], size)
  where
    grid :: [[Address]] -> [Cell]
    grid = join . map (\(y,r) -> map (\(x,a) -> ((x,y),a,0)) . zip [0..] $ r) . zip [0..]

    gridSize :: [[a]] -> Int
    gridSize (row:_) = length row
    gridSize _ = 0

    incPreGrid :: [[Address]] -> [[Address]]
    incPreGrid g = addRow . addCols $ g
      where
        size' = gridSize g
        isOdd = odd size'

        addRow rows = (if isOdd then unshift else push) (newRow,rows)

        addCols = mapColValToRow (if isOdd then push else unshift) . zip newCol
        mapColValToRow f = if isOdd then reverse . map f . reverse else map f

        (newCol, newRow) = (order . take size' $ addrs, order . drop size' $ addrs)
        addrs = [ sq size' + 1 .. sq (size'+1) ]
        order = if isOdd then reverse else id

        sq = flip (^) (2 :: Int)
        unshift (a,as) = a:as
        push (a,as) = as ++ [a]

getCoordsForAddress :: Address -> Grid -> Maybe Coords
getCoordsForAddress addr = fmap getCoords . find (hasAddr addr) . fst

populateGrid :: Grid -> Grid
populateGrid grid@(_,size) =
  let addrs = reverse [1 .. size]
   in (foldr populate (fst grid) $ addrs, size)
  where
    populate :: Address -> [Cell] -> [Cell]
    populate addr cells =
      let (Just (xy,a,_)) = find (((==) addr) . getAddr) cells
          neighborVals = map getVal . neighbors xy $ cells
          cell' = (xy, a, if a == 1 then 1 else sum neighborVals)
       in updateCell cell' cells

    updateCell :: Cell -> [Cell] -> [Cell]
    updateCell c@(_,a,_) cs =
      c:(filter (not . hasAddr a) $ cs)

    neighbors :: Coords -> [Cell] -> [Cell]
    neighbors c@(x,y) cs =
      let (xs,ys) = ( [x-1,x,x+1], [y-1,y,y+1] )
          xys = [(x',y') | x' <- xs, y' <- ys, (x',y') /= c]
       in filter (flip elem xys . getCoords) cs
