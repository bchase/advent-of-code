module Year2017.Day03 ( day03 ) where

import           Data.List     (find, sort)
import           Data.Maybe    (fromJust)
import           Control.Monad (join)

import           Types


day03 :: Mode -> AB -> String -> IO [String]
day03 mode ab input = return [ show result ]
  where
    result =
      case mode of
        Run -> do
          let addr = read input
              sizeA = ceiling . sqrt $ (fromIntegral addr :: Double)
              sizeB = sizeA `div` 6
          case ab of
            A -> fromJust . getDistanceBetween 1 addr . buildGrid $ sizeA
            B -> fromJust . find (flip (>=) addr) . sort . map getVal . fst . populateGrid . buildGrid $ sizeB
        Test -> do
          let addr = read input
              size = (+2) . ceiling . sqrt $ (fromIntegral addr :: Double)
              grid = buildGrid size
          case ab of
            A -> fromJust $ getDistanceBetween 1 addr grid
            B -> fromJust $ fmap getVal . find (hasAddr addr) . fst . populateGrid $ grid


type Grid = ([Cell], Size)
type Cell = (Coords, Address, Val)

type Coords = (Int, Int)
type Address = Int
type Val = Int
type Size = Int


getDistanceBetween :: Address -> Address -> Grid -> Maybe Int
getDistanceBetween a1 a2 g = do
  (oneX,oneY) <- getCoordsForAddress a1 g
  (locX,locY) <- getCoordsForAddress a2 g
  return $ abs (oneX - locX) + (abs (oneY - locY))


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
        addrs = [((sq size')+1)..(sq (size'+1))]
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
      let (Just (xy,a,_)) =find (((==) addr) . getAddr) cells
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
