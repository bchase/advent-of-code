module Year2017.Day03 ( day03 ) where

import           Data.List     (find)
import           Data.Maybe    (fromJust)
import           Control.Monad (join)

import           Types


type Coords = (Int, Int)
type Address = Int
type Val = Int
type Grid = ([Cell], Size)
type Cell = (Coords, Address, Val)
type Size = Int


day03 :: Mode -> AB -> String -> IO [String]
day03 mode ab input = do
  case mode of
    Run -> error "NOTIMPL"
      --
      -- let size = ceiling . sqrt $ (368078 :: Double)
      --     size = (+1) . ceiling . sqrt $ (fromIntegral 368078 :: Double)
      --     grid = buildGrid size
      --     grid' = populateGrid grid
      --     (Just cell) = find (valGT target) grid'
      -- return . map show $ [ b ]
    Test -> do
      let addr = read input
          size = (+2) . ceiling . sqrt $ (fromIntegral addr :: Double)
          grid = buildGrid size
      case ab of
        A -> return . map show $ [ fromJust $ getDistanceBetween 1 addr grid ]
        B -> return . map show $ [ fromJust $ getCellValInPopulatedGrid addr grid  ]


getDistanceBetween :: Address -> Address -> Grid -> Maybe Int
getDistanceBetween a1 a2 g = do
  (oneX,oneY) <- getCoordsForAddress a1 g
  (locX,locY) <- getCoordsForAddress a2 g
  return $ abs (oneX - locX) + (abs (oneY - locY))

getCellValInPopulatedGrid :: Address -> Grid -> Maybe Val
getCellValInPopulatedGrid addr grid =
  let (cells,_) = populateGrid grid
   in fmap getVal . find (hasAddr addr) $ cells


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
        addCols =
          if odd s then reverse . map cf . reverse . zip cs
                   else map cf . zip cs
        addRow g' = rf (r,g')
        cf = if odd s then push else unshift
        rf = if odd s then unshift else push
        unshift (a,as) = a:as
        push (a,as) = as ++ [a]
        s  = gridSize g
        sq = flip (^) (2 :: Int)
        ns = [((sq s)+1)..(sq (s+1))]
        cs = (if odd s then reverse else id) . take s $ ns
        rs = drop s $ ns
        r  = if even s then rs else reverse rs

getCoordsForAddress :: Address -> Grid -> Maybe Coords
getCoordsForAddress addr (grid,_)=
  fmap getCoords . find (hasAddr addr) $ grid

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
      where
        updateCell :: Cell -> [Cell] -> [Cell]
        updateCell c@(_,a,_) cs =
          c:(filter (not . hasAddr a) $ cs)

        neighbors :: Coords -> [Cell] -> [Cell]
        neighbors c@(x,y) cs =
          let (xs,ys) = ( [x-1,x,x+1], [y-1,y,y+1] )
              xys = [(x',y') | x' <- xs, y' <- ys, (x',y') /= c]
           in filter (flip elem xys . getCoords) cs


-- printCells :: [Cell] -> IO ()
-- printCells = mapM_ id . snd . foldl f (0, []) . reverse . sortBy xy
--   where
--     xy :: Cell -> Cell -> Ordering
--     xy ((x1,y1),_,_) ((x2,y2),_,_) = compare (y1,x1) (y2,x2)
--
--     f :: (Int, [IO ()]) -> Cell -> (Int, [IO ()])
--     f (y,ps) ((_,y'),_,v) =
--       -- let p = if y == y' then pr else pn
--       let p = if y' == y then pr else pn
--        in (y',(p v):ps)
--     pr :: Int -> IO ()
--     pr = printf "%03d "
--     pn :: Int -> IO ()
--     pn i = pr i >> putStrLn "\n"
--
-- printGrid :: Grid -> IO ()
-- printGrid = mapM_ id . snd . foldl f (0, []) . reverse . sortBy xy . join
--   where
--     xy :: Cell -> Cell -> Ordering
--     xy ((x1,y1),_,_) ((x2,y2),_,_) = compare (y1,x1) (y2,x2)
--
--     f :: (Int, [IO ()]) -> Cell -> (Int, [IO ()])
--     f (y,ps) ((_,y'),a,_) =
--       -- let p = if y == y' then pr else pn
--       let p = if y' == y then pr else pn
--        in (y',(p a):ps)
--     pr :: Int -> IO ()
--     pr = printf "%02d "
--     pn :: Int -> IO ()
--     pn i = pr i >> putStrLn "\n"
